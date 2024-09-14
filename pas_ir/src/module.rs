use crate::{build_closure_function_def, build_func_static_closure_def, build_func_def, build_static_closure_impl, metadata::*, pas_ty, translate_func_params, translate_stmt, write_instruction_list, Builder, ExternalFunctionRef, FieldID, Function, FunctionDeclKey, FunctionDef, FunctionDefKey, FunctionID, FunctionInstance, IROptions, Instruction, InstructionFormatter, Metadata, MethodDeclKey, StaticClosure, StaticClosureID, Type, TypeDef, TypeDefID, VirtualTypeID, LocalID, Ref};
use linked_hash_map::LinkedHashMap;
use pas_common::span::{Span, Spanned};
use pas_syn::{Ident, IdentPath};
use pas_typecheck::{ast::specialize_func_decl, builtin_string_name, TypeList};
use std::{collections::HashMap, fmt, rc::Rc};
use pas_syn::ast::StructKind;
use pas_typecheck::layout::{StructLayout, StructLayoutMember};

#[derive(Clone, Debug)]
pub struct Module {
    src_metadata: pas_ty::Context,

    type_cache: LinkedHashMap<pas_ty::Type, Type>,

    pub opts: IROptions,

    pub metadata: Metadata,

    pub functions: HashMap<FunctionID, Function>,
    translated_funcs: HashMap<FunctionDefKey, FunctionInstance>,

    static_closures: Vec<StaticClosure>,
    function_static_closures: HashMap<FunctionID, StaticClosureID>,

    pub init: Vec<Instruction>,
}

impl Module {
    pub fn new(src_metadata: pas_ty::Context, metadata: Metadata, opts: IROptions) -> Self {
        let module = Self {
            src_metadata,
            type_cache: Default::default(),

            init: Vec::new(),

            opts,

            functions: HashMap::new(),
            translated_funcs: HashMap::new(),

            static_closures: Vec::new(),
            function_static_closures: HashMap::new(),

            metadata,
        };

        // module.insert_func()

        module
    }

    pub fn module_span(&self) -> &Span {
        self.src_metadata.module_span()
    }

    pub fn translate_unit(&mut self, unit: &pas_ty::ast::Unit) {
        let mut init_builder = Builder::new(self);
        for stmt in &unit.init {
            translate_stmt(stmt, &mut init_builder);
        }
        let unit_init = init_builder.finish();

        self.init.extend(unit_init);
    }

    pub fn class_types(&self) -> impl Iterator<Item = &Type> {
        self.type_cache.iter().filter_map(|(src_ty, ir_ty)| {
            if src_ty.as_class().is_ok() {
                Some(ir_ty)
            } else {
                None
            }
        })
    }

    pub fn closure_types(&self) -> impl Iterator<Item = TypeDefID> + '_ {
        self.metadata.closures().iter().cloned()
    }

    pub fn insert_func(&mut self, id: FunctionID, function: Function) {
        assert!(
            self.metadata.get_function(id).is_some(),
            "function passed to insert_func must have been previously registered in metadata"
        );

        self.functions.insert(id, function);
    }

    pub(crate) fn instantiate_func(&mut self, key: FunctionDefKey) -> FunctionInstance {
        if let Some(cached_func) = self.translated_funcs.get(&key) {
            return cached_func.clone();
        }

        match &key.decl_key {
            FunctionDeclKey::Function { name } => {
                match self.src_metadata.find_def(&name).cloned() {
                    Some(pas_ty::Def::Function(func_def)) => {
                        self.instantiate_func_def(&func_def, key)
                    },

                    Some(pas_ty::Def::External(extern_decl)) => {
                        self.instantiate_func_external(&extern_decl, key)
                    },

                    _ => panic!("missing source def for function {}", name),
                }
            },

            FunctionDeclKey::Method(method_key) => {
                self.instantiate_method(method_key.clone(), None)
            },
        }
    }

    fn instantiate_func_def(
        &mut self,
        func_def: &pas_ty::ast::FunctionDef,
        key: FunctionDefKey,
    ) -> FunctionInstance {
        let specialized_decl = match &key.type_args {
            Some(key_type_args) => specialize_func_decl(
                &func_def.decl,
                key_type_args,
                func_def.span(),
                &self.src_metadata,
            )
            .expect("function specialization must be valid after typechecking"),
            None => func_def.decl.clone(),
        };

        let sig = pas_ty::FunctionSig::of_decl(&specialized_decl);

        let id = self
            .metadata
            .declare_func(&func_def.decl, key.type_args.as_ref());

        // cache the function before translating the instantiation, because
        // it may recurse and instantiate itself in its own body
        let cached_func = FunctionInstance {
            id,
            sig: Rc::new(sig),
        };

        let type_args = key.type_args.clone();

        self.translated_funcs.insert(key, cached_func.clone());

        let debug_name = specialized_decl.to_string();
        let ir_func = build_func_def(
            self,
            &func_def.decl.params,
            func_def.decl.type_params.as_ref(),
            type_args,
            func_def.decl.return_ty.as_ref(),
            &func_def.locals,
            &func_def.body,
            func_def.span.clone(),
            debug_name,
        );

        self.functions.insert(id, Function::Local(ir_func));

        cached_func
    }

    fn instantiate_func_external(
        &mut self,
        extern_decl: &pas_ty::ast::FunctionDecl,
        key: FunctionDefKey,
    ) -> FunctionInstance {
        assert!(
            key.type_args.is_none(),
            "external function must not be generic"
        );

        let id = self.metadata.declare_func(&extern_decl, None);
        let sig = pas_ty::FunctionSig::of_decl(&extern_decl);

        let return_ty = self.translate_type(&sig.return_ty, None);
        let param_tys = translate_func_params(&sig, self);

        let cached_func = FunctionInstance {
            id,
            sig: Rc::new(sig),
        };

        let extern_src = extern_decl
            .external_src()
            .expect("function with external def must have an extern src");

        self.functions.insert(
            id,
            Function::External(ExternalFunctionRef {
                src: extern_src.to_string(),
                symbol: extern_decl.ident.last().to_string(),

                sig: FunctionSig { return_ty, param_tys },

                src_span: extern_decl.span().clone(),
            }),
        );

        self.translated_funcs.insert(key, cached_func.clone());

        cached_func
    }

    fn instantiate_method(
        &mut self,
        method_key: MethodDeclKey,
        type_args: Option<TypeList>,
    ) -> FunctionInstance {        
        if method_key.self_ty.is_generic_param() {
            panic!("instantiate_method: method ({} of {}) must not have a generic self-type ({})", method_key.method, method_key.iface, method_key.self_ty);
        }
        
        let method_name = method_key.method.to_string();

        let iface_def = match self.src_metadata.find_iface_def(&method_key.iface) {
            Ok(def) => def,
            Err(..) => panic!("missing interface def {}", method_key.iface),
        };

        let iface_id = match self.metadata.find_iface_decl(&iface_def.name.qualified) {
            Some(iface_id) => iface_id,
            None => {
                let mut builder = Builder::new(self);
                let iface_meta = builder.translate_iface(&iface_def);
                self.metadata.define_iface(iface_meta)
            },
        };

        let method_def = self
            .src_metadata
            .find_method_impl_def(
                &iface_def.name.qualified,
                &method_key.self_ty,
                &method_key.method,
            )
            .cloned()
            .unwrap_or_else(|| {
                // typechecking should have already ensured any specialization of a method
                // has been defined somewhere
                panic!(
                    "missing method def: {}.{} for {}",
                    iface_def.name.qualified, method_key.method, method_key.self_ty,
                )
            });
        
        // the definition we found should already be correctly specialized - you can't pass
        // type args when calling an interface method, so the only thing that would change the method
        // being generated here is the self-type, which we already specialized
        let specialized_decl = method_def.decl.clone();

        let id = self
            .metadata
            .declare_func(&specialized_decl, type_args.as_ref());

        let self_ty = self.metadata.find_type(&method_key.self_ty);

        self.metadata
            .impl_method(iface_id, self_ty, method_name, id);

        // cache the function before translating the instantiation, because
        // it may recurse and instantiate itself in its own body
        let cached_func = FunctionInstance {
            id,
            sig: Rc::new(pas_ty::FunctionSig::of_decl(&specialized_decl)),
        };

        let key = FunctionDefKey {
            decl_key: FunctionDeclKey::Method(method_key),
            type_args: None,
        };
        self.translated_funcs.insert(key, cached_func.clone());

        let debug_name = specialized_decl.to_string();
        let ir_func = build_func_def(
            self,
            &method_def.decl.params,
            method_def.decl.type_params.as_ref(),
            None, // the definition is already specialized for this specialized call to the interface
            method_def.decl.return_ty.as_ref(),
            &method_def.locals,
            &method_def.body,
            method_def.span().clone(),
            debug_name,
        );
        self.functions.insert(id, Function::Local(ir_func));

        cached_func
    }

    pub fn translate_func_ty(
        &mut self,
        func_sig: &pas_ty::FunctionSig,
        type_args: Option<&pas_ty::TypeList>,
    ) -> TypeDefID {
        let func_ty_id = match self.metadata.find_func_ty(&func_sig) {
            Some(id) => id,
            None => {
                let ir_sig = FunctionSig::translate(func_sig, type_args, self);
                self.metadata.define_func_ty(func_sig.clone(), ir_sig)
            },
        };

        func_ty_id
    }

    pub fn build_closure_instance(
        &mut self,
        func: &pas_ty::ast::AnonymousFunctionDef,
        type_args: Option<pas_ty::TypeList>,
    ) -> ClosureInstance {
        let id = self.metadata.insert_func(None);

        // this is the signature of the *function type* of the closure, not the signature of
        // the real method implementing the closure, which has an extra type-erased parameter
        // for the closure itself
        let func_ty_sig = pas_ty::FunctionSig::of_anonymous_func(func);

        let func_ty_id = self.translate_func_ty(&func_ty_sig, type_args.as_ref());

        let closure_identity = ClosureIdentity {
            virt_func_ty: func_ty_id,
            module: func.span().file.display().to_string(),
            line: func.span().start.line,
            col: func.span().start.col,
        };
        let closure_id = translate_closure_struct(
            closure_identity,
            &func.captures,
            type_args.as_ref(),
            self
        );

        let debug_name = "<anonymous function>".to_string();

        let cached_func = FunctionInstance { id, sig: func_ty_sig };

        let src_span = func.span().clone();
        let ir_func = build_closure_function_def(self, &func, closure_id, src_span, debug_name);

        self.functions.insert(id, Function::Local(ir_func));

        ClosureInstance {
            func_instance: cached_func,
            func_ty_id,
            closure_id,
        }
    }
    
    pub fn build_func_static_closure_instance(&mut self, func: &FunctionInstance) -> &StaticClosure {
        if let Some(existing) = self.function_static_closures.get(&func.id) {
            return &self.static_closures[existing.0];
        }
        
        // function reference closures can never have a capture list or type args
        let captures = LinkedHashMap::default();
        let type_args = None;

        let func_ty_id = self.translate_func_ty(func.sig.as_ref(), type_args);

        let ir_func = self.functions.get(&func.id)
            .expect("function passed to build_function_closure_instance must have been previously translated")
            .clone();

        // the span associated with the static closure should be the function definition
        // itself, not the usage
        let src_span = ir_func.src_span();
        
        let closure_identity = ClosureIdentity {
            virt_func_ty: func_ty_id,
            col: src_span.start.col,
            line: src_span.end.col,
            module: src_span.file.display().to_string(),
        };

        let closure_id = translate_closure_struct(
            closure_identity,
            &captures,
            type_args,
            self
        );

        // build the closure function, which is a thunk that just calls the global function
        let thunk_id = self.metadata.insert_func(None);
        let thunk_def = build_func_static_closure_def(self, func, &ir_func);

        self.functions.insert(thunk_id, Function::Local(thunk_def));
        
        let closure = ClosureInstance {
            closure_id,
            func_ty_id,
            func_instance: FunctionInstance {
                id: thunk_id,
                sig: func.sig.clone(),
            },
        };

        let static_closure_id = self.build_static_closure_instance(closure).id;
        self.function_static_closures.insert(func.id, static_closure_id);

        &self.static_closures[static_closure_id.0]
    }

    pub fn build_static_closure_instance(&mut self, closure: ClosureInstance) -> &StaticClosure {
        let existing_index = self.static_closures.iter()
            .enumerate()
            .filter_map(|(i, static_closure)| {
                if static_closure.closure_id == closure.closure_id {
                    Some(i)
                } else {
                    None
                }
            })
            .next();

        if let Some(existing_index) = existing_index {
            return &self.static_closures[existing_index];
        }
        
        let id = StaticClosureID(self.static_closures.len());
        let instance = build_static_closure_impl(closure, id, self);

        self.static_closures.push(instance);

        &self.static_closures[self.static_closures.len() - 1]
    }

    pub fn static_closures(&self) -> &[StaticClosure] {
        &self.static_closures
    }

    // statically reference a method and get a function ID. interface methods are all translated
    // at the end of compilation for a module anyway, but for methods that are referenced statically
    // this call reserves us a function ID
    pub fn translate_method_impl(
        &mut self,
        iface: IdentPath,
        method: Ident,
        mut self_ty: pas_ty::Type,
        type_args: Option<TypeList>,
    ) -> FunctionInstance {
        // while we can't pass type args to an interface method call, we can call one in a
        // context where the self-type is a generic that needs resolving before codegen
        if let Some(args) = &type_args {
            self_ty = self.specialize_generic_type(&self_ty, args);
        }
        
        let key = FunctionDefKey {
            decl_key: FunctionDeclKey::Method(MethodDeclKey {
                iface,
                method,
                self_ty,
            }),

            // interface method calls can't pass type args
            type_args: None,
        };

        // methods must always be present so make sure they're immediately instantiated
        self.instantiate_func(key)
    }

    pub fn translate_func(
        &mut self,
        func_name: IdentPath,
        type_args: Option<pas_ty::TypeList>,
    ) -> FunctionInstance {
        let key = FunctionDefKey {
            type_args,
            decl_key: FunctionDeclKey::Function { name: func_name },
        };

        self.instantiate_func(key)
    }

    pub fn find_dyn_array_struct(&self, elem_ty: &Type) -> Option<TypeDefID> {
        self.metadata.find_dyn_array_struct(elem_ty)
    }

    // interface methods may not be statically referenced for every type that implements them due to
    // dynamic dispatch, so we need to cover all possible combinations and generate function bodies for
    // every interface method implemented by a class at the end of codegen
    pub fn gen_iface_impls(&mut self) {
        let mut last_instance_count = self.type_cache.len();

        // generating an impl might actually reference new types in the body of the
        // function, so just keep doing this until the type cache is a stable size
        loop {
            for real_ty in self.type_cache.keys().cloned().collect::<Vec<_>>() {
                let ifaces = self.src_metadata.implemented_ifaces(&real_ty);

                for iface in &ifaces {
                    let iface_def = self.src_metadata.find_iface_def(iface).unwrap();

                    for method in &iface_def.methods {
                        let cache_key = FunctionDefKey {
                            decl_key: FunctionDeclKey::Method(MethodDeclKey {
                                self_ty: real_ty.clone(),
                                method: method.ident().clone(),
                                iface: iface.clone(),
                            }),
                            type_args: None,
                        };

                        self.instantiate_func(cache_key);
                    }
                }
            }

            if self.type_cache.len() == last_instance_count {
                break;
            } else {
                last_instance_count = self.type_cache.len();
            }
        }
    }

    pub fn specialize_generic_type(
        &self,
        src_ty: &pas_ty::Type,
        type_args: &pas_ty::TypeList,
    ) -> pas_ty::Type {
        src_ty.specialize_generic(type_args, &self.src_metadata).unwrap().into_owned()
    }

    pub fn translate_type(
        &mut self,
        src_ty: &pas_ty::Type,
        type_args: Option<&pas_ty::TypeList>,
    ) -> Type {
        let src_ty = match type_args {
            Some(current_ty_args) => {
                let src_ty = src_ty.clone().substitute_type_args(current_ty_args);
                src_ty
            },

            None => src_ty.clone(),
        };

        if let Some(cached) = self.type_cache.get(&src_ty) {
            return cached.clone();
        }

        // instantiate types which may contain generic params
        let ty = match &src_ty {
            pas_ty::Type::Variant(variant) => {
                let variant_def = self.src_metadata.instantiate_variant_def(variant).unwrap();

                let id = self.metadata.reserve_new_struct();
                let ty = Type::Variant(id);
                self.type_cache.insert(src_ty.clone(), ty.clone());

                let name_path = translate_name(&variant, type_args, self);
                self.metadata.declare_struct(id, &name_path);

                let variant_meta = translate_variant_def(&variant_def, type_args, self);

                self.metadata.define_variant(id, variant_meta);
                ty
            },

            pas_ty::Type::Record(name) | pas_ty::Type::Class(name) => {
                // handle builtin types
                if **name == builtin_string_name() {
                    let string_ty = Type::RcPointer(VirtualTypeID::Class(STRING_ID));

                    self.type_cache.insert(src_ty, string_ty.clone());

                    return string_ty;
                }

                let def = self.src_metadata.instantiate_struct_def(name).unwrap();

                let id = self.metadata.reserve_new_struct();

                let ty = match def.kind {
                    pas_syn::ast::StructKind::Class => {
                        Type::RcPointer(VirtualTypeID::Class(id))
                    },
                    pas_syn::ast::StructKind::Record | pas_syn::ast::StructKind::PackedRecord => {
                        Type::Struct(id)
                    },
                };

                self.type_cache.insert(src_ty.clone(), ty.clone());

                let name_path = translate_name(&name, type_args, self);

                self.metadata.declare_struct(id, &name_path);

                let struct_meta = translate_struct_def(&def, type_args, self);
                self.metadata.define_struct(id, struct_meta);

                ty
            },

            pas_ty::Type::Interface(iface_def) => {
                let iface_def = self.src_metadata.find_iface_def(iface_def).unwrap();

                let iface_name = translate_name(&iface_def.name, type_args, self);
                let id = self.metadata.declare_iface(&iface_name);
                let ty = Type::RcPointer(VirtualTypeID::Interface(id));

                self.type_cache.insert(src_ty, ty.clone());

                let iface_meta = translate_iface(&iface_def, type_args, self);
                let def_id = self.metadata.define_iface(iface_meta);
                assert_eq!(def_id, id);

                ty
            },

            pas_ty::Type::DynArray { element } => {
                let id = self.translate_dyn_array_struct(&element, type_args);

                let ty = Type::RcPointer(VirtualTypeID::Class(id));
                self.type_cache.insert(src_ty, ty.clone());

                ty
            },

            pas_ty::Type::Function(func_sig) => {
                if let Some(id) = self.metadata.find_func_ty(&func_sig) {
                    return Type::Function(id);
                }

                let ir_sig = FunctionSig::translate(&func_sig, type_args, self);
                let func_ty_id = self.metadata.define_func_ty((**func_sig).clone(), ir_sig);

                let ty = Type::RcPointer(VirtualTypeID::Closure(func_ty_id));

                self.type_cache.insert(src_ty, ty.clone());

                ty
            },

            real_ty => {
                // nothing to be instantiated
                let ty = self.metadata.find_type(real_ty);
                self.type_cache.insert(src_ty, ty.clone());

                ty
            },
        };
        
        // ensure the runtime type info exists for all referenced types
        self.runtime_type(&ty);

        // println!("{} <- {}", src_ty, self.pretty_ty_name(&ty));

        ty
    }

    // get or generate runtime type for a given type, which contains the function IDs etc
    // used for RC operations at runtime
    pub fn runtime_type(&mut self, ty: &Type) -> RuntimeType {
        if let Some(boilerplate) = self.metadata.get_runtime_type(ty) {
            return boilerplate.clone();
        }

        // declare new func IDs then define them here
        let funcs = self.metadata.declare_runtime_type(ty);

        let release_body = {
            let mut release_builder = Builder::new(self);
            release_builder.bind_param(LocalID(0), ty.clone().ptr(), "target", true);
            let target_ref = Ref::Local(LocalID(0)).to_deref();

            release_builder.visit_deep(target_ref, ty, |builder, el_ty, el_ref| {
                builder.release(el_ref, el_ty)
            });
            release_builder.finish()
        };

        self.insert_func(
            funcs.release,
            Function::Local(FunctionDef {
                body: release_body,
                sig: FunctionSig {
                    return_ty: Type::Nothing,
                    param_tys: vec![ty.clone().ptr()],
                },
                debug_name: format!("<generated releaser for {}>", self.metadata.pretty_ty_name(ty)),
                src_span: self.module_span().clone(),
            }),
        );

        let retain_body = {
            let mut retain_builder = Builder::new(self);
            retain_builder.bind_param(LocalID(0), ty.clone().ptr(), "target", true);
            let target_ref = Ref::Local(LocalID(0)).to_deref();
            retain_builder.visit_deep(target_ref, ty, |builder, el_ty, el_ref| {
                builder.retain(el_ref, el_ty)
            });
            retain_builder.finish()
        };

        self.insert_func(
            funcs.retain,
            Function::Local(FunctionDef {
                body: retain_body,
                sig: FunctionSig {
                    return_ty: Type::Nothing,
                    param_tys: vec![ty.clone().ptr()],
                },
                debug_name: format!("generated RC retain func for {}", self.metadata.pretty_ty_name(ty)),
                src_span: self.module_span().clone(),
            }),
        );

        funcs
    }

    pub fn translate_dyn_array_struct(
        &mut self,
        element_ty: &pas_ty::Type,
        type_args: Option<&TypeList>,
    ) -> TypeDefID {
        let element_ty = self.translate_type(element_ty, type_args);

        match self.metadata.find_dyn_array_struct(&element_ty) {
            Some(id) => id,
            None => self.metadata.define_dyn_array_struct(element_ty),
        }
    }

    pub fn aligned_struct_members<'a>(&self, struct_def: &'a pas_ty::ast::StructDef) -> Vec<StructLayoutMember<'a>> {
        let layout = match struct_def.kind {
            StructKind::Class | StructKind::Record => StructLayout::Auto,
            StructKind::PackedRecord => StructLayout::Packed,
        };

        layout.members_of(&struct_def, &self.src_metadata).unwrap()
    }
}

impl fmt::Display for Module {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "* Type Definitions")?;
        let mut defs: Vec<_> = self.metadata.type_defs().collect();
        defs.sort_by_key(|(id, _)| *id);

        for (id, def) in &defs {
            match def {
                TypeDef::Struct(s) => {
                    write!(f, "{}: ", id.0)?;

                    match &s.identity {
                        StructIdentity::Class(name) | StructIdentity::Record(name) => {
                            self.metadata.format_name(name, f)?;
                        },

                        StructIdentity::Closure(identity) => {
                            let func_ty_name = self
                                .metadata
                                .pretty_ty_name(&Type::Function(identity.virt_func_ty));
                            write!(
                                f,
                                "closure of {} @ {}:{}:{}",
                                func_ty_name, identity.module, identity.line, identity.col
                            )?;
                        },

                        StructIdentity::Array(element, dim) => {
                            write!(f, "array[{dim}] of {}", self.metadata.pretty_ty_name(element))?;
                        }

                        StructIdentity::DynArray(element) => {
                            write!(f, "array of {}", self.metadata.pretty_ty_name(element))?;
                        }
                    }

                    writeln!(f)?;

                    let max_field_id = s.fields.keys().max().cloned().unwrap_or(FieldID(0));
                    let fields = (0..=max_field_id.0).filter_map(|id| {
                        let field = s.fields.get(&FieldID(id))?;
                        Some((id, field))
                    });

                    for (id, field) in fields {
                        write!(f, "  {:8>}: ", id)?;
                        self.metadata.format_type(&field.ty, f)?;

                        if let Some(field_name) = &field.name {
                            write!(f, " (`{}`)", field_name)?;
                        }

                        writeln!(f)?;
                    }

                    let ty_as_struct = Type::Struct(*id);
                    let ty_as_class = Type::RcPointer(VirtualTypeID::Class(*id));
                    let mut iface_impls = self.metadata.impls(&ty_as_struct);
                    iface_impls.extend(self.metadata.impls(&ty_as_class));

                    if !iface_impls.is_empty() {
                        writeln!(f, "  Implements:")?;
                        for iface_id in iface_impls {
                            writeln!(f, "    {}", self.metadata.iface_name(iface_id))?;
                        }
                    }
                },

                TypeDef::Variant(v) => {
                    writeln!(f, "{}: {}", id, v.name)?;
                    for (i, case) in v.cases.iter().enumerate() {
                        write!(f, "{:8>} ({})", format!("  .{}", i), case.name,)?;

                        if let Some(ty) = &case.ty {
                            write!(f, ": {}", ty)?;
                        }
                        writeln!(f)?;
                    }
                },

                TypeDef::Function(def) => {
                    write!(f, "{}: {}", id.0, def.to_pretty(&self.metadata))?;
                },
            }

            writeln!(f)?;
        }

        writeln!(f, "* Interfaces: ")?;
        let mut ifaces: Vec<_> = self.metadata.ifaces().collect();
        ifaces.sort_by_key(|(id, _)| *id);

        for (id, iface) in &ifaces {
            writeln!(f, "{}: {}", id, iface.name)?;

            for (i, method) in iface.methods.iter().enumerate() {
                let sig_params: Vec<_> = method
                    .params
                    .iter()
                    .map(|param| self.metadata.pretty_ty_name(param))
                    .collect();
                let return_ty = self.metadata.pretty_ty_name(&method.return_ty);

                let sig = format!("({}) -> {}", sig_params.join(", "), return_ty);

                let index = format!("  .{}", i);
                write!(f, "{:8>} ({}): {}", index, method.name, sig)?;
            }
            writeln!(f)?;
        }
        writeln!(f)?;

        writeln!(f, "* String literals")?;
        for (id, lit) in self.metadata.strings() {
            writeln!(f, "{}: '{}'", id.0, lit)?;
        }
        writeln!(f)?;

        let mut funcs: Vec<_> = self.functions.iter().collect();
        funcs.sort_by_key(|(id, _)| **id);

        writeln!(f, "* Functions")?;
        for (id, func) in funcs {
            write!(f, "{}: ", id.0)?;
            match self.metadata.func_desc(*id) {
                Some(desc_name) => {
                    writeln!(f, "{}", desc_name)?;
                },
                None => {
                    writeln!(f, " /* {} */", func.debug_name())?;
                },
            }

            match func {
                Function::Local(FunctionDef { body, .. }) => {
                    write_instruction_list(f, &self.metadata, body)?;
                },

                Function::External(ExternalFunctionRef { symbol, src, .. }) => {
                    writeln!(f, "<external function '{}' in module '{}'>", symbol, src)?;
                },
            }
            writeln!(f)?;
        }

        writeln!(f, "* Init:")?;
        write_instruction_list(f, &self.metadata, &self.init)?;
        Ok(())
    }
}
