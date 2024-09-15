use std::rc::Rc;
use linked_hash_map::LinkedHashMap;
use crate::metadata::{translate_closure_struct, DYNARRAY_LEN_FIELD, DYNARRAY_PTR_FIELD};
use crate::metadata::translate_iface;
use crate::metadata::translate_name;
use crate::metadata::translate_struct_def;
use crate::metadata::translate_variant_def;
use crate::metadata::ClosureIdentity;
use crate::metadata::ClosureInstance;
use crate::metadata::FunctionID;
use crate::metadata::FunctionSig;
use crate::metadata::RuntimeType;
use crate::metadata::TypeDefID;
use crate::metadata::VirtualTypeID;
use crate::metadata::STRING_CHARS_FIELD;
use crate::metadata::STRING_ID;
use crate::metadata::Metadata;
use crate::{build_closure_function_def, build_static_closure_impl, translate_func_params, RETURN_REF};
use crate::build_func_static_closure_def;
use crate::FunctionDef;
use crate::GlobalRef;
use crate::Instruction;
use crate::LocalID;
use crate::Ref;
use crate::StaticClosure;
use crate::StaticClosureID;
use crate::Type;
use crate::Value;
use crate::build_func_def;
use crate::typ;
use crate::ExternalFunctionRef;
use crate::Function;
use crate::FunctionInstance;
use crate::IROptions;
use crate::Module;
use common::span::Span;
use common::span::Spanned;
use frontend::ast::{IdentPath, StructKind};
use frontend::Ident;
use frontend::typecheck::ast::specialize_func_decl;
use frontend::typecheck::builtin_string_name;
use frontend::typecheck::TypeList;
use frontend::typecheck::layout::StructLayout;
use frontend::typecheck::layout::StructLayoutMember;
use crate::builder::Builder;
use crate::stmt::translate_stmt;

#[derive(Debug)]
pub struct ModuleBuilder {
    src_metadata: typ::Context,
    
    opts: IROptions,
    
    type_cache: LinkedHashMap<typ::Type, Type>,

    module: Module,
}

impl ModuleBuilder {
    pub fn new(src_metadata: typ::Context, metadata: Metadata, opts: IROptions) -> Self {
        ModuleBuilder {
            opts,
            src_metadata,
            
            type_cache: LinkedHashMap::new(),

            module: Module::new(metadata),
        }
    }

    pub fn module_span(&self) -> &Span {
        self.src_metadata.module_span()
    }

    pub fn build(mut self) -> Module {
        self.gen_static_closure_init();

        self.gen_iface_impls();
        for (elem_ty, struct_id) in self.metadata().dyn_array_structs().clone() {
            gen_dyn_array_rc_boilerplate(&mut self, &elem_ty, struct_id);
            gen_dyn_array_funcs(&mut self, &elem_ty, struct_id);
        }
        for class_ty in self.class_types().cloned().collect::<Vec<_>>() {
            gen_class_rc_boilerplate(&mut self, &class_ty);
        }
        for closure_id in self.module.closure_types().collect::<Vec<_>>() {
            self.runtime_type(&Type::Struct(closure_id));
        }

        self.module.metadata.sort_type_defs_by_deps();

        self.module
    }
    
    pub fn opts(&self) -> &IROptions {
        &self.opts
    }

    pub fn metadata(&self) -> &Metadata {
        &self.module.metadata
    }    
    
    pub fn metadata_mut(&mut self) -> &mut Metadata {
        &mut self.module.metadata
    }

    pub fn translate_unit(&mut self, unit: &typ::ast::Unit) {
        let mut init_builder = Builder::new(self);
        for stmt in &unit.init {
            translate_stmt(stmt, &mut init_builder);
        }
        let unit_init = init_builder.finish();

        self.module.init.extend(unit_init);
        
        if self.module.span.is_none() {
            
        }
    }

    pub(crate) fn instantiate_func(&mut self, key: FunctionDefKey) -> FunctionInstance {
        if let Some(cached_func) = self.module.translated_funcs.get(&key) {
            return cached_func.clone();
        }

        match &key.decl_key {
            FunctionDeclKey::Function { name } => {
                match self.src_metadata.find_def(&name).cloned() {
                    Some(typ::Def::Function(func_def)) => {
                        self.instantiate_func_def(&func_def, key)
                    },

                    Some(typ::Def::External(extern_decl)) => {
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
        func_def: &typ::ast::FunctionDef,
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

        let sig = typ::FunctionSig::of_decl(&specialized_decl);

        let id = self
            .module
            .metadata
            .declare_func(&func_def.decl, key.type_args.as_ref());

        // cache the function before translating the instantiation, because
        // it may recurse and instantiate itself in its own body
        let cached_func = FunctionInstance {
            id,
            sig: Rc::new(sig),
        };

        let type_args = key.type_args.clone();

        self.module.translated_funcs.insert(key, cached_func.clone());

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

        self.module.functions.insert(id, Function::Local(ir_func));

        cached_func
    }

    fn instantiate_func_external(
        &mut self,
        extern_decl: &typ::ast::FunctionDecl,
        key: FunctionDefKey,
    ) -> FunctionInstance {
        assert!(
            key.type_args.is_none(),
            "external function must not be generic"
        );

        let id = self.module.metadata.declare_func(&extern_decl, None);
        let sig = typ::FunctionSig::of_decl(&extern_decl);

        let return_ty = self.translate_type(&sig.return_ty, None);
        let param_tys = translate_func_params(&sig, self);

        let cached_func = FunctionInstance {
            id,
            sig: Rc::new(sig),
        };

        let extern_src = extern_decl
            .external_src()
            .expect("function with external def must have an extern src");

        self.module.functions.insert(
            id,
            Function::External(ExternalFunctionRef {
                src: extern_src.to_string(),
                symbol: extern_decl.ident.last().to_string(),

                sig: FunctionSig { return_ty, param_tys },

                src_span: extern_decl.span().clone(),
            }),
        );

        self.module.translated_funcs.insert(key, cached_func.clone());

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

        let iface_id = match self.module.metadata.find_iface_decl(&iface_def.name.qualified) {
            Some(iface_id) => iface_id,
            None => {
                let mut builder = Builder::new(self);
                let iface_meta = builder.translate_iface(&iface_def);
                self.module.metadata.define_iface(iface_meta)
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
            .module
            .metadata
            .declare_func(&specialized_decl, type_args.as_ref());

        let self_ty = self.module.metadata.find_type(&method_key.self_ty);

        self.module.metadata
            .impl_method(iface_id, self_ty, method_name, id);

        // cache the function before translating the instantiation, because
        // it may recurse and instantiate itself in its own body
        let cached_func = FunctionInstance {
            id,
            sig: Rc::new(typ::FunctionSig::of_decl(&specialized_decl)),
        };

        let key = FunctionDefKey {
            decl_key: FunctionDeclKey::Method(method_key),
            type_args: None,
        };
        self.module.translated_funcs.insert(key, cached_func.clone());

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
        self.module.functions.insert(id, Function::Local(ir_func));

        cached_func
    }

    // statically reference a method and get a function ID. interface methods are all translated
    // at the end of compilation for a module anyway, but for methods that are referenced statically
    // this call reserves us a function ID
    pub fn translate_method_impl(
        &mut self,
        iface: IdentPath,
        method: Ident,
        mut self_ty: typ::Type,
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
        type_args: Option<TypeList>,
    ) -> FunctionInstance {
        let key = FunctionDefKey {
            type_args,
            decl_key: FunctionDeclKey::Function { name: func_name },
        };

        self.instantiate_func(key)
    }

    pub fn insert_func(&mut self, id: FunctionID, function: Function) {
        assert!(
            self.module.metadata.get_function(id).is_some(),
            "function passed to insert_func must have been previously registered in metadata"
        );

        self.module.functions.insert(id, function);
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
        src_ty: &typ::Type,
        type_args: &typ::TypeList,
    ) -> typ::Type {
        src_ty.specialize_generic(type_args, &self.src_metadata).unwrap().into_owned()
    }

    pub fn translate_type(
        &mut self,
        src_ty: &typ::Type,
        type_args: Option<&typ::TypeList>,
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
            typ::Type::Variant(variant) => {
                let variant_def = self.src_metadata.instantiate_variant_def(variant).unwrap();

                let id = self.module.metadata.reserve_new_struct();
                let ty = Type::Variant(id);
                self.type_cache.insert(src_ty.clone(), ty.clone());

                let name_path = translate_name(&variant, type_args, self);
                self.module.metadata.declare_struct(id, &name_path);

                let variant_meta = translate_variant_def(&variant_def, type_args, self);

                self.module.metadata.define_variant(id, variant_meta);
                ty
            },

            typ::Type::Record(name) | typ::Type::Class(name) => {
                // handle builtin types
                if **name == builtin_string_name() {
                    let string_ty = Type::RcPointer(VirtualTypeID::Class(STRING_ID));

                    self.type_cache.insert(src_ty, string_ty.clone());

                    return string_ty;
                }

                let def = self.src_metadata.instantiate_struct_def(name).unwrap();

                let id = self.module.metadata.reserve_new_struct();

                let ty = match def.kind {
                    StructKind::Class => {
                        Type::RcPointer(VirtualTypeID::Class(id))
                    },
                    StructKind::Record | StructKind::PackedRecord => {
                        Type::Struct(id)
                    },
                };

                self.type_cache.insert(src_ty.clone(), ty.clone());

                let name_path = translate_name(&name, type_args, self);

                self.module.metadata.declare_struct(id, &name_path);

                let struct_meta = translate_struct_def(&def, type_args, self);
                self.module.metadata.define_struct(id, struct_meta);

                ty
            },

            typ::Type::Interface(iface_def) => {
                let iface_def = self.src_metadata.find_iface_def(iface_def).unwrap();

                let iface_name = translate_name(&iface_def.name, type_args, self);
                let id = self.module.metadata.declare_iface(&iface_name);
                let ty = Type::RcPointer(VirtualTypeID::Interface(id));

                self.type_cache.insert(src_ty, ty.clone());

                let iface_meta = translate_iface(&iface_def, type_args, self);
                let def_id = self.module.metadata.define_iface(iface_meta);
                assert_eq!(def_id, id);

                ty
            },

            typ::Type::DynArray { element } => {
                let id = self.translate_dyn_array_struct(&element, type_args);

                let ty = Type::RcPointer(VirtualTypeID::Class(id));
                self.type_cache.insert(src_ty, ty.clone());

                ty
            },

            typ::Type::Function(func_sig) => {
                if let Some(id) = self.module.metadata.find_func_ty(&func_sig) {
                    return Type::Function(id);
                }

                let ir_sig = FunctionSig::translate(&func_sig, type_args, self);
                let func_ty_id = self.module.metadata.define_func_ty((**func_sig).clone(), ir_sig);

                let ty = Type::RcPointer(VirtualTypeID::Closure(func_ty_id));

                self.type_cache.insert(src_ty, ty.clone());

                ty
            },

            real_ty => {
                // nothing to be instantiated
                let ty = self.module.metadata.find_type(real_ty);
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
        if let Some(boilerplate) = self.module.metadata.get_runtime_type(ty) {
            return boilerplate.clone();
        }

        // declare new func IDs then define them here
        let funcs = self.module.metadata.declare_runtime_type(ty);

        // special handling for System.String, which has magic cleanup behaviour for its
        // allocated memory

        let release_body = {
            let mut release_builder = Builder::new(self);
            release_builder.bind_param(LocalID(0), ty.clone().ptr(), "target", true);
            let target_ref = Ref::Local(LocalID(0)).to_deref();

            match ty {
                Type::Struct(STRING_ID) => {
                    let chars_field_ref = release_builder.local_temp(Type::U8.ptr().ptr());
                    release_builder.field(chars_field_ref.clone(), target_ref, ty.clone(), STRING_CHARS_FIELD);
                    release_builder.free_mem(chars_field_ref.to_deref());
                },
                _ => {
                    release_builder.visit_deep(target_ref, ty, |builder, el_ty, el_ref| {
                        builder.release(el_ref, el_ty)
                    });
                }
            }
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
                debug_name: format!("<generated releaser for {}>", self.module.metadata.pretty_ty_name(ty)),
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
                debug_name: format!("generated RC retain func for {}", self.module.metadata.pretty_ty_name(ty)),
                src_span: self.module_span().clone(),
            }),
        );

        funcs
    }

    pub fn translate_dyn_array_struct(
        &mut self,
        element_ty: &typ::Type,
        type_args: Option<&TypeList>,
    ) -> TypeDefID {
        let element_ty = self.translate_type(element_ty, type_args);

        match self.module.metadata.find_dyn_array_struct(&element_ty) {
            Some(id) => id,
            None => self.module.metadata.define_dyn_array_struct(element_ty),
        }
    }

    pub fn aligned_struct_members<'a>(&self, struct_def: &'a typ::ast::StructDef) -> Vec<StructLayoutMember<'a>> {
        let layout = match struct_def.kind {
            StructKind::Class | StructKind::Record => StructLayout::Auto,
            StructKind::PackedRecord => StructLayout::Packed,
        };

        layout.members_of(&struct_def, &self.src_metadata).unwrap()
    }

    fn class_types(&self) -> impl Iterator<Item = &Type> {
        self.type_cache.iter().filter_map(|(src_ty, ir_ty)| {
            if src_ty.as_class().is_ok() {
                Some(ir_ty)
            } else {
                None
            }
        })
    }

    pub fn translate_func_ty(
        &mut self,
        func_sig: &typ::FunctionSig,
        type_args: Option<&typ::TypeList>,
    ) -> TypeDefID {
        let func_ty_id = match self.module.metadata.find_func_ty(&func_sig) {
            Some(id) => id,
            None => {
                let ir_sig = FunctionSig::translate(func_sig, type_args, self);
                self.module.metadata.define_func_ty(func_sig.clone(), ir_sig)
            },
        };

        func_ty_id
    }

    pub fn build_closure_instance(
        &mut self,
        func: &typ::ast::AnonymousFunctionDef,
        type_args: Option<typ::TypeList>,
    ) -> ClosureInstance {
        let id = self.module.metadata.insert_func(None);

        // this is the signature of the *function type* of the closure, not the signature of
        // the real method implementing the closure, which has an extra type-erased parameter
        // for the closure itself
        let func_ty_sig = typ::FunctionSig::of_anonymous_func(func);

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

        self.module.functions.insert(id, Function::Local(ir_func));

        ClosureInstance {
            func_instance: cached_func,
            func_ty_id,
            closure_id,
        }
    }

    pub fn build_func_static_closure_instance(&mut self, func: &FunctionInstance) -> &StaticClosure {
        if let Some(existing) = self.module.function_static_closures.get(&func.id) {
            return &self.module.static_closures[existing.0];
        }

        // function reference closures can never have a capture list or type args
        let captures = LinkedHashMap::default();
        let type_args = None;

        let func_ty_id = self.translate_func_ty(func.sig.as_ref(), type_args);

        let ir_func = self.module.functions.get(&func.id)
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
        let thunk_id = self.module.metadata.insert_func(None);
        let thunk_def = build_func_static_closure_def(self, func, &ir_func);

        self.module.functions.insert(thunk_id, Function::Local(thunk_def));

        let closure = ClosureInstance {
            closure_id,
            func_ty_id,
            func_instance: FunctionInstance {
                id: thunk_id,
                sig: func.sig.clone(),
            },
        };

        let static_closure_id = self.build_static_closure_instance(closure).id;
        self.module.function_static_closures.insert(func.id, static_closure_id);

        &self.module.static_closures[static_closure_id.0]
    }

    pub fn build_static_closure_instance(&mut self, closure: ClosureInstance) -> &StaticClosure {
        let existing_index = self.module.static_closures.iter()
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
            return &self.module.static_closures[existing_index];
        }

        let id = StaticClosureID(self.module.static_closures.len());
        let instance = build_static_closure_impl(closure, id, self);

        self.module.static_closures.push(instance);

        &self.module.static_closures[self.module.static_closures.len() - 1]
    }

    /// Add static closure init function calls at top of init block
    fn gen_static_closure_init(&mut self) {
        let mut static_closures_init = Vec::new();
        for static_closure in &self.module.static_closures {
            static_closures_init.push(Instruction::Call {
                function: Value::Ref(Ref::Global(GlobalRef::Function(static_closure.init_func))),
                args: Vec::new(),
                out: None,
            });
        }
        static_closures_init.append(&mut self.module.init);
        self.module.init = static_closures_init;
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum FunctionDeclKey {
    Function { name: IdentPath },
    Method(MethodDeclKey),
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct MethodDeclKey {
    pub iface: IdentPath,
    pub self_ty: typ::Type,
    pub method: Ident,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub(crate) struct FunctionDefKey {
    pub decl_key: FunctionDeclKey,
    pub type_args: Option<typ::TypeList>,
}

fn gen_dyn_array_funcs(module: &mut ModuleBuilder, elem_ty: &Type, struct_id: TypeDefID) {
    let mut alloc_builder = Builder::new(module);
    gen_dyn_array_alloc_func(&mut alloc_builder, elem_ty, struct_id);
    let alloc_body = alloc_builder.finish();

    let dyn_array_rtti = module.metadata().get_dynarray_runtime_type(elem_ty)
        .expect("missing dynarray rtti for type");

    module.insert_func(dyn_array_rtti.alloc, Function::Local(FunctionDef {
        debug_name: format!(
            "dynarray alloc function for element type {}",
            module.metadata().pretty_ty_name(elem_ty)
        ),
        sig: FunctionSig {
            param_tys: vec![Type::any(), Type::I32, Type::any(), Type::any()],
            return_ty: Type::Nothing,
        },
        body: alloc_body,
        src_span: module.module_span().clone(),
    }));

    let mut length_builder = Builder::new(module);
    gen_dyn_array_length_func(&mut length_builder, struct_id);
    let length_body = length_builder.finish();

    module.insert_func(dyn_array_rtti.length, Function::Local(FunctionDef {
        debug_name: format!(
            "dynarray length function for element type {}",
            module.metadata().pretty_ty_name(elem_ty)
        ),
        sig: FunctionSig {
            param_tys: vec![Type::any()],
            return_ty: Type::I32,
        },
        body: length_body,
        src_span: module.module_span().clone(),
    }));
}

fn gen_dyn_array_alloc_func(builder: &mut Builder, elem_ty: &Type, struct_id: TypeDefID) {
    let array_ref_ty = Type::RcPointer(VirtualTypeID::Class(struct_id));
    let el_ptr_ty = elem_ty.clone().ptr();

    builder.comment("bind params");
    let arr_arg = LocalID(0);
    let len_arg = LocalID(1);
    let src_arr_arg = LocalID(2);
    let default_val_arg = LocalID(3);
    builder.bind_param(arr_arg, Type::any(), "arr_ptr", false);
    builder.bind_param(len_arg, Type::I32, "len", false);
    builder.bind_param(src_arr_arg, Type::any(), "src_arr_ptr", false);
    builder.bind_param(default_val_arg, Type::Nothing.ptr(), "default_val", false);

    builder.comment("retain the refs to the array params");
    builder.retain(Ref::Local(LocalID(0)), &Type::any());
    builder.retain(Ref::Local(LocalID(2)), &Type::any());

    builder.comment("cast the array params to this array type");
    let arr = builder.local_temp(array_ref_ty.clone());
    let src_arr = builder.local_temp(array_ref_ty.clone());
    builder.cast(arr.clone(), Ref::Local(LocalID(0)), array_ref_ty.clone());
    builder.cast(src_arr.clone(), Ref::Local(LocalID(2)), array_ref_ty.clone());

    let default_el_ptr = builder.local_temp(el_ptr_ty.clone());
    builder.cast(default_el_ptr.clone(), default_val_arg, el_ptr_ty.clone());

    builder.comment("copy_len := copy_from->length");
    let src_len = builder.local_temp(Type::I32);
    builder.field_val(src_len.clone(), src_arr.clone(), array_ref_ty.clone(), DYNARRAY_LEN_FIELD, Type::I32);

    builder.comment("el_len := sizeof(elem_ty)");
    let el_len = builder.local_temp(Type::I32);
    builder.size_of(el_len.clone(), elem_ty.clone());

    builder.comment("data_len := el_len * len");
    let data_len = builder.local_temp(Type::I32);
    builder.mul(data_len.clone(), el_len.clone(), len_arg);

    builder.comment("data = GetMem(data_len) as ^elem_ty");
    let data_mem = builder.local_temp(Type::U8.ptr());
    builder.get_mem(data_len, data_mem.clone());
    let data = builder.local_temp(el_ptr_ty.clone());
    builder.cast(data.clone(), data_mem, el_ptr_ty.clone());

    builder.comment("iteration counter for initializing elements");
    let counter = builder.local_temp(Type::I32);
    builder.mov(counter.clone(), Value::LiteralI32(0));

    builder.comment("loop break flag we use in a couple of places later");
    let done = builder.local_temp(Type::Bool);

    builder.comment("copy elements from copied array");
    builder.scope(|builder| {
        let copy_count = builder.local_temp(Type::I32);
        let copy_count_ok = builder.local_temp(Type::Bool);

        builder.comment("copy_count := src_len");
        builder.mov(copy_count.clone(), src_len.clone());

        builder.comment("if there are more elements in the source than we want, copy `len` elements instead");
        let copy_count_ok_label = builder.alloc_label();

        builder.comment("copy_count_ok := copy_count <= len");
        builder.lte(copy_count_ok.clone(), copy_count.clone(), len_arg);

        builder.comment("if not copy_count_ok then copy_count := len");
        builder.jmp_if(copy_count_ok_label, copy_count_ok);
        builder.mov(copy_count.clone(), len_arg);
        builder.label(copy_count_ok_label);

        builder.comment("for `copy_count` iterations, copy the value at copy_src[counter] to copy_dst[counter]");
        let copy_loop_label = builder.alloc_label();
        let copy_break_label = builder.alloc_label();

        builder.label(copy_loop_label);

        builder.comment("done := counter = copy_count");
        builder.comment("if done then break");
        builder.eq(done.clone(), counter.clone(), copy_count.clone());
        builder.jmp_if(copy_break_label, done.clone());

        builder.scope(|builder| {
            let copy_dst = builder.local_temp(el_ptr_ty.clone());
            let copy_src = builder.local_temp(el_ptr_ty.clone());

            builder.comment("copy_dst := data + counter");
            builder.add(copy_dst.clone(), data.clone(), counter.clone());

            builder.comment("copy_src := src_arr->ptr + counter");
            builder.field_val(copy_src.clone(), src_arr.clone(), array_ref_ty.clone(), DYNARRAY_PTR_FIELD, el_ptr_ty.clone());
            builder.add(copy_src.clone(), copy_src.clone(), counter.clone());

            builder.comment("copy_dst^ := copy_src^");
            builder.mov(copy_dst.clone().to_deref(), copy_src.to_deref());

            builder.retain(copy_dst.to_deref(), elem_ty);
        });

        builder.comment("counter += 1");
        builder.add(counter.clone(), counter.clone(), Value::LiteralI32(1));

        builder.jmp(copy_loop_label);
        builder.label(copy_break_label);
    });

    builder.comment("while counter < len, default init next element");
    let init_break_label = builder.alloc_label();
    let init_loop_label = builder.alloc_label();

    builder.label(init_loop_label);

    builder.comment("done := counter = len");
    builder.comment("if done then break");
    builder.eq(done.clone(), counter.clone(), len_arg);
    builder.jmp_if(init_break_label, done);

    builder.scope(|builder| {
        builder.comment("data[counter] := default_val_ptr^");
        let el_ptr = builder.local_temp(el_ptr_ty.clone());
        builder.add(el_ptr.clone(), data.clone(), counter.clone());
        builder.mov(el_ptr.clone().to_deref(), default_el_ptr.clone().to_deref());

        builder.retain(el_ptr.to_deref(), elem_ty);
    });

    builder.comment("counter += 1");
    builder.add(counter.clone(), counter.clone(), Value::LiteralI32(1));
    builder.jmp(init_loop_label);

    builder.label(init_break_label);

    builder.set_field(arr.clone(), array_ref_ty.clone(), DYNARRAY_LEN_FIELD, Type::I32, len_arg);
    builder.set_field(arr, array_ref_ty, DYNARRAY_PTR_FIELD, el_ptr_ty, data);
}

fn gen_dyn_array_length_func(builder: &mut Builder, struct_id: TypeDefID) {
    let array_ref_ty = Type::RcPointer(VirtualTypeID::Class(struct_id));

    builder.comment("bind and retain params");
    builder.bind_return();
    builder.bind_param(LocalID(1), Type::any(), "arr_ptr", false);
    builder.retain(Ref::Local(LocalID(1)), &Type::any());

    builder.comment("cast pointer down to this array type");
    let arr = builder.local_temp(array_ref_ty.clone());
    builder.cast(arr.clone(), Ref::Local(LocalID(1)), array_ref_ty.clone());

    builder.comment("evaluate length field into return ref");
    builder.field_val(RETURN_REF, arr, array_ref_ty, DYNARRAY_LEN_FIELD, Type::I32);
}

fn gen_dyn_array_rc_boilerplate(module: &mut ModuleBuilder, elem_ty: &Type, struct_id: TypeDefID) {
    let array_ref_ty = Type::RcPointer(VirtualTypeID::Class(struct_id));
    let array_struct_ty = Type::Struct(struct_id);

    let rc_boilerplate = module
        .metadata()
        .get_runtime_type(&array_struct_ty)
        .expect("rtti function ids for dynarray inner struct must exist");

    let mut builder = Builder::new(module);

    builder.comment("%0 is the self arg, the pointer to the inner struct");
    builder.bind_param(LocalID(0), array_struct_ty.clone().ptr(), "self", true);
    let self_arg = Ref::Local(LocalID(0)).to_deref();

    builder.comment("pointer to the length field of the dynarray object");
    let len_field_ptr = builder.local_temp(Type::I32.ptr());

    builder.comment("pointer to the pointer field of the dynarray object");
    let arr_field_ptr = builder.local_temp(elem_ty.clone().ptr().ptr());

    builder.comment("u8 pointer type field to cast the array memory into to call FreeMem");
    let arr_mem_ptr = builder.local_temp(Type::U8.ptr());

    builder.comment("iteration vars");
    let counter = builder.local_temp(Type::I32);
    let has_more = builder.local_temp(Type::Bool);
    let el_ptr = builder.local_temp(elem_ty.clone().ptr());

    let zero_elements = builder.local_temp(Type::Bool);

    builder.comment("jump to loop end if counter == array len");
    let start_loop_label = builder.alloc_label();
    let end_loop_label = builder.alloc_label();

    let after_free = builder.alloc_label();

    builder.field(len_field_ptr.clone(), self_arg.clone(), array_struct_ty.clone(), DYNARRAY_LEN_FIELD);
    builder.field(arr_field_ptr.clone(), self_arg, array_struct_ty.clone(), DYNARRAY_PTR_FIELD);

    builder.comment("release every element");
    builder.mov(counter.clone(), Value::LiteralI32(0));

    builder.label(start_loop_label);

    builder.comment("has_more := counter < array.length");

    builder.lt(
        has_more.clone(),
        counter.clone(),
        len_field_ptr.clone().to_deref(),
    );

    builder.comment("if not has_more then break");
    let at_end = builder.not_to_val(has_more);
    builder.jmp_if(end_loop_label, at_end);

    builder.comment("release arr[counter]");
    builder.add(el_ptr.clone(), arr_field_ptr.clone().to_deref(), counter.clone());
    builder.release(el_ptr.to_deref(), &elem_ty);

    builder.comment("counter := counter + 1");
    builder.add(counter.clone(), counter, Value::LiteralI32(1));

    builder.jmp(start_loop_label);
    builder.label(end_loop_label);

    builder.comment("free the dynamic-allocated buffer - if len > 0");
    builder.eq(zero_elements.clone(), len_field_ptr.clone().to_deref(), Value::LiteralI32(0));
    builder.jmp_if(after_free, zero_elements);

    builder.cast(arr_mem_ptr.clone(), arr_field_ptr.clone().to_deref(), Type::U8.ptr());
    builder.free_mem(arr_mem_ptr);

    builder.append(Instruction::Label(after_free));

    builder.mov(len_field_ptr.to_deref(), Value::LiteralI32(0));
    builder.mov(arr_field_ptr.to_deref(), Value::LiteralNull);

    let releaser_body = builder.finish();

    let array_ref_ty_name = module.metadata().pretty_ty_name(&array_ref_ty).into_owned();

    module.insert_func(
        rc_boilerplate.release,
        Function::Local(FunctionDef {
            debug_name: format!("<generated dynarray releaser for {}>", array_ref_ty_name),
            sig: FunctionSig {
                return_ty: Type::Nothing,
                param_tys: vec![array_struct_ty.clone().ptr()],
            },
            body: releaser_body,
            src_span: module.module_span().clone(),
        }),
    );

    // no custom retain behaviour (dynarrays can't be retained!)
    module.insert_func(
        rc_boilerplate.retain,
        Function::Local(FunctionDef {
            debug_name: format!("<generated empty retainer for {}>", array_ref_ty_name),
            sig: FunctionSig {
                return_ty: Type::Nothing,
                param_tys: vec![array_struct_ty.clone().ptr()],
            },
            body: Vec::new(),
            src_span: module.module_span().clone()
        }),
    );
}

// class types must generate cleanup code for their inner struct which isn't
// explicitly called in IR but must be called dynamically by the target to
// clean up the inner structs of class RC cells.
// for example, a class instance maybe be stored behind an `Any` reference,
// at which point rc instructions must discover the actual class type
// dynamically from the rc cell's class pointer/class ID
fn gen_class_rc_boilerplate(module: &mut ModuleBuilder, class_ty: &Type) {
    let resource_struct = class_ty
        .rc_resource_class_id()
        .and_then(|class_id| class_id.as_class())
        .expect("resource class of translated class type was not a struct");

    module.runtime_type(&Type::Struct(resource_struct));
}
