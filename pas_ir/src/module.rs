use crate::{
    jmp_exists, metadata::*, pas_ty, translate_block, translate_stmt, write_instruction_list,
    Builder, ClassID, ExternalFunctionRef, FieldID, Function, FunctionCacheKey, FunctionDeclKey,
    FunctionDef, FunctionID, FunctionInstance, IROptions, Instruction, InstructionFormatter,
    LocalID, Metadata, Ref, StructID, Type, TypeDef, EXIT_LABEL, RETURN_REF,
};
use linked_hash_map::LinkedHashMap;
use pas_common::span::{Span, Spanned};
use pas_syn::ast::FunctionParamMod;
use pas_syn::{ast, IdentPath};
use pas_typecheck::ast::specialize_func_decl;
use pas_typecheck::{builtin_string_name, Specializable, TypeList};
use std::collections::HashMap;
use std::fmt;

#[derive(Clone, Debug)]
pub struct Module {
    src_metadata: pas_ty::Context,

    type_cache: LinkedHashMap<pas_ty::Type, Type>,

    pub opts: IROptions,

    pub metadata: Metadata,

    pub functions: HashMap<FunctionID, Function>,
    translated_funcs: HashMap<FunctionCacheKey, FunctionInstance>,

    pub init: Vec<Instruction>,
}

impl Module {
    pub fn new(src_metadata: pas_ty::Context, metadata: Metadata, opts: IROptions) -> Self {
        Self {
            src_metadata,
            type_cache: Default::default(),

            init: Vec::new(),

            opts,

            functions: HashMap::new(),
            translated_funcs: HashMap::new(),

            metadata,
        }
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

    pub fn insert_func(&mut self, id: FunctionID, function: Function) {
        assert!(
            self.metadata.get_function(id).is_some(),
            "function passed to insert_func must have been previously registered in metadata"
        );

        self.functions.insert(id, function);
    }

    pub(crate) fn instantiate_func(&mut self, key: FunctionCacheKey) -> FunctionInstance {
        if let Some(cached_func) = self.translated_funcs.get(&key) {
            return cached_func.clone();
        }

        match &key.decl_key {
            FunctionDeclKey::Function { name } => {
                match self.src_metadata.find_def(&name).cloned() {
                    Some(pas_ty::Def::Function(func_def)) => {
                        let specialized_decl = match key.type_args.as_ref() {
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
                        let cached_func = FunctionInstance { id, sig };
                        self.translated_funcs
                            .insert(key.clone(), cached_func.clone());

                        let debug_name = specialized_decl.to_string();
                        let ir_func =
                            self.translate_func_def(&func_def, key.type_args.clone(), debug_name);

                        self.functions.insert(id, Function::Local(ir_func));

                        cached_func
                    }

                    Some(pas_ty::Def::External(extern_decl)) => {
                        assert!(
                            key.type_args.is_none(),
                            "external function must not be generic"
                        );

                        let id = self
                            .metadata
                            .declare_func(&extern_decl, key.type_args.as_ref());
                        let sig = pas_ty::FunctionSig::of_decl(&extern_decl);

                        let return_ty = self.translate_type(&sig.return_ty, None);
                        let params: Vec<_> = sig
                            .params
                            .iter()
                            .map(|sig_param| {
                                let param_ty = self.translate_type(&sig_param.ty, None);
                                match sig_param.modifier {
                                    None => param_ty,
                                    Some(FunctionParamMod::Var | FunctionParamMod::Out) => {
                                        param_ty.ptr()
                                    }
                                }
                            })
                            .collect();

                        let cached_func = FunctionInstance { id, sig };

                        let extern_src = extern_decl
                            .external_src()
                            .expect("function with external def must have an extern src");

                        self.functions.insert(
                            id,
                            Function::External(ExternalFunctionRef {
                                src: extern_src.to_string(),
                                symbol: extern_decl.ident.last().to_string(),

                                return_ty,
                                params,

                                src_span: extern_decl.span().clone(),
                            }),
                        );
                        self.translated_funcs.insert(key, cached_func.clone());

                        cached_func
                    }

                    _ => panic!("missing source def for function {}", name),
                }
            }

            FunctionDeclKey::Method {
                iface,
                self_ty,
                method,
            } => {
                let method_name = method.to_string();

                let iface_def = match self.src_metadata.find_iface_def(iface) {
                    Ok(def) => def,
                    Err(..) => panic!("missing interface def {}", iface),
                };

                let iface_id = match self.metadata.find_iface_decl(&iface_def.name.qualified) {
                    Some(iface_id) => iface_id,
                    None => {
                        let mut builder = Builder::new(self);
                        let iface_meta = builder.translate_iface(&iface_def);
                        self.metadata.define_iface(iface_meta)
                    }
                };

                let method_def = self
                    .src_metadata
                    .find_method_impl_def(&iface_def.name.qualified, self_ty, method)
                    .cloned()
                    .unwrap_or_else(|| {
                        panic!(
                            "missing method def: {}.{} for {}",
                            iface_def.name.qualified, method, self_ty,
                        )
                    });

                let specialized_decl = match &key.type_args {
                    Some(key_type_args) => specialize_func_decl(
                        &method_def.decl,
                        &key_type_args,
                        &method_def.span(),
                        &self.src_metadata,
                    )
                    .expect("method specialization failed in codegen"),
                    None => method_def.decl.clone(),
                };

                let id = self
                    .metadata
                    .declare_func(&specialized_decl, key.type_args.as_ref());

                let self_ty = self.metadata.find_type(self_ty);

                self.metadata
                    .impl_method(iface_id, self_ty, method_name, id);

                // cache the function before translating the instantiation, because
                // it may recurse and instantiate itself in its own body
                let cached_func = FunctionInstance {
                    id,
                    sig: pas_ty::FunctionSig::of_decl(&specialized_decl),
                };
                self.translated_funcs
                    .insert(key.clone(), cached_func.clone());

                let debug_name = specialized_decl.to_string();
                let ir_func =
                    self.translate_func_def(&method_def, key.type_args.clone(), debug_name);
                self.functions.insert(id, Function::Local(ir_func));

                cached_func
            }
        }
    }

    // statically reference a method and get a function ID. interface methods are all translated
    // at the end of compilation for a module anyway, but for methods that are referenced statically
    // this call reserves us a function ID
    pub fn translate_method_impl(
        &mut self,
        iface: IdentPath,
        method: pas_syn::Ident,
        self_ty: pas_ty::Type,
    ) -> FunctionInstance {
        let key = FunctionCacheKey {
            decl_key: FunctionDeclKey::Method {
                iface,
                method,
                self_ty,
            },

            // dynamic method calls can't have type args
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
        let key = FunctionCacheKey {
            type_args,
            decl_key: FunctionDeclKey::Function { name: func_name },
        };

        self.instantiate_func(key)
    }

    fn translate_func_def(
        &mut self,
        func: &pas_ty::ast::FunctionDef,
        type_args: Option<pas_ty::TypeList>,
        debug_name: String,
    ) -> FunctionDef {
        let mut body_builder = match type_args {
            Some(type_args) => {
                let type_params = match &func.decl.type_params {
                    Some(params) if params.len() == type_args.len() => params,
                    Some(params) => panic!(
                        "type args in function body don't match params! expected {}, got {}",
                        params, type_args
                    ),
                    _ => panic!(
                        "type args in function body don't match params! expected nothing, got {}",
                        type_args
                    ),
                };

                let mut builder = Builder::new(self).with_type_args(type_args.clone());
                builder.comment("function def body with type args:");
                for (type_param, type_arg) in type_params.iter().zip(type_args.iter()) {
                    builder.comment(&format!("{} = {}", type_param, type_arg));
                }
                builder
            }
            None => Builder::new(self),
        };

        let return_ty = match func.decl.return_ty.as_ref() {
            None | Some(pas_ty::Type::Nothing) => Type::Nothing,
            Some(ty) => {
                let return_ty = body_builder.translate_type(ty);

                // anonymous return binding at %0
                body_builder.comment(&format!(
                    "{} = {} (return slot)",
                    LocalID(0),
                    body_builder.pretty_ty_name(&return_ty),
                ));

                body_builder.bind_return(return_ty.clone());
                return_ty
            }
        };

        let param_id_offset = match &return_ty {
            Type::Nothing => 0,
            _ => {
                assert!(func.decl.return_ty.is_some());
                1
            }
        };

        let mut bound_params = Vec::with_capacity(func.decl.params.len());

        for (i, param) in func.decl.params.iter().enumerate() {
            // if the function returns a value, $0 is the return pointer, and args start at $1
            let id = LocalID(i + param_id_offset);

            let (param_ty, by_ref) = match &param.modifier {
                Some(ast::FunctionParamMod::Var) | Some(ast::FunctionParamMod::Out) => {
                    (body_builder.translate_type(&param.ty).ptr(), true)
                }

                None => (body_builder.translate_type(&param.ty), false),
            };

            body_builder.comment(&format!(
                "{} = {}",
                id,
                body_builder.pretty_ty_name(&param_ty)
            ));
            body_builder.bind_param(id, param_ty.clone(), param.ident.to_string(), by_ref);

            bound_params.push((id, param_ty));
        }

        for (id, ty) in &bound_params {
            body_builder.retain(Ref::Local(*id), ty);
        }

        let body_block_out_ref = match return_ty {
            Type::Nothing => Ref::Discard,
            _ => RETURN_REF.clone(),
        };

        translate_block(&func.body, body_block_out_ref, &mut body_builder);
        let mut body_instructions = body_builder.finish();

        // all functions should finish with the reserved EXIT label but to
        // avoid writing unused label instructions, if none of the other instructions in the body
        // are jumps to the exit label, we can elide it
        if jmp_exists(&body_instructions, EXIT_LABEL) {
            body_instructions.push(Instruction::Label(EXIT_LABEL));
        }

        FunctionDef {
            body: body_instructions,
            params: bound_params.into_iter().map(|(_id, ty)| ty).collect(),
            return_ty,
            debug_name,
            src_span: func.decl.span().clone(),
        }
    }

    pub fn find_dyn_array_struct(&self, elem_ty: &Type) -> Option<StructID> {
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
                        let cache_key = FunctionCacheKey {
                            decl_key: FunctionDeclKey::Method {
                                self_ty: real_ty.clone(),
                                method: method.ident.single().clone(),
                                iface: iface.clone(),
                            },
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

    pub fn translate_type(
        &mut self,
        src_ty: &pas_ty::Type,
        type_args: Option<&pas_ty::TypeList>,
    ) -> Type {
        let src_ty = match type_args {
            Some(current_ty_args) => {
                let src_ty = src_ty.clone().substitute_type_args(&current_ty_args);
                src_ty
            }

            None => src_ty.clone(),
        };

        if let Some(cached) = self.type_cache.get(&src_ty) {
            return cached.clone();
        }

        // instantiate types which may contain generic params
        match &src_ty {
            pas_ty::Type::Variant(variant) => {
                let variant_def = self.src_metadata.instantiate_variant(variant).unwrap();

                let id = self.metadata.reserve_new_struct();
                let ty = Type::Variant(id);
                self.type_cache.insert(src_ty.clone(), ty.clone());
                //                println!("{} <- {}", src_ty, self.pretty_ty_name(&ty));

                let name_path = self.translate_name(&variant, type_args);
                self.metadata.declare_struct(id, &name_path);

                let variant_meta = self.translate_variant(&variant_def, type_args);

                self.metadata.define_variant(id, variant_meta);
                ty
            }

            pas_ty::Type::Record(name) | pas_ty::Type::Class(name) => {
                // handle builtin types
                if **name == builtin_string_name() {
                    let string_ty = Type::RcPointer(Some(ClassID::Class(STRING_ID)));

                    self.type_cache.insert(src_ty, string_ty.clone());

                    return string_ty;
                }

                let class_def = self.src_metadata.instantiate_class(name).unwrap();

                let id = self.metadata.reserve_new_struct();

                let ty = match class_def.kind {
                    pas_syn::ast::ClassKind::Object => Type::RcPointer(Some(ClassID::Class(id))),
                    pas_syn::ast::ClassKind::Record => Type::Struct(id),
                };

                self.type_cache.insert(src_ty.clone(), ty.clone());
                //                println!("{} <- {}", src_ty, ty);

                let name_path = self.translate_name(&name, type_args);

                self.metadata.declare_struct(id, &name_path);

                let struct_meta = self.translate_class(&class_def, type_args);
                self.metadata.define_struct(id, struct_meta);

                ty
            }

            pas_ty::Type::Interface(iface_def) => {
                let iface_def = self.src_metadata.find_iface_def(iface_def).unwrap();

                let iface_name = self.translate_name(&iface_def.name, type_args);
                let id = self.metadata.declare_iface(&iface_name);
                let ty = Type::RcPointer(Some(ClassID::Interface(id)));

                self.type_cache.insert(src_ty, ty.clone());
                //                println!("{} <- {}", src_ty, ty);

                let iface_meta = self.translate_iface(&iface_def, type_args);
                self.metadata.define_iface(iface_meta);

                ty
            }

            pas_ty::Type::DynArray { element } => {
                let id = self.translate_dyn_array_struct(&element, type_args);

                let ty = Type::RcPointer(Some(ClassID::Class(id)));
                self.type_cache.insert(src_ty, ty.clone());
                //                println!("{} <- {}", src_ty, ty);

                ty
            }

            real_ty => {
                // nothing to be instantiated
                let ty = self.metadata.find_type(real_ty);
                self.type_cache.insert(src_ty, ty.clone());
                //                println!("{} <- {}", src_ty, ty);

                ty
            }
        }
    }

    pub fn translate_name(
        &mut self,
        name: &pas_ty::Symbol,
        type_args: Option<&pas_ty::TypeList>,
    ) -> NamePath {
        if name.is_unspecialized_generic() {
            panic!("can't translate unspecialized generic name: {}", name);
        }

        if let Some(name_type_args) = name.type_args.as_ref() {
            if let Some(t) = name_type_args.items.iter().find(|t| t.is_generic_param()) {
                panic!(
                    "can't translate name containing generic parameters (found {}): {}",
                    t, name
                );
            }
        }

        let path_parts = name
            .qualified
            .clone()
            .into_parts()
            .into_iter()
            .map(|ident| ident.to_string());

        let type_args = name.type_args.as_ref().map(|name_type_args_list| {
            name_type_args_list
                .items
                .iter()
                .map(|arg| self.translate_type(arg, type_args))
                .collect()
        });

        NamePath {
            path: pas_syn::Path::from_parts(path_parts),
            type_args,
        }
    }

    pub fn translate_variant(
        &mut self,
        variant_def: &pas_ty::ast::Variant,
        type_args: Option<&pas_ty::TypeList>,
    ) -> Variant {
        let name_path = self.translate_name(&variant_def.name, type_args);

        let mut cases = Vec::new();
        for case in &variant_def.cases {
            let (case_ty, case_rc) = match case.data_ty.as_ref() {
                Some(data_ty) => {
                    let case_ty = self.translate_type(data_ty, type_args);
                    (Some(case_ty), data_ty.is_rc_reference())
                }
                None => (None, false),
            };

            cases.push(VariantCase {
                name: case.ident.to_string(),
                ty: case_ty,
                rc: case_rc,
            });
        }

        Variant {
            name: name_path,
            src_span: Some(variant_def.span().clone()),
            cases,
        }
    }

    pub fn translate_iface(
        &mut self,
        iface_def: &pas_ty::ast::Interface,
        type_args: Option<&pas_ty::TypeList>,
    ) -> Interface {
        let name = self.translate_name(&iface_def.name, type_args);

        // it needs to be declared to reference its own ID in the Self type
        let id = self.metadata.declare_iface(&name);

        let methods: Vec<_> = iface_def
            .methods
            .iter()
            .map(|method| {
                let self_ty = Type::RcPointer(Some(ClassID::Interface(id)));

                Method {
                    name: method.ident.to_string(),
                    return_ty: match &method.return_ty {
                        Some(pas_ty::Type::MethodSelf) => self_ty.clone(),
                        Some(return_ty) => self.translate_type(return_ty, type_args),
                        None => Type::Nothing,
                    },
                    params: method
                        .params
                        .iter()
                        .map(|param| match &param.ty {
                            pas_ty::Type::MethodSelf => self_ty.clone(),
                            param_ty => self.translate_type(param_ty, type_args),
                        })
                        .collect(),
                }
            })
            .collect();

        Interface::new(name, methods)
    }

    pub fn translate_class(
        &mut self,
        class_def: &pas_ty::ast::Class,
        type_args: Option<&pas_ty::TypeList>,
    ) -> Struct {
        let name_path = self.translate_name(&class_def.name, type_args);

        let mut fields = HashMap::new();
        for (id, member) in class_def.members.iter().enumerate() {
            let name = member.ident.to_string();
            let ty = self.translate_type(&member.ty, type_args);
            let rc = member.ty.is_rc_reference();

            fields.insert(FieldID(id), StructFieldDef { name, ty, rc });
        }

        let src_span = class_def.span().clone();

        Struct::new(name_path, Some(src_span)).with_fields(fields)
    }

    pub fn translate_dyn_array_struct(
        &mut self,
        element_ty: &pas_ty::Type,
        type_args: Option<&TypeList>,
    ) -> StructID {
        let element_ty = self.translate_type(element_ty, type_args);

        match self.metadata.find_dyn_array_struct(&element_ty) {
            Some(id) => id,
            None => self.metadata.define_dyn_array_struct(element_ty),
        }
    }
}

impl fmt::Display for Module {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "* Structures")?;
        let mut defs: Vec<_> = self.metadata.type_defs().collect();
        defs.sort_by_key(|(id, _)| *id);

        for (id, def) in &defs {
            match def {
                TypeDef::Struct(s) => {
                    write!(f, "{}: ", id.0)?;
                    self.metadata.format_name(&s.name, f)?;
                    writeln!(f)?;

                    let max_field_id = s.fields.keys().max().cloned().unwrap_or(FieldID(0));
                    let fields = (0..=max_field_id.0).filter_map(|id| {
                        let field = s.fields.get(&FieldID(id))?;
                        Some((id, field))
                    });

                    for (id, field) in fields {
                        write!(f, "{:8>} {}: ", format!("  .{}", id), field.name,)?;
                        self.metadata.format_type(&field.ty, f)?;
                        writeln!(f)?;
                    }

                    let ty_as_struct = Type::Struct(*id);
                    let ty_as_class = Type::RcPointer(Some(ClassID::Class(*id)));
                    let mut iface_impls = self.metadata.impls(&ty_as_struct);
                    iface_impls.extend(self.metadata.impls(&ty_as_class));

                    if !iface_impls.is_empty() {
                        writeln!(f, "interface impls:")?;
                        for iface_id in iface_impls {
                            let iface_ty = Type::RcPointer(Some(ClassID::Interface(iface_id)));
                            write!(f, "  * ")?;
                            self.metadata.format_type(&iface_ty, f)?;
                            writeln!(f)?;
                        }
                    }
                }

                TypeDef::Variant(v) => {
                    writeln!(f, "{}: {}", id, v.name)?;
                    for (i, case) in v.cases.iter().enumerate() {
                        write!(f, "{:8>} ({})", format!("  .{}", i), case.name,)?;

                        if let Some(ty) = &case.ty {
                            write!(f, ": {}", ty)?;
                        }
                        writeln!(f)?;
                    }
                }
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
                }
                None => {
                    writeln!(f, " /* {} */", func.debug_name())?;
                }
            }

            match func {
                Function::Local(FunctionDef { body, .. }) => {
                    write_instruction_list(f, &self.metadata, body)?;
                }

                Function::External(ExternalFunctionRef { symbol, src, .. }) => {
                    writeln!(f, "<external function '{}' in module '{}'>", symbol, src)?;
                }
            }
            writeln!(f)?;
        }

        writeln!(f, "* Init:")?;
        write_instruction_list(f, &self.metadata, &self.init)?;
        Ok(())
    }
}
