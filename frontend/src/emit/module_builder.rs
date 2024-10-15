use crate::ast::FunctionParamMod;
use crate::ast::IdentPath;
use crate::ast::StructKind;
use crate::emit::build_closure_function_def;
use crate::emit::build_func_def;
use crate::emit::build_func_static_closure_def;
use crate::emit::build_static_closure_impl;
use crate::emit::builder::Builder;
use crate::emit::ir;
use crate::emit::metadata::translate_closure_struct;
use crate::emit::metadata::translate_iface;
use crate::emit::metadata::translate_name;
use crate::emit::metadata::translate_sig;
use crate::emit::metadata::translate_struct_def;
use crate::emit::metadata::translate_variant_def;
use crate::emit::metadata::ClosureInstance;
use crate::emit::metadata::NamePathExt;
use crate::emit::stmt::translate_stmt;
use crate::emit::typ;
use crate::emit::FunctionInstance;
use crate::emit::IROptions;
use crate::typ::ast::apply_func_decl_named_ty_args;
use crate::typ::builtin_ident;
use crate::typ::free_mem_sig;
use crate::typ::get_mem_sig;
use crate::typ::layout::StructLayout;
use crate::typ::layout::StructLayoutMember;
use crate::typ::Specializable;
use crate::typ::TypeArgResolver;
use crate::typ::TypeArgsResult;
use crate::typ::TypeParamContainer;
use crate::typ::SYSTEM_UNIT_NAME;
use crate::Ident;
use common::span::Span;
use common::span::Spanned;
use ir_lang::FunctionID;
use linked_hash_map::LinkedHashMap;
use std::borrow::Cow;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

#[derive(Debug)]
pub struct ModuleBuilder {
    src_metadata: typ::Context,
    
    opts: IROptions,
    
    type_cache: LinkedHashMap<typ::Type, ir::Type>,
    
    translated_funcs: HashMap<FunctionDefKey, FunctionInstance>,

    function_types_by_sig: HashMap<typ::FunctionSig, ir::TypeDefID>,

    // looked up on first use
    free_mem_func: Option<FunctionID>,
    get_mem_func: Option<FunctionID>,
    
    module: ir::Module,
}

impl ModuleBuilder {
    pub fn new(src_metadata: typ::Context, metadata: ir::Metadata, opts: IROptions) -> Self {
        ModuleBuilder {
            opts,
            src_metadata,
            
            type_cache: LinkedHashMap::new(),
            
            translated_funcs: HashMap::new(),
            
            function_types_by_sig: HashMap::new(),

            module: ir::Module::new(metadata),
            
            // placeholders
            free_mem_func: None,
            get_mem_func: None,
        }
    }

    pub fn module_span(&self) -> &Span {
        self.src_metadata.module_span()
    }

    pub fn build(mut self) -> ir::Module {
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
            self.runtime_type(&ir::Type::Struct(closure_id));
        }

        self.module.metadata.sort_type_defs_by_deps();

        self.module
    }
    
    pub fn opts(&self) -> &IROptions {
        &self.opts
    }

    pub fn metadata(&self) -> &ir::Metadata {
        &self.module.metadata
    }    
    
    pub fn metadata_mut(&mut self) -> &mut ir::Metadata {
        &mut self.module.metadata
    }

    pub fn translate_unit_init(&mut self, unit: &typ::ast::Unit) {
        if let Some(init_block) = &unit.init {
            let mut init_builder = Builder::new(self);
            
            for stmt in &init_block.body {
                translate_stmt(stmt, &mut init_builder);
            }
            
            let unit_init = init_builder.finish();

            let init_func = ir::FunctionDef {
                body: unit_init,
                sig: ir::FunctionSig {
                    param_tys: Vec::new(),
                    return_ty: ir::Type::Nothing
                },
                src_span: init_block.keyword_span.clone(),
                debug_name: format!("{}.<init>", unit.ident),
            };

            let init_func_id = self.module.metadata.insert_func(None);
            self.module.functions.insert(init_func_id, ir::Function::Local(init_func));

            self.module.init.push(ir::Instruction::Call {
                function: ir::Ref::Global(ir::GlobalRef::Function(init_func_id)).into(),
                args: Vec::new(),
                out: None,
            });
        }
    }

    pub(crate) fn instantiate_func(&mut self, key: &FunctionDefKey) -> FunctionInstance {
        if let Some(cached_func) = self.translated_funcs.get(&key) {
            return cached_func.clone();
        }

        match &key.decl_key {
            FunctionDeclKey::Function { name, sig } => {
                match self.src_metadata.find_func_def(&name, sig.clone()).cloned() {
                    Some(typ::Def::Function(func_def)) => {
                        self.instantiate_func_def(&func_def, key.clone())
                    },

                    Some(typ::Def::External(extern_decl)) => {
                        self.instantiate_func_external(&extern_decl, key.clone())
                    },

                    _ => panic!("missing source def for function {}", name),
                }
            },

            FunctionDeclKey::Method(method_key) => {
                self.instantiate_method(method_key, key.type_args.as_ref())
            },

            FunctionDeclKey::VirtualMethod(virtual_key) => {
                self.instantiate_virtual_method(virtual_key)
            }
        }
    }

    fn instantiate_system_func(&mut self, name: &str, sig: Rc<typ::FunctionSig>) -> FunctionID {
        let ident_path = IdentPath::new(builtin_ident(name), [
            builtin_ident(SYSTEM_UNIT_NAME),
        ]);

        let instance = self.instantiate_func(&mut FunctionDefKey {
            decl_key: FunctionDeclKey::Function {
                name: ident_path,
                sig,
            },
            type_args: None,
        });
        instance.id
    }
    
    pub fn instantiate_get_mem_func(&mut self) -> FunctionID {
        match self.get_mem_func {
            Some(id) => id,
            None => {
                let id = self.instantiate_system_func("GetMem", Rc::new(get_mem_sig()));
                self.get_mem_func = Some(id);
                id
            }
        }
    }

    pub fn instantiate_free_mem_func(&mut self) -> FunctionID {
        match self.free_mem_func {
            Some(id) => id,
            None => {
                let id = self.instantiate_system_func("FreeMem", Rc::new(free_mem_sig()));
                self.free_mem_func = Some(id);
                id
            }
        }
    }

    fn instantiate_func_def(
        &mut self,
        func_def: &typ::ast::FunctionDef,
        key: FunctionDefKey,
    ) -> FunctionInstance {
        // eprintln!("instantiate_func_def: {}", func_def.decl.name);
        
        let generic_ctx = match key.type_args.as_ref() {
            Some(type_args) => {
                let type_params = func_def
                    .decl
                    .type_params
                    .as_ref()
                    .expect("instantiate_func_def: function referenced with type args must have type params");

                typ::GenericContext::new(type_params, type_args)
            }
            None => {
                typ::GenericContext::empty()
            }
        };
        
        let specialized_decl = apply_func_decl_named_ty_args((*func_def.decl).clone(), &generic_ctx, &generic_ctx);

        let sig = specialized_decl.sig();
        let ns = key.decl_key.namespace().into_owned();

        let id = self.declare_func(&specialized_decl, ns, key.type_args.as_ref());

        // cache the function before translating the instantiation, because
        // it may recurse and instantiate itself in its own body
        let cached_func = FunctionInstance {
            id,
            sig: Rc::new(sig),
        };

        self.translated_funcs.insert(key, cached_func.clone());

        let debug_name = specialized_decl.to_string();
        let ir_func = build_func_def(
            self,
            generic_ctx,
            &specialized_decl.params,
            &specialized_decl.return_ty,
            &func_def.locals,
            &func_def.body,
            func_def.span.clone(),
            debug_name,
        );

        self.module.functions.insert(id, ir::Function::Local(ir_func));

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

        let generic_ctx = typ::GenericContext::empty();

        let decl_namespace = key.decl_key.namespace().into_owned();
        let id = self.declare_func(&extern_decl, decl_namespace, None);

        let sig = extern_decl.sig();
        let return_ty = self.translate_type(&sig.return_ty, &generic_ctx);

        let param_tys = sig.params
            .iter()
            .map(|sig_param| {
                let param_ty = self.translate_type(&sig_param.ty, &generic_ctx);
                match sig_param.modifier {
                    None => param_ty,
                    Some(FunctionParamMod::Var | FunctionParamMod::Out) => param_ty.ptr(),
                }
            })
            .collect();

        let cached_func = FunctionInstance {
            id,
            sig: Rc::new(sig),
        };

        let extern_src = extern_decl
            .external_src()
            .expect("function with external def must have an extern src");

        self.module.functions.insert(
            id,
            ir::Function::External(ir::ExternalFunctionRef {
                src: extern_src.to_string(),
                symbol: extern_decl.name.to_string(),

                sig: ir::FunctionSig { return_ty, param_tys },

                src_span: extern_decl.span().clone(),
            }),
        );

        self.translated_funcs.insert(key, cached_func.clone());

        cached_func
    }

    fn instantiate_method(
        &mut self,
        method_key: &MethodDeclKey,
        type_args: Option<&typ::TypeArgList>,
    ) -> FunctionInstance {
        // eprintln!("instantiate_method: {:#?}", method_key);
        
        // the self-type determines which actual class implements the method:
        // * for direct type method calls, both types are the same, so it doesn't matter which
        //   one we instantiate
        // * for virtual calls, instantiate this implementor's impl method
        // * if the self-type is null, this is a static call, so instantiate the method from
        //   the owning type
        let self_ty = match &method_key.self_ty {
            typ::Type::Nothing => method_key.owning_ty.clone(),
            ty => ty.clone(),
        };
        
        let mut generic_ctx = typ::GenericContext::empty();
        
        // if the self type is a parameterized generic, we'll need to instantiate the specialized
        // def here, since only the type's generic version will be in the definition map
        let generic_self_ty = match &self_ty {
            typ::Type::Class(sym) if sym.type_args.is_some() => {
                generic_ctx.push(sym.type_params.as_ref().unwrap(), sym.type_args.as_ref().unwrap());
                typ::Type::class(sym.as_ref().clone().with_ty_args(None))
            }
            typ::Type::Record(sym) if sym.type_args.is_some() => {
                generic_ctx.push(sym.type_params.as_ref().unwrap(), sym.type_args.as_ref().unwrap());
                typ::Type::record(sym.as_ref().clone().with_ty_args(None))
            }
            typ::Type::Variant(sym) if sym.type_args.is_some() => {
                generic_ctx.push(sym.type_params.as_ref().unwrap(), sym.type_args.as_ref().unwrap());
                typ::Type::variant(sym.as_ref().clone().with_ty_args(None))
            }
        
            // nothing to do if the type isn't parameterized
            _ => {
                assert_eq!(
                    TypeArgsResult::NotGeneric, 
                    self_ty.type_args(), "expected non-generic self type for instantiation of {}.{} without type args", 
                    method_key.owning_ty, 
                    method_key.method
                );
                self_ty.clone()
            }
        };
        
        let generic_method_def = self
            .src_metadata
            .find_method(
                &generic_self_ty,
                &method_key.method,
                &method_key.sig,
            )
            .cloned()
            .unwrap_or_else(|| {
                panic!("instantiate_method: missing method def: {}.{}", generic_self_ty, method_key.method)
            });
        let generic_method_decl = generic_method_def.decl.as_ref();

        // the definition we found should already be correctly specialized - you can't pass
        // type args when calling an interface method, so the only thing that would change the method
        // being generated here is the self-type, which we already specialized
        if let Some(ty_args) = type_args {
            let decl_ty_params = generic_method_decl
                .type_params
                .as_ref()
                .expect("instantiate_method: method called with type args must have type params");

            generic_ctx.push(decl_ty_params, ty_args);
        };

        let mut specialized_decl = apply_func_decl_named_ty_args(
            generic_method_decl.clone(),
            &generic_ctx,
            &generic_ctx);
        specialized_decl.name.owning_ty = Some(self_ty);

        let ns = method_key
            .self_ty
            .full_path()
            .expect("instantiate_method: methods should only be generated for named types")
            .into_owned();

        let id = self.declare_func(&specialized_decl, ns, type_args);

        // cache the function before translating the instantiation, because
        // it may recurse and instantiate itself in its own body
        let cached_func = FunctionInstance {
            id,
            sig: Rc::new(specialized_decl.sig()),
        };

        let key = FunctionDefKey {
            decl_key: FunctionDeclKey::Method(method_key.clone()),
            type_args: type_args.cloned(),
        };
        self.translated_funcs.insert(key, cached_func.clone());

        let debug_name = specialized_decl.to_string();
        let ir_func = build_func_def(
            self,
            generic_ctx,
            &generic_method_def.decl.params,
            &generic_method_def.decl.return_ty,
            &generic_method_def.locals,
            &generic_method_def.body,
            generic_method_def.span().clone(),
            debug_name,
        );
        self.module.functions.insert(id, ir::Function::Local(ir_func));

        cached_func
    }

    fn instantiate_virtual_method(&mut self, virtual_key: &VirtualMethodKey) -> FunctionInstance {
        // virtual calls can't have type args yet
        let type_args = None;

        let instance = self.instantiate_method(&virtual_key.impl_method, type_args);

        let iface_ty_name = virtual_key
            .iface_ty
            .full_path()
            .expect("interface type must not be unnamed");

        let iface_id = self
            .find_iface_decl(&iface_ty_name)
            .unwrap_or_else(|| {
                let src_iface_def = self
                    .src_metadata
                    .find_iface_def(&iface_ty_name)
                    .cloned()
                    .unwrap_or_else(|_err| panic!(
                        "failed to get interface def {} referenced in metadata",
                        iface_ty_name,
                    ));

                let mut builder = Builder::new(self);
                let iface_meta = builder.translate_iface(&src_iface_def);
                self.module.metadata.define_iface(iface_meta)
            });
        
        // virtual methods can't be generic
        let generic_ctx = typ::GenericContext::empty();

        let self_ty = self.translate_type(&virtual_key.impl_method.self_ty.clone(), &generic_ctx);
        let method_name = (*virtual_key.impl_method.method.name).clone();

        self.module.metadata.impl_method(iface_id, self_ty, method_name, instance.id);

        let key = FunctionDefKey {
            decl_key: FunctionDeclKey::VirtualMethod(virtual_key.clone()),
            type_args: None,
        };

        self.translated_funcs.insert(key.clone(), instance.clone());

        instance
    }

    // statically reference a method and get a function ID. interface methods are all translated
    // at the end of compilation for a module anyway, but for methods that are referenced statically
    // this call reserves us a function ID
    pub fn translate_method_impl(
        &mut self,
        owning_ty: typ::Type,
        self_ty: typ::Type,
        method_name: Ident,
        method_sig: Rc<typ::FunctionSig>,
        type_args: Option<typ::TypeArgList>,
    ) -> FunctionInstance {
        let mut key = FunctionDefKey {
            decl_key: FunctionDeclKey::Method(MethodDeclKey {
                owning_ty,
                self_ty,
                method: method_name,
                sig: method_sig,
            }),

            // interface method calls can't pass type args
            type_args: type_args.clone(),
        };

        // methods must always be present so make sure they're immediately instantiated
        self.instantiate_func(&mut key)
    }

    pub fn declare_func(
        &mut self,
        func_decl: &typ::ast::FunctionDecl,
        namespace: IdentPath,
        type_args: Option<&typ::TypeArgList>,
    ) -> ir::FunctionID {
        let global_name = match &func_decl.name.owning_ty {
            Some(..) => None,
            
            None => {
                let ns: Vec<_> = namespace
                    .iter()
                    .map(|part| part.to_string())
                    .collect();
                let name = func_decl.name.to_string();

                let global_name = ir::NamePath::new(ns, name);

                Some(match type_args {
                    None => global_name,
                    Some(type_args) => {
                        let params_list = func_decl.type_params
                            .as_ref()
                            .expect("function decl with type args must have type params");
                        let generic_ctx = typ::GenericContext::new(params_list, type_args);

                        let types = type_args.iter()
                            .map(|ty| self.translate_type(ty, &generic_ctx));

                        global_name.with_ty_args(types)
                    },
                })
            },
        };

        self.module.metadata.insert_func(global_name)
    }

    pub fn translate_func(
        &mut self,
        func_name: IdentPath,
        func_sig: Rc<typ::FunctionSig>,
        type_args: Option<typ::TypeArgList>,
    ) -> FunctionInstance {
        let mut key = FunctionDefKey {
            type_args,
            decl_key: FunctionDeclKey::Function { name: func_name, sig: func_sig },
        };

        self.instantiate_func(&mut key)
    }

    pub fn insert_func(&mut self, id: ir::FunctionID, function: ir::Function) {
        assert!(
            self.module.metadata.get_function(id).is_some(),
            "function passed to insert_func must have been previously registered in metadata"
        );

        self.module.functions.insert(id, function);
    }

    pub fn find_iface_decl(&self, iface_ident: &IdentPath) -> Option<ir::InterfaceID> {
        let name = ir::NamePath::from_parts(iface_ident
            .iter()
            .map(Ident::to_string));

        self.module.metadata
            .ifaces()
            .find_map(|(id, decl)| {
                if decl.name == name {
                    Some(id)
                } else {
                    None
                }
            })
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
                let ifaces = real_ty
                    .implemented_ifaces(&self.src_metadata)
                    .unwrap_or_else(|err| {
                        panic!("failed to retrieve implementation list for type {}: {}", real_ty, err)
                    });

                for iface_ty in &ifaces {
                    if iface_ty.type_params().is_some() {
                        // generic types can't be instantiated here
                        continue;
                    }

                    let iface_methods: Vec<_> = iface_ty
                        .methods(&self.src_metadata)
                        .unwrap()
                        .into_iter()
                        .collect();

                    for method in iface_methods {
                        if method.decl.type_params.is_some() {
                            // generic methods can't be instantiated here
                            continue;
                        }

                        let virtual_key = FunctionDeclKey::VirtualMethod(VirtualMethodKey {
                            iface_ty: iface_ty.clone(),
                            impl_method: MethodDeclKey {
                                method: method.decl.name.ident.clone(),
                                owning_ty: iface_ty.clone(),
                                self_ty: real_ty.clone(),
                                sig: Rc::new(method.decl.sig()),
                            },
                        });

                        self.instantiate_func(&mut FunctionDefKey {
                            decl_key: virtual_key,
                            type_args: None,
                        });
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

    pub fn find_type(&self, ty: &typ::Type) -> ir::Type {
        match ty {
            typ::Type::Nothing => ir::Type::Nothing,
            typ::Type::Nil => ir::Type::Nothing.ptr(),

            typ::Type::Interface(iface) => {
                let iface_id = match self.find_iface_decl(iface) {
                    Some(id) => id,
                    None => panic!("missing IR definition for interface {}", iface),
                };

                ir::Type::RcPointer(ir::VirtualTypeID::Interface(iface_id))
            },

            typ::Type::Primitive(typ::Primitive::Boolean) => ir::Type::Bool,

            typ::Type::Primitive(typ::Primitive::Int8) => ir::Type::I8,
            typ::Type::Primitive(typ::Primitive::UInt8) => ir::Type::U8,
            typ::Type::Primitive(typ::Primitive::Int16) => ir::Type::I16,
            typ::Type::Primitive(typ::Primitive::UInt16) => ir::Type::U16,
            typ::Type::Primitive(typ::Primitive::Int32) => ir::Type::I32,
            typ::Type::Primitive(typ::Primitive::UInt32) => ir::Type::U32,
            typ::Type::Primitive(typ::Primitive::Int64) => ir::Type::I64,
            typ::Type::Primitive(typ::Primitive::UInt64) => ir::Type::U64,
            typ::Type::Primitive(typ::Primitive::NativeInt) => ir::Type::ISize,
            typ::Type::Primitive(typ::Primitive::NativeUInt) => ir::Type::USize,

            typ::Type::Primitive(typ::Primitive::Real32) => ir::Type::F32,

            typ::Type::Primitive(typ::Primitive::Pointer) => ir::Type::Nothing.ptr(),

            typ::Type::Pointer(target) => self.find_type(target).ptr(),

            typ::Type::Record(class) | typ::Type::Class(class) => {
                expect_no_generic_args(&class, class.type_args.as_ref());

                let ty_name = ir::NamePath::from_decl(class.as_ref().clone(), self);
                let struct_id = match self.metadata().find_type_decl(&ty_name) {
                    Some(id) => id,
                    None => panic!("{} was not found in metadata (not instantiated)", class),
                };

                match ty {
                    typ::Type::Class(..) => {
                        let class_id = ir::VirtualTypeID::Class(struct_id);
                        ir::Type::RcPointer(class_id)
                    },

                    typ::Type::Record(..) => ir::Type::Struct(struct_id),

                    _ => unreachable!(),
                }
            },

            typ::Type::Array(array_ty) => {
                let element = self.find_type(&array_ty.element_ty);
                ir::Type::Array {
                    element: Rc::new(element),
                    dim: array_ty.dim,
                }
            },

            typ::Type::DynArray { element } => {
                let element = self.find_type(element.as_ref());

                let array_struct = match self.metadata().find_dyn_array_struct(&element) {
                    Some(id) => id,
                    None => panic!(
                        "missing dyn array IR struct definition for element type {}",
                        element
                    ),
                };

                ir::Type::RcPointer(ir::VirtualTypeID::Class(array_struct))
            },

            typ::Type::Variant(variant) => {
                expect_no_generic_args(&variant, variant.type_args.as_ref());

                let ty_name = ir::NamePath::from_decl(variant.as_ref().clone(), self);

                match self.metadata().find_type_decl(&ty_name) {
                    Some(id) => ir::Type::Variant(id),
                    None => panic!("missing IR struct metadata for variant {}", variant),
                }
            },

            typ::Type::MethodSelf => panic!("Self is not a real type in this context"),

            typ::Type::GenericParam(param) => panic!(
                "{} is not a real type in this context",
                param,
            ),

            typ::Type::Function(sig) => match self.find_func_ty(sig) {
                Some(id) => ir::Type::Function(id),
                None => panic!("no type definition for function with sig {}", sig),
            },

            // TODO: enums may later be variably sized
            typ::Type::Enum(..) => ir::Type::ISize,

            typ::Type::Any => ir::Type::RcPointer(ir::VirtualTypeID::Any),
        }
    }
    
    pub fn apply_ty_args<Generic>(&self,
        target: Generic,
        params: &impl TypeParamContainer,
        args: &impl TypeArgResolver
    ) -> Generic 
    where 
        Generic: Specializable,
    {
        target.apply_type_args_by_name(params, args)
    }

    pub fn translate_type(
        &mut self,
        src_ty: &typ::Type,
        generic_ctx: &typ::GenericContext,
    ) -> ir::Type {
        let src_ty = src_ty.clone().apply_type_args_by_name(generic_ctx, generic_ctx);

        if let Some(cached) = self.type_cache.get(&src_ty) {
            return cached.clone();
        }

        // instantiate types which may contain generic params
        let ty = match &src_ty {
            typ::Type::Variant(variant) => {
                let variant_def = self.src_metadata.instantiate_variant_def(variant).unwrap();

                let id = self.module.metadata.reserve_new_struct();
                let ty = ir::Type::Variant(id);
                self.type_cache.insert(src_ty.clone(), ty.clone());

                let name_path = translate_name(&variant, generic_ctx, self);
                self.module.metadata.declare_struct(id, &name_path);

                let variant_meta = translate_variant_def(&variant_def, generic_ctx, self);

                self.module.metadata.define_variant(id, variant_meta);
                ty
            },

            typ::Type::Record(name) | typ::Type::Class(name) => {
                // handle builtin types
                if **name == typ::builtin_string_name() {
                    let string_ty = ir::Type::RcPointer(ir::VirtualTypeID::Class(ir::STRING_ID));

                    self.type_cache.insert(src_ty, string_ty.clone());

                    return string_ty;
                }

                let def = self.src_metadata.instantiate_struct_def(name).unwrap();

                let id = self.module.metadata.reserve_new_struct();

                let ty = match def.kind {
                    StructKind::Class => {
                        ir::Type::RcPointer(ir::VirtualTypeID::Class(id))
                    },
                    StructKind::Record | StructKind::PackedRecord => {
                        ir::Type::Struct(id)
                    },
                };

                self.type_cache.insert(src_ty.clone(), ty.clone());

                let name_path = translate_name(&name, generic_ctx, self);

                self.module.metadata.declare_struct(id, &name_path);

                let struct_meta = translate_struct_def(&def, generic_ctx, self);
                self.module.metadata.define_struct(id, struct_meta);

                ty
            },

            typ::Type::Interface(iface_def) => {
                let iface_def = self.src_metadata
                    .find_iface_def(iface_def)
                    .cloned()
                    .unwrap();

                let iface_name = translate_name(&iface_def.name, generic_ctx, self);
                let id = self.module.metadata.declare_iface(&iface_name);
                let ty = ir::Type::RcPointer(ir::VirtualTypeID::Interface(id));

                self.type_cache.insert(src_ty, ty.clone());

                let iface_meta = translate_iface(&iface_def, generic_ctx, self);
                let def_id = self.module.metadata.define_iface(iface_meta);
                assert_eq!(def_id, id);

                ty
            },

            typ::Type::Array(array_ty) => {
                let elem_ty = self.translate_type(&array_ty.element_ty, generic_ctx);
                let ty = ir::Type::Array {
                    element: Rc::new(elem_ty),
                    dim: array_ty.dim,
                };

                self.type_cache.insert(src_ty, ty.clone());
                ty
            },


            typ::Type::DynArray { element } => {
                let id = self.translate_dyn_array_struct(&element, generic_ctx);

                let ty = ir::Type::RcPointer(ir::VirtualTypeID::Class(id));
                self.type_cache.insert(src_ty, ty.clone());

                ty
            },

            typ::Type::Function(func_sig) => {
                if let Some(id) = self.find_func_ty(&func_sig) {
                    return ir::Type::Function(id);
                }

                let ir_sig = translate_sig(&func_sig, generic_ctx, self);
                let func_ty_id = self.define_func_ty((**func_sig).clone(), ir_sig);

                let ty = ir::Type::RcPointer(ir::VirtualTypeID::Closure(func_ty_id));

                self.type_cache.insert(src_ty, ty.clone());

                ty
            },

            real_ty => {
                // nothing to be instantiated
                let ty = self.find_type(real_ty);
                self.type_cache.insert(src_ty, ty.clone());

                ty
            },
        };

        // ensure the runtime type info exists for all referenced types
        self.runtime_type(&ty);

        // println!("{} <- {}", src_ty, self.pretty_ty_name(&ty_def));

        ty
    }

    pub fn find_func_ty(&self, sig: &typ::FunctionSig) -> Option<ir::TypeDefID> {
        self.function_types_by_sig.get(&sig).cloned()
    }

    pub fn define_func_ty(
        &mut self,
        sig: typ::FunctionSig,
        func_ty: ir::FunctionSig,
    ) -> ir::TypeDefID {
        assert!(!self.function_types_by_sig.contains_key(&sig));

        let decl = ir::TypeDecl::Def(ir::TypeDef::Function(func_ty));

        let id = self.metadata_mut().insert_type_decl(decl);

        self.function_types_by_sig.insert(sig, id);

        id
    }

    // get or generate runtime type for a given type, which contains the function IDs etc
    // used for RC operations at runtime
    pub fn runtime_type(&mut self, ty: &ir::Type) -> ir::RuntimeType {
        if let Some(boilerplate) = self.module.metadata.get_runtime_type(ty) {
            return boilerplate.clone();
        }

        // declare new func IDs then define them here
        let funcs = self.module.metadata.declare_runtime_type(ty);

        // special handling for System.String, which has magic cleanup behaviour for its
        // allocated memory

        let release_body = {
            let mut release_builder = Builder::new(self);
            release_builder.bind_param(ir::LocalID(0), ty.clone().ptr(), "target", true);
            let target_ref = ir::Ref::Local(ir::LocalID(0)).to_deref();

            match ty {
                ir::Type::Struct(ir::STRING_ID) => {
                    let chars_field_ref = release_builder.local_temp(ir::Type::U8.ptr().ptr());
                    release_builder.field(chars_field_ref.clone(), target_ref, ty.clone(), ir::STRING_CHARS_FIELD);
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
            ir::Function::Local(ir::FunctionDef {
                body: release_body,
                sig: ir::FunctionSig {
                    return_ty: ir::Type::Nothing,
                    param_tys: vec![ty.clone().ptr()],
                },
                debug_name: format!("<generated releaser for {}>", self.module.metadata.pretty_ty_name(ty)),
                src_span: self.module_span().clone(),
            }),
        );

        let retain_body = {
            let mut retain_builder = Builder::new(self);
            retain_builder.bind_param(ir::LocalID(0), ty.clone().ptr(), "target", true);
            let target_ref = ir::Ref::Local(ir::LocalID(0)).to_deref();
            retain_builder.visit_deep(target_ref, ty, |builder, el_ty, el_ref| {
                builder.retain(el_ref, el_ty)
            });
            retain_builder.finish()
        };

        self.insert_func(
            funcs.retain,
            ir::Function::Local(ir::FunctionDef {
                body: retain_body,
                sig: ir::FunctionSig {
                    return_ty: ir::Type::Nothing,
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
        generic_ctx: &typ::GenericContext,
    ) -> ir::TypeDefID {
        let element_ty = self.translate_type(element_ty, generic_ctx);

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

    fn class_types(&self) -> impl Iterator<Item = &ir::Type> {
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
        generic_ctx: &typ::GenericContext,
    ) -> ir::TypeDefID {
        let func_ty_id = match self.find_func_ty(&func_sig) {
            Some(id) => id,
            None => {
                let ir_sig = translate_sig(func_sig, generic_ctx, self);
                self.define_func_ty(func_sig.clone(), ir_sig)
            },
        };

        func_ty_id
    }

    pub fn build_closure_instance(
        &mut self,
        func: &typ::ast::AnonymousFunctionDef,
        generic_ctx: &typ::GenericContext,
    ) -> ClosureInstance {
        let id = self.module.metadata.insert_func(None);

        // this is the signature of the *function type* of the closure, not the signature of
        // the real method implementing the closure, which has an extra type-erased parameter
        // for the closure itself
        let func_ty_sig = typ::FunctionSig::of_anonymous_func(func);

        let func_ty_id = self.translate_func_ty(&func_ty_sig, generic_ctx);

        let closure_identity = ir::ClosureIdentity {
            virt_func_ty: func_ty_id,
            module: func.span().file.display().to_string(),
            line: func.span().start.line,
            col: func.span().start.col,
        };
        let closure_id = translate_closure_struct(
            closure_identity,
            &func.captures,
            generic_ctx,
            self
        );

        let debug_name = "<anonymous function>".to_string();

        let cached_func = FunctionInstance { id, sig: func_ty_sig };

        let src_span = func.span().clone();
        let ir_func = build_closure_function_def(self, &func, closure_id, src_span, debug_name);

        self.module.functions.insert(id, ir::Function::Local(ir_func));

        ClosureInstance {
            func_instance: cached_func,
            func_ty_id,
            closure_id,
        }
    }

    pub fn build_func_static_closure_instance(&mut self,
        func: &FunctionInstance,
        generic_ctx: &typ::GenericContext
    ) -> &ir::StaticClosure {
        if let Some(existing) = self.module.metadata.get_static_closure(func.id) {
            return &self.module.static_closures[existing.0];
        }

        // function reference closures can never have a capture list or type args
        let captures = LinkedHashMap::default();

        let func_ty_id = self.translate_func_ty(func.sig.as_ref(), generic_ctx);

        let ir_func = self.module.functions.get(&func.id)
            .expect("function passed to build_function_closure_instance must have been previously translated")
            .clone();

        // the span associated with the static closure should be the function definition
        // itself, not the usage
        let src_span = ir_func.src_span();

        let closure_identity = ir::ClosureIdentity {
            virt_func_ty: func_ty_id,
            col: src_span.start.col,
            line: src_span.end.col,
            module: src_span.file.display().to_string(),
        };

        let closure_id = translate_closure_struct(
            closure_identity,
            &captures,
            generic_ctx,
            self
        );

        // build the closure function, which is a thunk that just calls the global function
        let thunk_id = self.module.metadata.insert_func(None);
        let thunk_def = build_func_static_closure_def(self, func, &ir_func);

        self.module.functions.insert(thunk_id, ir::Function::Local(thunk_def));

        let closure = ClosureInstance {
            closure_id,
            func_ty_id,
            func_instance: FunctionInstance {
                id: thunk_id,
                sig: func.sig.clone(),
            },
        };

        let static_closure_id = self.build_static_closure_instance(closure).id;
        self.module.metadata.insert_static_closure(func.id, static_closure_id);

        &self.module.static_closures[static_closure_id.0]
    }

    pub fn build_static_closure_instance(&mut self, closure: ClosureInstance) -> &ir::StaticClosure {
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

        let id = ir::StaticClosureID(self.module.static_closures.len());
        let instance = build_static_closure_impl(closure, id, self);

        self.module.static_closures.push(instance);

        &self.module.static_closures[self.module.static_closures.len() - 1]
    }

    /// Add static closure init function calls at top of init block
    fn gen_static_closure_init(&mut self) {
        let mut static_closures_init = Vec::new();
        for static_closure in &self.module.static_closures {
            static_closures_init.push(ir::Instruction::Call {
                function: ir::Value::Ref(ir::Ref::Global(ir::GlobalRef::Function(static_closure.init_func))),
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
    Function { name: IdentPath, sig: Rc<typ::FunctionSig> },
    Method(MethodDeclKey),
    VirtualMethod(VirtualMethodKey),
}

impl FunctionDeclKey {
    pub fn namespace(&self) -> Cow<IdentPath> {
        match self {
            FunctionDeclKey::Function { name, .. } => name
                .parent()
                .map(Cow::Owned)
                .expect("all functions must be declared within a namespace!"),
            
            FunctionDeclKey::VirtualMethod(key) => key
                .iface_ty
                .full_path()
                .expect("types used as interfaces should never be unnamed"),

            FunctionDeclKey::Method(key) => key
                .owning_ty
                .full_path()
                .expect("types with method implementations should never be unnamed"),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct MethodDeclKey {
    pub owning_ty: typ::Type,
    pub self_ty: typ::Type,

    pub method: Ident,
    pub sig: Rc<typ::FunctionSig>,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct VirtualMethodKey {
    pub iface_ty: typ::Type,

    pub impl_method: MethodDeclKey,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub(crate) struct FunctionDefKey {
    pub decl_key: FunctionDeclKey,
    pub type_args: Option<typ::TypeArgList>,
}

fn gen_dyn_array_funcs(module: &mut ModuleBuilder, elem_ty: &ir::Type, struct_id: ir::TypeDefID) {
    let mut alloc_builder = Builder::new(module);
    gen_dyn_array_alloc_func(&mut alloc_builder, elem_ty, struct_id);
    let alloc_body = alloc_builder.finish();

    let dyn_array_rtti = module.metadata().get_dynarray_runtime_type(elem_ty)
        .expect("missing dynarray rtti for type");

    module.insert_func(dyn_array_rtti.alloc, ir::Function::Local(ir::FunctionDef {
        debug_name: format!(
            "dynarray alloc function for element type {}",
            module.metadata().pretty_ty_name(elem_ty)
        ),
        sig: ir::FunctionSig {
            param_tys: vec![ir::Type::any(), ir::Type::I32, ir::Type::any(), ir::Type::any()],
            return_ty: ir::Type::Nothing,
        },
        body: alloc_body,
        src_span: module.module_span().clone(),
    }));

    let mut length_builder = Builder::new(module);
    gen_dyn_array_length_func(&mut length_builder, struct_id);
    let length_body = length_builder.finish();

    module.insert_func(dyn_array_rtti.length, ir::Function::Local(ir::FunctionDef {
        debug_name: format!(
            "dynarray length function for element type {}",
            module.metadata().pretty_ty_name(elem_ty)
        ),
        sig: ir::FunctionSig {
            param_tys: vec![ir::Type::any()],
            return_ty: ir::Type::I32,
        },
        body: length_body,
        src_span: module.module_span().clone(),
    }));
}

fn gen_dyn_array_alloc_func(builder: &mut Builder, elem_ty: &ir::Type, struct_id: ir::TypeDefID) {
    let array_ref_ty = ir::Type::RcPointer(ir::VirtualTypeID::Class(struct_id));
    let el_ptr_ty = elem_ty.clone().ptr();

    builder.comment("bind params");
    let arr_arg = ir::LocalID(0);
    let len_arg = ir::LocalID(1);
    let src_arr_arg = ir::LocalID(2);
    let default_val_arg = ir::LocalID(3);
    builder.bind_param(arr_arg, ir::Type::any(), "arr_ptr", false);
    builder.bind_param(len_arg, ir::Type::I32, "len", false);
    builder.bind_param(src_arr_arg, ir::Type::any(), "src_arr_ptr", false);
    builder.bind_param(default_val_arg, ir::Type::Nothing.ptr(), "default_val", false);

    builder.comment("retain the refs to the array params");
    builder.retain(ir::Ref::Local(ir::LocalID(0)), &ir::Type::any());
    builder.retain(ir::Ref::Local(ir::LocalID(2)), &ir::Type::any());

    builder.comment("cast the array params to this array type");
    let arr = builder.local_temp(array_ref_ty.clone());
    let src_arr = builder.local_temp(array_ref_ty.clone());
    builder.cast(arr.clone(), ir::Ref::Local(ir::LocalID(0)), array_ref_ty.clone());
    builder.cast(src_arr.clone(), ir::Ref::Local(ir::LocalID(2)), array_ref_ty.clone());

    let default_el_ptr = builder.local_temp(el_ptr_ty.clone());
    builder.cast(default_el_ptr.clone(), default_val_arg, el_ptr_ty.clone());

    builder.comment("copy_len := copy_from->length");
    let src_len = builder.local_temp(ir::Type::I32);
    builder.field_val(src_len.clone(), src_arr.clone(), array_ref_ty.clone(), ir::DYNARRAY_LEN_FIELD, ir::Type::I32);

    builder.comment("el_len := sizeof(elem_ty)");
    let el_len = builder.local_temp(ir::Type::I32);
    builder.size_of(el_len.clone(), elem_ty.clone());

    builder.comment("data_len := el_len * len");
    let data_len = builder.local_temp(ir::Type::I32);
    builder.mul(data_len.clone(), el_len.clone(), len_arg);

    builder.comment("data = GetMem(data_len) as ^elem_ty");
    let data_mem = builder.local_temp(ir::Type::U8.ptr());
    builder.get_mem(data_len, data_mem.clone());
    let data = builder.local_temp(el_ptr_ty.clone());
    builder.cast(data.clone(), data_mem, el_ptr_ty.clone());

    builder.comment("iteration counter for initializing elements");
    let counter = builder.local_temp(ir::Type::I32);
    builder.mov(counter.clone(), ir::Value::LiteralI32(0));

    builder.comment("loop break flag we use in a couple of places later");
    let done = builder.local_temp(ir::Type::Bool);

    builder.comment("copy elements from copied array");
    builder.scope(|builder| {
        let copy_count = builder.local_temp(ir::Type::I32);
        let copy_count_ok = builder.local_temp(ir::Type::Bool);

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
            builder.field_val(copy_src.clone(), src_arr.clone(), array_ref_ty.clone(), ir::DYNARRAY_PTR_FIELD, el_ptr_ty.clone());
            builder.add(copy_src.clone(), copy_src.clone(), counter.clone());

            builder.comment("copy_dst^ := copy_src^");
            builder.mov(copy_dst.clone().to_deref(), copy_src.to_deref());

            builder.retain(copy_dst.to_deref(), elem_ty);
        });

        builder.comment("counter += 1");
        builder.add(counter.clone(), counter.clone(), ir::Value::LiteralI32(1));

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
    builder.add(counter.clone(), counter.clone(), ir::Value::LiteralI32(1));
    builder.jmp(init_loop_label);

    builder.label(init_break_label);

    builder.set_field(arr.clone(), array_ref_ty.clone(), ir::DYNARRAY_LEN_FIELD, ir::Type::I32, len_arg);
    builder.set_field(arr, array_ref_ty, ir::DYNARRAY_PTR_FIELD, el_ptr_ty, data);
}

fn gen_dyn_array_length_func(builder: &mut Builder, struct_id: ir::TypeDefID) {
    let array_ref_ty = ir::Type::RcPointer(ir::VirtualTypeID::Class(struct_id));

    builder.comment("bind and retain params");
    builder.bind_return();
    builder.bind_param(ir::LocalID(1), ir::Type::any(), "arr_ptr", false);
    builder.retain(ir::Ref::Local(ir::LocalID(1)), &ir::Type::any());

    builder.comment("cast pointer down to this array type");
    let arr = builder.local_temp(array_ref_ty.clone());
    builder.cast(arr.clone(), ir::Ref::Local(ir::LocalID(1)), array_ref_ty.clone());

    builder.comment("evaluate length field into return ref");
    builder.field_val(ir::RETURN_REF, arr, array_ref_ty, ir::DYNARRAY_LEN_FIELD, ir::Type::I32);
}

fn gen_dyn_array_rc_boilerplate(module: &mut ModuleBuilder, elem_ty: &ir::Type, struct_id: ir::TypeDefID) {
    let array_ref_ty = ir::Type::RcPointer(ir::VirtualTypeID::Class(struct_id));
    let array_struct_ty = ir::Type::Struct(struct_id);

    let rc_boilerplate = module
        .metadata()
        .get_runtime_type(&array_struct_ty)
        .expect("rtti function ids for dynarray inner struct must exist");

    let mut builder = Builder::new(module);

    builder.comment("%0 is the self arg, the pointer to the inner struct");
    builder.bind_param(ir::LocalID(0), array_struct_ty.clone().ptr(), "self", true);
    let self_arg = ir::Ref::Local(ir::LocalID(0)).to_deref();

    builder.comment("pointer to the length field of the dynarray object");
    let len_field_ptr = builder.local_temp(ir::Type::I32.ptr());

    builder.comment("pointer to the pointer field of the dynarray object");
    let arr_field_ptr = builder.local_temp(elem_ty.clone().ptr().ptr());

    builder.comment("u8 pointer type field to cast the array memory into to call FreeMem");
    let arr_mem_ptr = builder.local_temp(ir::Type::U8.ptr());

    builder.comment("iteration vars");
    let counter = builder.local_temp(ir::Type::I32);
    let has_more = builder.local_temp(ir::Type::Bool);
    let el_ptr = builder.local_temp(elem_ty.clone().ptr());

    let zero_elements = builder.local_temp(ir::Type::Bool);

    builder.comment("jump to loop end if counter == array len");
    let start_loop_label = builder.alloc_label();
    let end_loop_label = builder.alloc_label();

    let after_free = builder.alloc_label();

    builder.field(len_field_ptr.clone(), self_arg.clone(), array_struct_ty.clone(), ir::DYNARRAY_LEN_FIELD);
    builder.field(arr_field_ptr.clone(), self_arg, array_struct_ty.clone(), ir::DYNARRAY_PTR_FIELD);

    builder.comment("release every element");
    builder.mov(counter.clone(), ir::Value::LiteralI32(0));

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
    builder.add(counter.clone(), counter, ir::Value::LiteralI32(1));

    builder.jmp(start_loop_label);
    builder.label(end_loop_label);

    builder.comment("free the dynamic-allocated buffer - if len > 0");
    builder.eq(zero_elements.clone(), len_field_ptr.clone().to_deref(), ir::Value::LiteralI32(0));
    builder.jmp_if(after_free, zero_elements);

    builder.cast(arr_mem_ptr.clone(), arr_field_ptr.clone().to_deref(), ir::Type::U8.ptr());
    builder.free_mem(arr_mem_ptr);

    builder.append(ir::Instruction::Label(after_free));

    builder.mov(len_field_ptr.to_deref(), ir::Value::LiteralI32(0));
    builder.mov(arr_field_ptr.to_deref(), ir::Value::LiteralNull);

    let releaser_body = builder.finish();

    let array_ref_ty_name = module.metadata().pretty_ty_name(&array_ref_ty).into_owned();

    module.insert_func(
        rc_boilerplate.release,
        ir::Function::Local(ir::FunctionDef {
            debug_name: format!("<generated dynarray releaser for {}>", array_ref_ty_name),
            sig: ir::FunctionSig {
                return_ty: ir::Type::Nothing,
                param_tys: vec![array_struct_ty.clone().ptr()],
            },
            body: releaser_body,
            src_span: module.module_span().clone(),
        }),
    );

    // no custom retain behaviour (dynarrays can't be retained!)
    module.insert_func(
        rc_boilerplate.retain,
        ir::Function::Local(ir::FunctionDef {
            debug_name: format!("<generated empty retainer for {}>", array_ref_ty_name),
            sig: ir::FunctionSig {
                return_ty: ir::Type::Nothing,
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
fn gen_class_rc_boilerplate(module: &mut ModuleBuilder, class_ty: &ir::Type) {
    let resource_struct = class_ty
        .rc_resource_class_id()
        .and_then(|class_id| class_id.as_class())
        .expect("resource class of translated class type was not a struct");

    module.runtime_type(&ir::Type::Struct(resource_struct));
}

fn expect_no_generic_args<T: fmt::Display>(target: &T, type_args: Option<&typ::TypeArgList>) {
    if let Some(type_args) = type_args {
        let any_generic_args = type_args.items.iter().any(|arg| arg.is_generic_param());
        assert!(
            !any_generic_args,
            "name of translated variant must not contain unspecialized generics: {}",
            target
        );
    }
}
