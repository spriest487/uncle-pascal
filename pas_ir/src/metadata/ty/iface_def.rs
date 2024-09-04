use std::collections::HashMap;
use crate::{FunctionID, MethodID, Module, NamePath, pas_ty, translate_name, Type, VirtualTypeID};

#[derive(Clone, Debug)]
pub struct Method {
    pub name: String,
    pub return_ty: Type,
    pub params: Vec<Type>,
}

#[derive(Clone, Debug)]
pub struct Interface {
    pub name: NamePath,
    pub methods: Vec<Method>,
    pub impls: HashMap<Type, InterfaceImpl>,
}

impl Interface {
    pub fn new(name: impl Into<NamePath>, methods: impl Into<Vec<Method>>) -> Self {
        Self {
            name: name.into(),
            methods: methods.into(),
            impls: HashMap::new(),
        }
    }

    pub fn add_impl(&mut self,
        implementor: Type,
        method: MethodID,
        func_id: FunctionID,
    ) {
        assert!(method.0 < self.methods.len());

        let methods_len = self.methods.len();
        let impl_entry = self
            .impls
            .entry(implementor.clone())
            .or_insert_with(|| InterfaceImpl::new(methods_len));
        assert!(
            !impl_entry.methods.contains_key(&method),
            "adding duplicate impl ({}) of method {}.{} for {}, already defined as {}",
            func_id,
            self.name,
            self.methods[method.0].name,
            implementor,
            impl_entry.methods[&method],
        );

        impl_entry.methods.insert(method, func_id);
    }

    pub fn method_index(&self, name: &str) -> Option<MethodID> {
        self.methods
            .iter()
            .position(|m| m.name.as_str() == name)
            .map(MethodID)
    }

    pub fn get_method(&self, id: MethodID) -> Option<&Method> {
        self.methods.get(id.0)
    }
}

#[derive(Clone, Debug)]
pub struct InterfaceImpl {
    // method index -> method impl
    pub methods: HashMap<MethodID, FunctionID>,
}

impl InterfaceImpl {
    fn new(method_count: usize) -> Self {
        Self {
            methods: HashMap::with_capacity(method_count),
        }
    }
}

pub fn translate_iface(
    iface_def: &pas_ty::ast::InterfaceDecl,
    type_args: Option<&pas_ty::TypeList>,
    module: &mut Module,
) -> Interface {
    let name = translate_name(&iface_def.name, type_args, module);

    // it needs to be declared to reference its own ID in the Self type
    let id = module.metadata.declare_iface(&name);

    let methods: Vec<_> = iface_def
        .methods
        .iter()
        .map(|method| {
            let self_ty = Type::RcPointer(VirtualTypeID::Interface(id));

            Method {
                name: method.ident().to_string(),
                return_ty: match &method.decl.return_ty {
                    Some(pas_ty::Type::MethodSelf) => self_ty.clone(),
                    Some(return_ty) => module.translate_type(return_ty, type_args),
                    None => Type::Nothing,
                },
                params: method
                    .decl
                    .params
                    .iter()
                    .map(|param| match &param.ty {
                        pas_ty::Type::MethodSelf => self_ty.clone(),
                        param_ty => module.translate_type(param_ty, type_args),
                    })
                    .collect(),
            }
        })
        .collect();

    Interface::new(name, methods)
}
