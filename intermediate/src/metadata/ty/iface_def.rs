use crate::module_builder::ModuleBuilder;
use crate::translate_name;
use crate::typ;
use crate::ir;

pub fn translate_iface(
    iface_def: &typ::ast::InterfaceDecl,
    type_args: Option<&typ::TypeList>,
    module: &mut ModuleBuilder,
) -> ir::Interface {
    let name = translate_name(&iface_def.name, type_args, module);

    // it needs to be declared to reference its own ID in the Self type
    let id = module.metadata_mut().declare_iface(&name);

    let methods: Vec<_> = iface_def
        .methods
        .iter()
        .map(|method| {
            let self_ty = ir::Type::RcPointer(ir::VirtualTypeID::Interface(id));

            ir::Method {
                name: method.ident().to_string(),
                return_ty: match &method.decl.return_ty {
                    Some(typ::Type::MethodSelf) => self_ty.clone(),
                    Some(return_ty) => module.translate_type(return_ty, type_args),
                    None => ir::Type::Nothing,
                },
                params: method
                    .decl
                    .params
                    .iter()
                    .map(|param| match &param.ty {
                        typ::Type::MethodSelf => self_ty.clone(),
                        param_ty => module.translate_type(param_ty, type_args),
                    })
                    .collect(),
            }
        })
        .collect();

    ir::Interface::new(name, methods)
}
