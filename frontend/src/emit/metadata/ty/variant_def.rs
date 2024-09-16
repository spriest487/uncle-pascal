use crate::emit::ir;
use crate::emit::module_builder::ModuleBuilder;
use crate::emit::translate_name;
use crate::emit::typ;
use common::span::Spanned;

pub fn translate_variant_def(
    variant_def: &typ::ast::VariantDef,
    type_args: Option<&typ::TypeList>,
    module: &mut ModuleBuilder,
) -> ir::VariantDef {
    let name_path = translate_name(&variant_def.name, type_args, module);

    let mut cases = Vec::new();
    for case in &variant_def.cases {
        let (case_ty, case_rc) = match case.data_ty.as_ref() {
            Some(data_ty) => {
                let case_ty = module.translate_type(data_ty, type_args);
                (Some(case_ty), data_ty.is_rc_reference())
            },
            None => (None, false),
        };

        cases.push(ir::VariantCase {
            name: case.ident.to_string(),
            ty: case_ty,
            rc: case_rc,
        });
    }

    ir::VariantDef {
        name: name_path,
        src_span: Some(variant_def.span().clone()),
        cases,
    }
}
