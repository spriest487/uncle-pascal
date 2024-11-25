use crate::codegen::ir;
use crate::codegen::library_builder::LibraryBuilder;
use crate::codegen::translate_name;
use crate::codegen::typ;
use common::span::Spanned;

pub fn translate_variant_def(
    variant_def: &typ::ast::VariantDef,
    generic_ctx: &typ::GenericContext,
    module: &mut LibraryBuilder,
) -> ir::VariantDef {
    let name_path = translate_name(&variant_def.name, generic_ctx, module);

    let mut cases = Vec::new();
    for case in &variant_def.cases {
        let (case_ty, case_rc) = match case.data_ty.as_ref() {
            Some(data_ty) => {
                let case_ty = module.translate_type(data_ty, generic_ctx);
                (Some(case_ty), data_ty.is_strong_rc_reference())
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
