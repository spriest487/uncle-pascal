use pas_common::span::{Span, Spanned};
use crate::{Module, NamePath, pas_ty, translate_name, Type};

#[derive(Clone, Debug)]
pub struct VariantCase {
    pub name: String,
    pub ty: Option<Type>,
    pub rc: bool,
}

#[derive(Clone, Debug)]
pub struct Variant {
    pub name: NamePath,
    pub cases: Vec<VariantCase>,

    pub src_span: Option<Span>,
}

pub fn translate_variant(
    variant_def: &pas_ty::ast::Variant,
    type_args: Option<&pas_ty::TypeList>,
    module: &mut Module,
) -> Variant {
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