use crate::ast::Call;
use crate::ast::VariantCtorCall;
use crate::typ::ast::Expr;
use crate::typ::Context;
use crate::typ::GenericError;
use crate::typ::GenericTarget;
use crate::typ::GenericTypeHint;
use crate::typ::NameContainer;
use crate::typ::NameError;
use crate::typ::Specializable;
use crate::typ::Symbol;
use crate::typ::Type;
use crate::typ::TypeError;
use crate::typ::TypeResult;
use crate::typ::TypedValue;
use crate::typ::VariantCaseTyped;
use crate::Ident;
use common::span::Span;
use common::span::Spanned;
use std::rc::Rc;

pub fn typecheck_variant_case(
    variant_name: &Symbol,
    member_ident: &Ident,
    span: &Span,
    ctx: &mut Context,
) -> TypeResult<VariantCaseTyped> {
    if let Some(args_list) = &variant_name.type_args {
        return Err(TypeError::InvalidExplicitVariantCtorTypeArgs {
            span: args_list.span.clone(),
        });
    }
    assert!(
        variant_name.type_args.is_none(),
        "shouldn't be possible to have explicit type args for a variant constructor expr"
    );

    // we check the named case exists in the unspecialized definition here, but
    // we don't want to try instantiating the actual variant type because we have
    // no information about its type args.
    let variant_def = ctx
        .find_variant_def(&variant_name.full_path)
        .map_err(|err| TypeError::from_name_err(err, span.clone()))?;

    let case_exists = variant_def.find_case(member_ident).is_some();

    if !case_exists {
        return Err(TypeError::from_name_err(
            NameError::MemberNotFound {
                base: NameContainer::Type(Type::variant(variant_name.clone())),
                member: member_ident.clone(),
            },
            span.clone(),
        ));
    }

    let ctor_annotation = VariantCaseTyped {
        variant_name: Rc::new(variant_name.clone()),
        case: member_ident.clone(),
        span: member_ident.span().clone(),
    };

    Ok(ctor_annotation)
}

pub fn try_expr_into_noargs_variant_ctor(
    case_annotation: &VariantCaseTyped,
    expect_ty: &Type,
    span: &Span,
    ctx: &Context
) -> TypeResult<Option<Expr>> {
    let mut variant_name = case_annotation.variant_name.clone();

    // if the variant is generic, we have to be able to infer the type from the usage
    if variant_name.is_unspecialized_generic() {
        let inferred_name = match expect_ty {
            Type::Variant(expect_name) => {
                variant_name.infer_specialized_from_hint(expect_name).cloned()
            }

            _ => None,
        };

        match inferred_name {
            None => {
                let infer_err = GenericError::CannotInferArgs {
                    target: GenericTarget::Name(variant_name.full_path.clone()),
                    hint: GenericTypeHint::ExpectedValueType(expect_ty.clone()),
                };

                return Err(TypeError::from_generic_err(infer_err, span.clone()));
            }

            Some(name) => {
                variant_name = Rc::new(name);
            }
        }
    }

    // we don't need to specialize the def, we only need to check if the case has a data arg
    let variant_def = ctx
        .find_variant_def(&variant_name.full_path)
        .map_err(|e| TypeError::from_name_err(e, span.clone()))?;

    match variant_def.find_case(&case_annotation.case) {
        None => return Ok(None),

        Some(case) => {
            if case.data_ty.is_some() {
                return Ok(None);
            }
        }
    };

    let variant_ty = Type::Variant(variant_name.clone());

    let ctor_call = VariantCtorCall {
        case: case_annotation.case.clone(),
        variant: variant_name.clone(),
        arg: None,
        annotation: TypedValue::temp(variant_ty, span.clone()).into(),
    };

    let call_expr = Expr::Call(Box::new(Call::VariantCtor(ctor_call)));

    Ok(Some(call_expr))
}
