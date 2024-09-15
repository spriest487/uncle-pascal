use crate::typecheck::ast::typecheck_expr;
use crate::typecheck::string_type;
use crate::typecheck::Context;
use crate::typecheck::Type;
use crate::typecheck::Typed;
use crate::typecheck::TypecheckResult;
use crate::typecheck::TypedValue;
use crate::typecheck::ValueKind;
use pas_common::span::Span;
use pas_common::span::Spanned;
use crate::ast;

pub type Raise = ast::Raise<Typed>;

pub fn typecheck_raise(
    raise: &ast::Raise<Span>,
    expect_ty: &Type,
    ctx: &mut Context,
) -> TypecheckResult<Raise> {
    let string_ty = string_type(ctx)?;
    let value = typecheck_expr(&raise.value, &string_ty, ctx)?;
    value.annotation().expect_value(&string_ty)?;

    // the "raise" expr just aborts, so it has whatever type is expected of it, so we
    // can use it in any expr position
    let annotation = TypedValue {
        ty: expect_ty.clone(),
        span: raise.span().clone(),
        decl: None,
        value_kind: ValueKind::Temporary,
    }
    .into();

    Ok(Raise {
        value: Box::new(value),
        annotation,
    })
}
