use crate::ast::prelude::*;

pub type Raise = ast::Raise<TypeAnnotation>;

pub fn typecheck_raise(
    raise: &ast::Raise<Span>,
    expect_ty: &Type,
    ctx: &mut Context
) -> TypecheckResult<Raise> {
    let string_ty = string_type(ctx)?;
    let value = typecheck_expr(&raise.value, &string_ty, ctx)?;
    value.annotation().expect_value(&string_ty)?;

    // the "raise" expression just aborts, so it has whatever type is expected of it, so we
    // can use it in any expression position
    let annotation = TypedValueAnnotation {
        ty: expect_ty.clone(),
        span: raise.span().clone(),
        decl: None,
        value_kind: ValueKind::Temporary,
    }.into();

    Ok(Raise {
        value: Box::new(value),
        annotation,
    })
}