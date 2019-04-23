use {
    crate::ast::prelude::*,
};

pub type IfCond = ast::IfCond<TypeAnnotation>;

pub fn typecheck_if_cond(
    if_cond: &ast::IfCond<Span>,
    expect_ty: &Type,
    ctx: &mut Context)
    -> TypecheckResult<IfCond>
{
    let cond = typecheck_expr(&if_cond.cond, &Type::Primitive(Primitive::Boolean), ctx)?;
    if *cond.annotation.ty() != Type::Primitive(Primitive::Boolean) {
        return Err(TypecheckError::TypeMismatch {
            expected: Type::Primitive(Primitive::Boolean),
            actual: cond.annotation.ty().clone(),
            span: cond.annotation.span().clone(),
        });
    }

    let then_branch = typecheck_expr(&if_cond.then_branch, expect_ty, ctx)?;
    let else_branch = match &if_cond.else_branch {
        Some(else_expr) => Some(typecheck_expr(else_expr, expect_ty, ctx)?),
        None => None
    };

    let span = if_cond.span().clone();

    let annotation = match (then_branch.annotation.ty(), else_branch.as_ref()) {
        (Type::Nothing, _) | (_, None) => {
            TypeAnnotation::Untyped(span)
        },

        (then_ty, Some(else_branch)) => {
            if *else_branch.annotation.ty() != *then_ty {
                return Err(TypecheckError::TypeMismatch {
                    expected: then_branch.annotation.ty().clone(),
                    actual: else_branch.annotation.ty().clone(),
                    span: else_branch.annotation.span().clone(),
                });
            } else {
                TypeAnnotation::TypedValue {
                    ty: then_ty.clone(),
                    value_kind: ValueKind::Temporary,
                    span
                }
            }
        }
    };

    Ok(IfCond {
        cond,
        then_branch,
        else_branch,
        annotation,
    })
}