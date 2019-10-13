use crate::ast::prelude::*;

pub type IfCond = ast::IfCond<TypeAnnotation>;

pub fn typecheck_if_cond(
    if_cond: &ast::IfCond<Span>,
    expect_ty: &Type,
    ctx: &mut Context,
) -> TypecheckResult<IfCond> {
    // condition expr has a boolean hint if we're not doing an is-match
    let cond_expect_ty = match if_cond.is_pattern {
        Some(..) => Type::Nothing,
        None => Type::Primitive(Primitive::Boolean),
    };

    let cond = typecheck_expr(&if_cond.cond, &cond_expect_ty, ctx)?;

    // ...and verify the condition is definitely a boolean in non-is-match conditions
    if if_cond.is_pattern.is_none() {
        cond.annotation()
            .expect_value(&Type::Primitive(Primitive::Boolean))?;
    }

    let is_pattern = match &if_cond.is_pattern {
        Some(pattern) => Some(TypePattern::typecheck(
            pattern,
            cond.annotation().ty(),
            ctx,
        )?),
        None => None,
    };

    let mut then_ctx = ctx.clone();

    // is-pattern binding only exists in the "then" branch, if present
    if let Some(pattern) = &is_pattern {
        for binding in pattern.bindings(ctx)? {
            then_ctx.declare_binding(
                binding.ident.clone(),
                Binding {
                    kind: ValueKind::Immutable,
                    ty: binding.ty.clone(),
                    def: Some(binding.ident.span().clone()),
                },
            )?;
        }
    }

    let then_branch = typecheck_expr(&if_cond.then_branch, expect_ty, &mut then_ctx)?;
    let else_branch = match &if_cond.else_branch {
        Some(else_expr) => {
            let mut else_ctx = ctx.clone();
            let else_expr = typecheck_expr(else_expr, expect_ty, &mut else_ctx)?;

            ctx.consolidate_branches(&[then_ctx, else_ctx]);
            Some(else_expr)
        }
        None => {
            ctx.consolidate_branches(&[then_ctx]);
            None
        }
    };

    let span = if_cond.span().clone();

    let annotation = match (then_branch.annotation().ty(), else_branch.as_ref()) {
        (Type::Nothing, _) | (_, None) => TypeAnnotation::Untyped(span),

        (then_ty, Some(else_branch)) => {
            if *else_branch.annotation().ty() != *then_ty {
                return Err(TypecheckError::TypeMismatch {
                    expected: then_branch.annotation().ty().clone(),
                    actual: else_branch.annotation().ty().clone(),
                    span: else_branch.annotation().span().clone(),
                });
            } else {
                TypeAnnotation::TypedValue {
                    ty: then_ty.clone(),
                    value_kind: ValueKind::Temporary,
                    span,
                    decl: None,
                }
            }
        }
    };

    Ok(IfCond {
        cond,
        is_pattern,
        then_branch,
        else_branch,
        annotation,
    })
}
