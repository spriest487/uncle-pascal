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

    fn check_matchable(ty: &Type, span: &Span) -> TypecheckResult<()> {
        if !ty.is_matchable() {
            Err(TypecheckError::NotMatchable {
                ty: ty.clone(),
                span: span.clone(),
            })
        } else {
            Ok(())
        }
    }

    let is_pattern: Option<ast::CondPattern<TypeAnnotation>> = match &if_cond.is_pattern {
        Some(ast::CondPattern::Positive {
            is_ty,
            binding,
            span,
        }) => {
            let is_ty = ctx.find_type(is_ty)?;
            check_matchable(&is_ty, span)?;

            Some(ast::CondPattern::Positive {
                is_ty,
                binding: binding.clone(),
                span: span.clone(),
            })
        }
        Some(ast::CondPattern::Negative { is_not_ty, span }) => {
            let is_not_ty = ctx.find_type(is_not_ty)?;
            check_matchable(&is_not_ty, span)?;

            Some(ast::CondPattern::Negative {
                is_not_ty,
                span: span.clone(),
            })
        }
        None => None,
    };

    let mut then_ctx = ctx.clone();

    // is-pattern binding only exists in the "then" branch, if present
    if let Some(ast::CondPattern::Positive {
        binding: Some(binding),
        is_ty,
        span: binding_span,
        ..
    }) = &is_pattern
    {
        then_ctx.declare_binding(
            binding.clone(),
            Binding {
                kind: cond.annotation().value_kind().unwrap(),
                ty: is_ty.clone(),
                def: Some(binding_span.clone()),
            },
        )?;
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
