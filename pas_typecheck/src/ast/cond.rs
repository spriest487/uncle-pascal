use std::borrow::Cow;
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

    // if there's no is-match, implicit conversion of the condition expression to bool
    let cond = match if_cond.is_pattern {
        Some(..) => cond,
        None => implicit_conversion(cond, &Type::Primitive(Primitive::Boolean), ctx)?,
    };

    let is_pattern = match &if_cond.is_pattern {
        Some(pattern) => Some(TypePattern::typecheck(
            pattern,
            &cond.annotation().ty(),
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
            let then_ty = then_branch.annotation().ty();

            let else_expr = typecheck_expr(else_expr, &then_ty, &mut else_ctx)?;
            let else_expr = match then_ty.as_ref() {
                Type::Nothing => else_expr,
                then_ty => implicit_conversion(else_expr, &then_ty, ctx)?,
            };

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
        | (Cow::Owned(Type::Nothing) | Cow::Borrowed(Type::Nothing), _)
        | (_, None) => TypeAnnotation::Untyped(span),

        | (then_ty, Some(_else_branch)) => {
            TypedValueAnnotation {
                ty: then_ty.into_owned(),
                value_kind: ValueKind::Temporary,
                span,
                decl: None,
            }.into()
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
