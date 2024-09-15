use crate::ast;
use crate::typecheck::ast::implicit_conversion;
use crate::typecheck::ast::typecheck_expr;
use crate::typecheck::ast::typecheck_stmt;
use crate::typecheck::ast::Expr;
use crate::typecheck::ast::Stmt;
use crate::typecheck::Binding;
use crate::typecheck::Context;
use crate::typecheck::Primitive;
use crate::typecheck::Type;
use crate::typecheck::TypePattern;
use crate::typecheck::TypecheckError;
use crate::typecheck::TypecheckResult;
use crate::typecheck::Typed;
use crate::typecheck::TypedValue;
use crate::typecheck::ValueKind;
use pas_common::span::Span;
use pas_common::span::Spanned;
use std::borrow::Cow;

pub type IfCond<B> = ast::IfCond<Typed, B>;
pub type IfCondExpression = ast::IfCond<Typed, Expr>;
pub type IfCondStatement = ast::IfCond<Typed, Stmt>;

fn typecheck_cond_expr<B>(
    if_cond: &ast::IfCond<Span, B>,
    ctx: &mut Context,
) -> TypecheckResult<Expr> {
    // condition expr has a boolean hint if we're not doing an is-match
    let cond_expect_ty = if if_cond.is_pattern.is_some() {
        Type::Nothing
    } else {
        Type::Primitive(Primitive::Boolean)
    };

    let cond = typecheck_expr(&if_cond.cond, &cond_expect_ty, ctx)?;

    // if there's no is-match, implicit conversion of the condition expr to bool
    let cond = match if_cond.is_pattern {
        Some(..) => cond,
        None => implicit_conversion(cond, &Type::Primitive(Primitive::Boolean), ctx)?,
    };

    Ok(cond)
}

fn typecheck_pattern_match<B>(
    if_cond: &ast::IfCond<Span, B>,
    cond: &Expr,
    ctx: &mut Context,
) -> TypecheckResult<Option<TypePattern>> {
    let is_pattern = match &if_cond.is_pattern {
        Some(pattern) => {
            let pattern = TypePattern::typecheck(pattern, &cond.annotation().ty(), ctx)?;

            Some(pattern)
        },

        None => None,
    };

    Ok(is_pattern)
}

fn create_then_branch_ctx(
    is_pattern: Option<&TypePattern>,
    ctx: &mut Context,
) -> TypecheckResult<Context> {
    let mut then_ctx = ctx.clone();

    // is-pattern binding only exists in the "then" branch, if present
    if let Some(pattern) = &is_pattern {
        let bindings = pattern
            .bindings(ctx)
            .map_err(|err| TypecheckError::from_name_err(err, pattern.span().clone()))?;

        for binding in bindings {
            then_ctx.declare_binding(
                binding.ident.clone(),
                Binding {
                    kind: ValueKind::Immutable,
                    ty: binding.ty.clone(),
                    def: Some(binding.ident.clone()),
                },
            )?;
        }
    }

    Ok(then_ctx)
}

pub fn typecheck_if_cond_stmt(
    if_cond: &ast::IfCond<Span, ast::Stmt<Span>>,
    expect_ty: &Type,
    ctx: &mut Context,
) -> TypecheckResult<IfCond<Stmt>> {
    let cond = typecheck_cond_expr(&if_cond, ctx)?;

    let is_pattern = typecheck_pattern_match(&if_cond, &cond, ctx)?;

    let mut then_ctx = create_then_branch_ctx(is_pattern.as_ref(), ctx)?;

    let then_branch = typecheck_stmt(&if_cond.then_branch, expect_ty, &mut then_ctx)?;
    let else_branch = match &if_cond.else_branch {
        Some(else_expr) => {
            let mut else_ctx = ctx.clone();
            let else_stmt = typecheck_stmt(else_expr, &Type::Nothing, &mut else_ctx)?;

            ctx.consolidate_branches(&[then_ctx, else_ctx]);
            Some(else_stmt)
        },

        None => {
            ctx.consolidate_branches(&[then_ctx]);
            None
        },
    };

    let annotation = Typed::Untyped(if_cond.span().clone());

    Ok(IfCond {
        cond,
        is_pattern,
        then_branch,
        else_branch,
        annotation,
    })
}

pub fn typecheck_if_cond_expr(
    if_cond: &ast::IfCond<Span, ast::Expr<Span>>,
    expect_ty: &Type,
    ctx: &mut Context,
) -> TypecheckResult<IfCond<Expr>> {
    let cond = typecheck_cond_expr(&if_cond, ctx)?;

    let is_pattern = typecheck_pattern_match(&if_cond, &cond, ctx)?;

    let mut then_ctx = create_then_branch_ctx(is_pattern.as_ref(), ctx)?;

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
        },

        None => {
            ctx.consolidate_branches(&[then_ctx]);
            None
        },
    };

    let span = if_cond.span().clone();

    let annotation = match (then_branch.annotation().ty(), else_branch.as_ref()) {
        (Cow::Owned(Type::Nothing) | Cow::Borrowed(Type::Nothing), _) | (_, None) => {
            Typed::Untyped(span)
        },

        (then_ty, Some(_else_branch)) => TypedValue {
            ty: then_ty.into_owned(),
            value_kind: ValueKind::Temporary,
            span,
            decl: None,
        }
        .into(),
    };

    Ok(IfCond {
        cond,
        is_pattern,
        then_branch,
        else_branch,
        annotation,
    })
}
