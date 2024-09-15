use crate::typecheck::ast::typecheck_expr;
use crate::typecheck::ast::typecheck_stmt;
use crate::typecheck::ast::Expr;
use crate::typecheck::ast::implicit_conversion;
use crate::typecheck::ast::Stmt;
use crate::typecheck::Binding;
use crate::typecheck::Context;
use crate::typecheck::Environment;
use crate::typecheck::Type;
use crate::typecheck::Typed;
use crate::typecheck::TypePattern;
use crate::typecheck::TypecheckError;
use crate::typecheck::TypecheckResult;
use crate::typecheck::ValueKind;
use crate::typecheck::TypedValue;
use common::span::Span;
use common::span::Spanned;
use crate::ast;

pub type MatchBlock<B> = ast::MatchBlock<Typed, B>;
pub type MatchExpr = MatchBlock<Expr>;
pub type MatchStmt = MatchBlock<Stmt>;
pub type MatchBlockBranch<B> = ast::MatchBlockBranch<Typed, B>;

fn typecheck_match_cond<B>(
    match_block: &ast::MatchBlock<Span, B>,
    ctx: &mut Context,
) -> TypecheckResult<Expr> {
    let cond_expr = typecheck_expr(&match_block.cond_expr, &Type::Nothing, ctx)?;

    let cond_ty = cond_expr.annotation().ty();
    if !cond_ty.is_matchable() {
        return Err(TypecheckError::NotMatchable {
            ty: cond_ty.into_owned(),
            span: cond_expr.span().clone(),
        });
    }

    Ok(cond_expr)
}

fn typecheck_match_branches<BSrc, B, ItemCheck>(
    match_block: &ast::MatchBlock<Span, BSrc>,
    cond_ty: &Type,
    expect_ty: &Type,
    ctx: &mut Context,
    check_item: ItemCheck,
) -> TypecheckResult<Vec<MatchBlockBranch<B>>>
where
    ItemCheck: Fn(&BSrc, &Type, &[MatchBlockBranch<B>], &mut Context) -> TypecheckResult<B>,
    BSrc: Spanned,
{
    if match_block.branches.is_empty() {
        return Err(TypecheckError::EmptyMatchBlock {
            span: match_block.span().clone(),
        })
    }

    let mut branches = Vec::new();
    let mut branch_ctxs = Vec::new();

    for branch in &match_block.branches {
        let branch_ctx = ctx.clone();

        let branch_env = Environment::Block {
            allow_unsafe: false,
        };

        let branch = ctx.scope(branch_env, |branch_ctx| {
            let pattern = TypePattern::typecheck(&branch.pattern, cond_ty, branch_ctx)?;
            let bindings = pattern.bindings(branch_ctx)
                .map_err(|err| TypecheckError::from_name_err(err, pattern.span().clone()))?;

            for binding in bindings {
                branch_ctx.declare_binding(
                    binding.ident.clone(),
                    Binding {
                        ty: binding.ty,
                        def: Some(binding.ident),
                        kind: ValueKind::Temporary,
                    },
                )?;
            }

            let item = check_item(&branch.item, expect_ty, &branches, branch_ctx)?;

            Ok(MatchBlockBranch {
                item,
                pattern,
                span: branch.span().clone(),
            })
        })?;

        branches.push(branch);
        branch_ctxs.push(branch_ctx);
    }

    ctx.consolidate_branches(&branch_ctxs);

    Ok(branches)
}

pub fn typecheck_match_stmt(
    match_stmt: &ast::MatchStmt<Span>,
    expect_ty: &Type,
    ctx: &mut Context,
) -> TypecheckResult<MatchStmt> {
    let block_env = Environment::Block {
        allow_unsafe: false,
    };

    ctx.scope(block_env, |block_ctx| {
        let cond_expr = typecheck_match_cond(&match_stmt, block_ctx)?;

        let branches = typecheck_match_branches(
            &match_stmt,
            &cond_expr.annotation().ty(),
            expect_ty,
            block_ctx,
            |item, expect_ty, _branches, ctx| typecheck_stmt(item, expect_ty, ctx),
        )?;

        let else_branch = match &match_stmt.else_branch {
            Some(else_stmt) => {
                let else_stmt = typecheck_stmt(else_stmt, expect_ty, block_ctx)?;
                Some(else_stmt)
            },
            None => None,
        };

        let annotation = Typed::Untyped(match_stmt.span().clone());

        Ok(MatchStmt {
            cond_expr,
            annotation,
            branches,
            else_branch,
        })
    })
}

pub fn typecheck_match_expr(
    match_expr: &ast::MatchExpr<Span>,
    expect_ty: &Type,
    ctx: &mut Context,
) -> TypecheckResult<MatchExpr> {
    let block_env = Environment::Block {
        allow_unsafe: false,
    };

    ctx.scope(block_env, |block_ctx| {
        let cond_expr = typecheck_match_cond(&match_expr, block_ctx)?;

        let branches = typecheck_match_branches(
            &match_expr,
            &cond_expr.annotation().ty(),
            expect_ty,
            block_ctx,
            |item_expr, expect_ty, branches: &[MatchBlockBranch<Expr>], ctx| {
                if branches.len() > 0 {
                    let result_ty = branches[0].item.annotation().ty();
                    let item_expr = typecheck_expr(item_expr, &result_ty, ctx)?;
                    implicit_conversion(item_expr, &result_ty, ctx)
                } else {
                    typecheck_expr(item_expr, expect_ty, ctx)
                }
            },
        )?;

        let result_ty = branches[0].item.annotation().ty().into_owned();

        let else_branch = match &match_expr.else_branch {
            Some(else_stmt) => {
                let else_stmt = typecheck_expr(else_stmt, expect_ty, block_ctx)?;
                Some(else_stmt)
            },
            None => None,
        };

        if else_branch.is_none() {
            let mut missing_cases = Vec::new();
            let is_exhaustive = match cond_expr.annotation().ty().as_ref() {
                Type::Any | Type::Interface(..) => {
                    // matches on dynamic RC types can never be exhaustive
                    false
                }

                Type::Variant(var_sym) => {
                    let variant_def = block_ctx.find_variant_def(&var_sym.qualified)
                        .map_err(|err| TypecheckError::from_name_err(err, match_expr.span().clone()))?;

                    // add all variants and remove the ones mentioned by any variant pattern, or
                    // NOT mentioned by any negated variant pattern
                    missing_cases.reserve(variant_def.cases.len());

                    for def_case in &variant_def.cases {
                        let is_mentioned = branches.iter().any(|branch| match &branch.pattern {
                            TypePattern::VariantCase { case, .. } => *case == def_case.ident,
                            TypePattern::NegatedVariantCase { case, .. } => *case != def_case.ident,
                            _ => false,
                        });

                        if !is_mentioned {
                            missing_cases.push(def_case.ident.clone());
                        }
                    }

                    missing_cases.is_empty()
                }

                _ =>  true,
            };

            if !is_exhaustive {
                return Err(TypecheckError::MatchExprNotExhaustive {
                    span: match_expr.span().clone(),
                    missing_cases,
                })
            }
        }

        let annotation = TypedValue {
            ty: result_ty,
            span: match_expr.span().clone(),
            decl: None,
            value_kind: ValueKind::Temporary,
        }.into();

        Ok(MatchExpr {
            cond_expr,
            annotation,
            branches,
            else_branch,
        })
    })
}
