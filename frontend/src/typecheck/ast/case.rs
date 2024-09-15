use crate::typecheck::ast::typecheck_expr;
use crate::typecheck::ast::typecheck_stmt;
use crate::typecheck::TypedValue;
use crate::typecheck::TypecheckResult;
use crate::typecheck::TypecheckError;
use crate::typecheck::Typed;
use crate::typecheck::Type;
use crate::typecheck::Context;
use crate::typecheck::ValueKind;
use common::span::{Span, Spanned};
use crate::ast;

pub type CaseBranch<Item> = ast::CaseBranch<Typed, Item>;
pub type CaseBlock<Item> = ast::CaseBlock<Typed, Item>;
pub type CaseExpr = ast::CaseExpr<Typed>;
pub type CaseStmt = ast::CaseStmt<Typed>;

pub fn typecheck_case_stmt(
    case: &ast::CaseStmt<Span>,
    expect_ty: &Type,
    ctx: &mut Context,
) -> TypecheckResult<CaseStmt> {
    let cond_expr = typecheck_expr(&case.cond_expr, &Type::Nothing, ctx)?;
    let cond_ty = cond_expr.annotation().ty();

    let mut branches = Vec::new();
    for branch in &case.branches {
        let branch_value = typecheck_expr(&branch.value, &cond_ty, ctx)?;
        branch_value.annotation().expect_value(&cond_ty)?;

        let branch_stmt = typecheck_stmt(&branch.item, &expect_ty, ctx)?;

        branches.push(ast::CaseBranch {
            value: Box::new(branch_value),
            item: Box::new(branch_stmt),
            span: branch.span.clone(),
        })
    }

    let else_branch = match &case.else_branch {
        Some(else_branch) => {
            let else_stmt = typecheck_stmt(else_branch, &expect_ty, ctx)?;
            Some(else_stmt)
        },

        None => None,
    };

    let annotation = Typed::Untyped(case.span().clone());

    Ok(CaseStmt {
        cond_expr: Box::new(cond_expr),
        branches,
        else_branch: else_branch.map(Box::new),
        annotation,
    })
}

pub fn typecheck_case_expr(
    case: &ast::CaseExpr<Span>,
    expect_ty: &Type,
    ctx: &mut Context,
) -> TypecheckResult<CaseExpr> {
    let span = case.span().clone();

    if case.branches.is_empty() || case.else_branch.is_none() {
        return Err(TypecheckError::InvalidCaseExprBlock { span });
    }

    let cond_expr = typecheck_expr(&case.cond_expr, &Type::Nothing, ctx)?;
    let cond_ty = cond_expr.annotation().ty();
    let mut branches = Vec::new();

    if case.branches.len() == 0 || case.else_branch.is_none() {
        return Err(TypecheckError::InvalidCaseExprBlock { span });
    }

    let mut branch_expr_ty = None;

    for branch in &case.branches {
        let branch_value = typecheck_expr(&branch.value, &cond_ty, ctx)?;
        branch_value.annotation().expect_value(&cond_ty)?;

        let branch_expr = typecheck_expr(&branch.item, &expect_ty, ctx)?;

        if let Some(branch_expr_ty) = &branch_expr_ty {
            branch_expr.annotation().expect_value(branch_expr_ty)?;
        } else {
            branch_expr_ty = Some(branch_expr.annotation().ty().into_owned())
        }

        branches.push(ast::CaseBranch {
            value: Box::new(branch_value),
            item: Box::new(branch_expr),
            span: branch.span.clone(),
        })
    }

    let result_ty = match branch_expr_ty {
        Some(Type::Nothing) | None => {
            return Err(TypecheckError::InvalidCaseExprBlock { span });
        },

        Some(branch_expr_ty) => branch_expr_ty,
    };

    let else_expr = match &case.else_branch {
        Some(else_branch) => typecheck_expr(else_branch, &result_ty, ctx)?,

        _ => {
            return Err(TypecheckError::InvalidCaseExprBlock { span });
        },
    };

    let annotation = TypedValue {
        span,
        decl: None,
        value_kind: ValueKind::Temporary,
        ty: result_ty,
    }
    .into();

    Ok(CaseExpr {
        cond_expr: Box::new(cond_expr),
        branches,
        else_branch: Some(Box::new(else_expr)),
        annotation,
    })
}
