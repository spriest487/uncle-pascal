use crate::ast::prelude::*;

pub type CaseBranch<Item> = ast::CaseBranch<TypeAnnotation, Item>;
pub type CaseBlock<Item> = ast::CaseBlock<TypeAnnotation, Item>;
pub type CaseExpr = ast::CaseExpr<TypeAnnotation>;
pub type CaseStatement = ast::CaseStatement<TypeAnnotation>;

pub fn typecheck_case_stmt(case: &ast::CaseStatement<Span>, ctx: &mut Context) -> TypecheckResult<CaseStatement> {
    let cond_expr = typecheck_expr(&case.cond_expr, &Type::Nothing, ctx)?;
    let cond_ty = cond_expr.annotation().ty();

    let mut branches = Vec::new();
    for branch in &case.branches {
        let branch_value = typecheck_expr(&branch.value, cond_ty, ctx)?;
        branch_value.annotation().expect_value(cond_ty)?;

        let branch_stmt = typecheck_stmt(&branch.item, &Type::Nothing, ctx)?;

        branches.push(ast::CaseBranch {
            value: Box::new(branch_value),
            item: Box::new(branch_stmt),
            span: branch.span.clone(),
        })
    }

    let else_branch = match &case.else_branch {
        Some(else_branch) => {
            let else_stmt = typecheck_stmt(else_branch, &Type::Nothing, ctx)?;
            Some(else_stmt)
        }

        None => None,
    };

    let annotation = TypeAnnotation::Untyped(case.span().clone());

    Ok(CaseStatement {
        cond_expr: Box::new(cond_expr),
        branches,
        else_branch: else_branch.map(Box::new),
        annotation,
    })
}

fn invalid_item_err(item_stmt: Statement) -> TypecheckError {
    TypecheckError::InvalidCaseExprBlock {
        span: item_stmt.span().clone(),
    }
}

pub fn case_stmt_into_expr(case: CaseStatement) -> TypecheckResult<CaseExpr> {
    let span = case.span().clone();

    if case.branches.is_empty() || case.else_branch.is_none() {
        return Err(TypecheckError::InvalidCaseExprBlock { span });
    }

    let else_stmt = if let Some(else_item) = case.else_branch {
        else_item
    } else {
        return Err(TypecheckError::InvalidCaseExprBlock { span });
    };

    let mut branches = Vec::new();
    let mut branch_expr_ty = None;

    for branch in case.branches {
        let branch_expr = branch.item.try_into_expr().map_err(invalid_item_err)?;

        if let Some(branch_ty) = &branch_expr_ty {
            branch_expr.annotation().expect_value(branch_ty)?;
        } else {
            branch_expr_ty = Some(branch_expr.annotation().ty().clone());
        }

        branches.push(CaseBranch {
            value: branch.value,
            item: Box::new(branch_expr),
            span: branch.span,
        })
    }

    let result_ty = match branch_expr_ty {
        Some(branch_expr_ty) => branch_expr_ty,
        None => {
            return Err(TypecheckError::InvalidCaseExprBlock { span });
        }
    };

    let else_expr = else_stmt.try_into_expr().map_err(invalid_item_err)?;
    else_expr.annotation().expect_value(&result_ty)?;

    let annotation = TypedValueAnnotation {
        span,
        decl: None,
        value_kind: ValueKind::Temporary,
        ty: result_ty,
    }.into();

    Ok(CaseExpr {
        cond_expr: case.cond_expr,
        branches,
        else_branch: Some(Box::new(else_expr)),
        annotation,
    })
}

pub fn typecheck_case_expr(case: &ast::CaseExpr<Span>, expect_ty: &Type, ctx: &mut Context) -> TypecheckResult<CaseExpr> {
    let span = case.span().clone();

    if case.branches.is_empty() || case.else_branch.is_none() {
        return Err(TypecheckError::InvalidCaseExprBlock { span });
    }

    let cond_expr = typecheck_expr(&case.cond_expr, &Type::Nothing, ctx)?;
    let cond_ty = cond_expr.annotation().ty();
    let mut branches = Vec::new();

    if case.branches.len() == 0 || case.else_branch.is_none() {
        return Err(TypecheckError::InvalidCaseExprBlock {
            span
        })
    }

    let mut branch_expr_ty = None;

    for branch in &case.branches {
        let branch_value = typecheck_expr(&branch.value, cond_ty, ctx)?;
        branch_value.annotation().expect_value(cond_ty)?;

        let branch_expr = typecheck_expr(&branch.item, &expect_ty, ctx)?;

        if let Some(branch_expr_ty) = &branch_expr_ty {
            branch_expr.annotation().expect_value(branch_expr_ty)?;
        } else {
            branch_expr_ty = Some(branch_expr.annotation().ty().clone())
        }

        branches.push(ast::CaseBranch {
            value: Box::new(branch_value),
            item: Box::new(branch_expr),
            span: branch.span.clone(),
        })
    }

    let result_ty = match branch_expr_ty {
        Some(branch_expr_ty) => branch_expr_ty,
        None => {
            return Err(TypecheckError::InvalidCaseExprBlock { span });
        }
    };


    let else_expr = match &case.else_branch {
        Some(else_branch) => {
            typecheck_expr(else_branch, &result_ty, ctx)?
        }

        _ => {
            return Err(TypecheckError::InvalidCaseExprBlock { span });
        }
    };

    let annotation = TypedValueAnnotation {
        span,
        decl: None,
        value_kind: ValueKind::Temporary,
        ty: result_ty,
    }.into();

    Ok(CaseExpr {
        cond_expr: Box::new(cond_expr),
        branches,
        else_branch: Some(Box::new(else_expr)),
        annotation,
    })
}
