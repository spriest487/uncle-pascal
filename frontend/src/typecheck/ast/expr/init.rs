use common::span::Spanned;
use crate::ast;
use crate::ast::Ident;
use crate::ast::FunctionParamMod;
use crate::typecheck::ast::Stmt;
use crate::typecheck::ast::MatchStmt;
use crate::typecheck::ast::MatchExpr;
use crate::typecheck::ast::IfCond;
use crate::typecheck::ast::Expr;
use crate::typecheck::ast::CaseStmt;
use crate::typecheck::ast::CaseExpr;
use crate::typecheck::ast::Call;
use crate::typecheck::ast::Block;
use crate::typecheck::ast::VarBinding;
use crate::typecheck::Context;
use crate::typecheck::FunctionParamSig;
use crate::typecheck::Type;
use crate::typecheck::TypecheckError;
use crate::typecheck::TypecheckResult;
use crate::typecheck::Typed;
use crate::typecheck::ValueKind;

pub fn expect_stmt_initialized(stmt: &Stmt, ctx: &Context) -> TypecheckResult<()> {
    match stmt {
        ast::Stmt::Ident(ident, annotation) => expect_ident_initialized(ident, annotation, ctx),

        ast::Stmt::Call(call) => expect_call_initialized(call, ctx),

        ast::Stmt::If(if_stmt) => expect_if_stmt_initialized(if_stmt, ctx),

        ast::Stmt::Block(block) => expect_block_initialized(block, ctx),

        ast::Stmt::LocalBinding(binding) => expect_binding_initialized(binding, ctx),

        ast::Stmt::ForLoop(for_loop) => {
            match &for_loop.init {
                ast::ForLoopInit::Binding(init_binding) => {
                    expect_binding_initialized(init_binding, ctx)?
                },
                ast::ForLoopInit::Assignment { counter: _, value } => {
                    // only the initial value needs to be initialized - we (re)initialize the counter in the loop
                    expect_expr_initialized(value, ctx)?;
                },
            }
            expect_expr_initialized(&for_loop.to_expr, ctx)?;
            expect_stmt_initialized(&for_loop.body, ctx)?;
            Ok(())
        },

        ast::Stmt::WhileLoop(while_loop) => {
            expect_expr_initialized(&while_loop.condition, ctx)?;
            expect_stmt_initialized(&while_loop.body, ctx)?;
            Ok(())
        },

        ast::Stmt::Exit(exit) => match exit.as_ref() {
            ast::Exit::WithoutValue(_) => Ok(()),
            ast::Exit::WithValue(exit_val, ..) => expect_expr_initialized(exit_val, ctx),
        },

        ast::Stmt::Assignment(assignment) => expect_expr_initialized(&assignment.rhs, ctx),

        ast::Stmt::CompoundAssignment(assignment) => {
            expect_expr_initialized(&assignment.lhs, ctx)?;
            expect_expr_initialized(&assignment.rhs, ctx)?;
            Ok(())
        },

        ast::Stmt::Break(..) | ast::Stmt::Continue(..) => Ok(()),

        ast::Stmt::Raise(raise) => expect_expr_initialized(&raise.value, ctx),

        ast::Stmt::Case(case) => expect_case_stmt_initialized(case, ctx),

        ast::Stmt::Match(match_stmt) => expect_match_stmt_initialized(match_stmt, ctx),
    }
}

fn expect_ident_initialized(
    ident: &Ident,
    annotation: &Typed,
    ctx: &Context,
) -> TypecheckResult<()> {
    match annotation.value_kind() {
        Some(ValueKind::Uninitialized) => {
            let decl_ident = ctx.find_decl(ident).unwrap_or(ident);
            Err(TypecheckError::NotInitialized {
                ident: decl_ident.clone(),
                usage: ident.span().clone(),
            })
        },

        _ => Ok(()),
    }
}

pub fn expect_expr_initialized(expr: &Expr, ctx: &Context) -> TypecheckResult<()> {
    match expr {
        ast::Expr::Ident(ident, annotation) => expect_ident_initialized(ident, annotation, ctx),

        ast::Expr::Literal(..) => Ok(()),

        ast::Expr::Block(block) => expect_block_initialized(block, ctx),

        ast::Expr::IfCond(cond) => expect_if_expr_initialized(cond, ctx),

        ast::Expr::ObjectCtor(ctor) => {
            for member in &ctor.args.members {
                expect_expr_initialized(&member.value, ctx)?;
            }
            Ok(())
        },

        ast::Expr::CollectionCtor(ctor) => {
            for el in &ctor.elements {
                expect_expr_initialized(&el.value, ctx)?;
            }
            Ok(())
        },

        ast::Expr::Call(call) => expect_call_initialized(call, ctx),

        ast::Expr::BinOp(bin_op) => {
            expect_expr_initialized(&bin_op.lhs, ctx)?;
            expect_expr_initialized(&bin_op.rhs, ctx)?;
            Ok(())
        },

        ast::Expr::UnaryOp(unary_op) => expect_expr_initialized(&unary_op.operand, ctx),

        ast::Expr::Raise(raise) => expect_expr_initialized(&raise.value, ctx),

        ast::Expr::Case(case) => expect_case_expr_initialized(&case, ctx),

        ast::Expr::Match(match_expr) => expect_match_expr_initialized(&match_expr, ctx),

        ast::Expr::Exit(exit) => match exit.as_ref() {
            ast::Exit::WithValue(exit_val, _) => expect_expr_initialized(exit_val, ctx),
            ast::Exit::WithoutValue(_) => Ok(()),
        },

        ast::Expr::Cast(cast) => expect_expr_initialized(&cast.expr, ctx),

        ast::Expr::AnonymousFunction(_) => Ok(()),
    }?;

    Ok(())
}

fn expect_binding_initialized(binding: &VarBinding, ctx: &Context) -> TypecheckResult<()> {
    if let Some(init_val) = &binding.val {
        expect_expr_initialized(init_val, ctx)?;
    }
    Ok(())
}

fn expect_args_initialized(
    params: &[FunctionParamSig],
    args: &[Expr],
    ctx: &Context,
) -> TypecheckResult<()> {
    assert_eq!(
        params.len(),
        args.len(),
        "function call with wrong number of args shouldn't pass type checking. got:\n{}\nexpected:\n{}",
        args.iter().map(Expr::to_string).collect::<Vec<_>>().join("; "),
        params.iter().map(|param| param.ty.to_string()).collect::<Vec<_>>().join("; "),
    );

    for (arg, param) in args.iter().zip(params.iter()) {
        if param.modifier != Some(FunctionParamMod::Out) {
            expect_expr_initialized(arg, ctx)?;
        }
    }

    Ok(())
}

fn expect_call_initialized(call: &Call, ctx: &Context) -> TypecheckResult<()> {
    match call {
        ast::Call::FunctionNoArgs(call) => {
            expect_expr_initialized(&call.target, ctx)?;
        }

        ast::Call::MethodNoArgs(call) => {
            expect_expr_initialized(&call.target, ctx)?;
            expect_expr_initialized(&call.self_arg, ctx)?;
        }

        ast::Call::Function(func_call) => {
            expect_expr_initialized(&func_call.target, ctx)?;

            let target_ty = func_call.target.annotation().ty();

            let params = match target_ty.as_ref() {
                Type::Function(sig) => &sig.params,
                _ => panic!(
                    "function call with wrong target type shouldn't pass type checking (was: {:#?})",
                    func_call.target.annotation().ty()
                ),
            };
            expect_args_initialized(params, &func_call.args, ctx)?;
        },

        ast::Call::Method(method_call) => {
            let params = match &method_call.func_type {
                Type::Function(sig) => &sig.params,
                _ => panic!(
                    "function call with wrong target type shouldn't pass type checking (was: {:#?})",
                    method_call.func_type,
                ),
            };

            expect_args_initialized(params, &method_call.args, ctx)?;
        },

        ast::Call::VariantCtor(ctor_call) => {
            if let Some(arg_expr) = &ctor_call.arg {
                expect_expr_initialized(&arg_expr, ctx)?;
            }
        },
    }

    Ok(())
}

fn expect_if_expr_initialized(if_stmt: &IfCond<Expr>, ctx: &Context) -> TypecheckResult<()> {
    expect_expr_initialized(&if_stmt.cond, ctx)?;
    expect_expr_initialized(&if_stmt.then_branch, ctx)?;
    if let Some(else_branch) = &if_stmt.else_branch {
        expect_expr_initialized(else_branch, ctx)?;
    }
    Ok(())
}

fn expect_if_stmt_initialized(if_stmt: &IfCond<Stmt>, ctx: &Context) -> TypecheckResult<()> {
    expect_expr_initialized(&if_stmt.cond, ctx)?;
    expect_stmt_initialized(&if_stmt.then_branch, ctx)?;
    if let Some(else_branch) = &if_stmt.else_branch {
        expect_stmt_initialized(else_branch, ctx)?;
    }
    Ok(())
}

fn expect_block_initialized(block: &Block, ctx: &Context) -> TypecheckResult<()> {
    for stmt in &block.stmts {
        expect_stmt_initialized(stmt, ctx)?;
    }
    if let Some(output) = &block.output {
        expect_expr_initialized(output, ctx)?;
    }
    Ok(())
}

fn expect_case_stmt_initialized(case: &CaseStmt, ctx: &Context) -> TypecheckResult<()> {
    expect_expr_initialized(&case.cond_expr, ctx)?;

    for branch in &case.branches {
        expect_expr_initialized(&branch.value, ctx)?;
        expect_stmt_initialized(&branch.item, ctx)?;
    }

    if let Some(else_branch) = &case.else_branch {
        expect_stmt_initialized(else_branch, ctx)?;
    }

    Ok(())
}

fn expect_case_expr_initialized(case: &CaseExpr, ctx: &Context) -> TypecheckResult<()> {
    expect_expr_initialized(&case.cond_expr, ctx)?;

    for branch in &case.branches {
        expect_expr_initialized(&branch.value, ctx)?;
        expect_expr_initialized(&branch.item, ctx)?;
    }

    if let Some(else_branch) = &case.else_branch {
        expect_expr_initialized(else_branch, ctx)?;
    }

    Ok(())
}

fn expect_match_expr_initialized(match_expr: &MatchExpr, ctx: &Context) -> TypecheckResult<()> {
    expect_expr_initialized(&match_expr.cond_expr, ctx)?;

    for branch in &match_expr.branches {
        expect_expr_initialized(&branch.item, ctx)?;
    }

    if let Some(else_branch) = &match_expr.else_branch {
        expect_expr_initialized(else_branch, ctx)?;
    }

    Ok(())
}

fn expect_match_stmt_initialized(match_stmt: &MatchStmt, ctx: &Context) -> TypecheckResult<()> {
    expect_expr_initialized(&match_stmt.cond_expr, ctx)?;

    for branch in &match_stmt.branches {
        expect_stmt_initialized(&branch.item, ctx)?;
    }

    if let Some(else_branch) = &match_stmt.else_branch {
        expect_stmt_initialized(else_branch, ctx)?;
    }

    Ok(())
}