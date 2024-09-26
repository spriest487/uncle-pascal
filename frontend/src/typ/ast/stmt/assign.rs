use crate::typ::ast::cast::implicit_conversion;
use crate::typ::ast::typecheck_expr;
use crate::typ::ast::Expr;
use crate::typ::Context;
use crate::typ::Type;
use crate::typ::TypeError;
use crate::typ::TypeResult;
use crate::typ::Typed;
use common::span::Span;
use common::span::Spanned;
use crate::ast;
use crate::Operator;

pub type Assignment = ast::Assignment<Typed>;
pub type CompoundAssignment = ast::CompoundAssignment<Typed>;

pub fn typecheck_assignment(
    assignment: &ast::Assignment<Span>,
    ctx: &mut Context,
) -> TypeResult<Assignment> {
    let (lhs, rhs) = typecheck_operands(&assignment.lhs, &assignment.rhs, ctx)?;

    if let ast::Expr::Ident(ident, ..) = &lhs {
        if ctx.get_decl_scope(ident).is_some() {
            ctx.initialize(ident)
        }
    }

    Ok(Assignment {
        lhs,
        rhs,
        annotation: Typed::Untyped(assignment.annotation.clone()),
    })
}

pub fn typecheck_compound_assignment(
    assignment: &ast::CompoundAssignment<Span>,
    ctx: &mut Context,
) -> TypeResult<CompoundAssignment> {
    let (lhs, rhs) = typecheck_operands(&assignment.lhs, &assignment.rhs, ctx)?;

    if !lhs.annotation().ty().valid_math_op(assignment.op.binary_operator(), &rhs.annotation().ty()) {
        return Err(TypeError::InvalidBinOp {
            lhs: lhs.annotation().ty().into_owned(),
            rhs: rhs.annotation().ty().into_owned(),
            op: assignment.op.binary_operator(),
            span: assignment.span().clone(),
        })
    }

    Ok(CompoundAssignment {
        lhs,
        rhs,
        annotation: Typed::Untyped(assignment.annotation.clone()),
        op: assignment.op,
    })
}

fn typecheck_operands(
    src_lhs: &ast::Expr<Span>,
    src_rhs: &ast::Expr<Span>,
    ctx: &mut Context,
) -> TypeResult<(Expr, Expr)> {
    let lhs = typecheck_expr(&src_lhs, &Type::Nothing, ctx)?;

    // lhs must evaluate to a mutable typed value
    match lhs.annotation() {
        Typed::TypedValue(val) => {
            if !val.value_kind.mutable() {
                return Err(TypeError::NotMutable {
                    decl: val.decl.clone(),
                    expr: Box::new(lhs),
                });
            }
        }
        _ => {
            return Err(TypeError::NotMutable {
                expr: Box::new(lhs),
                decl: None,
            });
        }
    }

    let lhs_ty = lhs.annotation().ty();

    let rhs = typecheck_expr(&src_rhs, &lhs_ty, ctx)?;
    let rhs = implicit_conversion(rhs, &lhs_ty, ctx)
        .map_err(|err| match err {
            TypeError::TypeMismatch {
                expected,
                actual,
                span,
            } => TypeError::InvalidBinOp {
                lhs: expected,
                rhs: actual,
                op: Operator::Assignment,
                span,
            },

            err => err,
        })?;

    Ok((lhs, rhs))
}
