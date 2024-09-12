use crate::ast::{typecheck_expr, Expr};
use crate::{Context, Type, Typed, TypecheckError, TypecheckResult};
use pas_common::span::{Span, Spanned};
use pas_syn::{ast, Operator};
use crate::ast::cast::implicit_conversion;

pub type Assignment = ast::Assignment<Typed>;
pub type CompoundAssignment = ast::CompoundAssignment<Typed>;

pub fn typecheck_assignment(
    assignment: &ast::Assignment<Span>,
    ctx: &mut Context,
) -> TypecheckResult<Assignment> {
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
) -> TypecheckResult<CompoundAssignment> {
    let (lhs, rhs) = typecheck_operands(&assignment.lhs, &assignment.rhs, ctx)?;

    if !lhs.annotation().ty().valid_math_op(assignment.op.binary_operator(), &rhs.annotation().ty()) {
        return Err(TypecheckError::InvalidBinOp {
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
) -> TypecheckResult<(Expr, Expr)> {
    let lhs = typecheck_expr(&src_lhs, &Type::Nothing, ctx)?;

    // lhs must evaluate to a mutable typed value
    match lhs.annotation() {
        Typed::TypedValue(val) => {
            if !val.value_kind.mutable() {
                return Err(TypecheckError::NotMutable {
                    decl: val.decl.clone(),
                    expr: Box::new(lhs),
                });
            }
        }
        _ => {
            return Err(TypecheckError::NotMutable {
                expr: Box::new(lhs),
                decl: None,
            });
        }
    }

    let lhs_ty = lhs.annotation().ty();

    let rhs = typecheck_expr(&src_rhs, &lhs_ty, ctx)?;
    let rhs = implicit_conversion(rhs, &lhs_ty, ctx)
        .map_err(|err| match err {
            TypecheckError::TypeMismatch {
                expected,
                actual,
                span,
            } => TypecheckError::InvalidBinOp {
                lhs: expected,
                rhs: actual,
                op: Operator::Assignment,
                span,
            },

            err => err,
        })?;

    Ok((lhs, rhs))
}
