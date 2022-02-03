use crate::ast::{typecheck_expr, Expression};
use crate::{Context, Type, TypeAnnotation, TypecheckError, TypecheckResult};
use pas_common::span::{Span, Spanned};
use pas_syn::{ast, Operator};

pub type Assignment = ast::Assignment<TypeAnnotation>;
pub type CompoundAssignment = ast::CompoundAssignment<TypeAnnotation>;

pub fn typecheck_assignment(
    assignment: &ast::Assignment<Span>,
    ctx: &mut Context,
) -> TypecheckResult<Assignment> {
    let (lhs, rhs) = typecheck_operands(&assignment.lhs, &assignment.rhs, assignment.span(), ctx)?;

    if let ast::Expression::Ident(ident, ..) = &lhs {
        if ctx.is_local(ident) {
            ctx.initialize(ident);
        }
    }

    Ok(Assignment {
        lhs,
        rhs,
        annotation: TypeAnnotation::Untyped(assignment.annotation.clone()),
    })
}

pub fn typecheck_compound_assignment(
    assignment: &ast::CompoundAssignment<Span>,
    ctx: &mut Context,
) -> TypecheckResult<CompoundAssignment> {
    let (lhs, rhs) = typecheck_operands(&assignment.lhs, &assignment.rhs, assignment.span(), ctx)?;

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
        annotation: TypeAnnotation::Untyped(assignment.annotation.clone()),
        op: assignment.op,
    })
}

fn typecheck_operands(
    src_lhs: &ast::Expression<Span>,
    src_rhs: &ast::Expression<Span>,
    span: &Span,
    ctx: &mut Context,
) -> TypecheckResult<(Expression, Expression)> {
    let lhs = typecheck_expr(&src_lhs, &Type::Nothing, ctx)?;

    // lhs must evaluate to a mutable typed value
    match lhs.annotation() {
        TypeAnnotation::TypedValue(val) => {
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

    let rhs = typecheck_expr(&src_rhs, &lhs.annotation().ty(), ctx)?;
    let rhs_ty = rhs.annotation().ty();

    lhs.annotation()
        .ty()
        .implicit_conversion_from(&rhs_ty, span, ctx)
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
