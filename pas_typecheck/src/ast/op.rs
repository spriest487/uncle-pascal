use {
    pas_syn::{
        Operator,
    },
    crate::ast::prelude::*,
};

pub type BinOp = ast::BinOp<TypeAnnotation>;

pub fn typecheck_bin_op(
    bin_op: &ast::BinOp<Span>,
    ctx: &mut Context)
    -> TypecheckResult<BinOp>
{
    let lhs = typecheck_expr(&bin_op.lhs, &Type::Nothing, ctx)?;
    let span = bin_op.annotation.clone();

    match &bin_op.op {
        Operator::Member => {
            // rhs of an ident op must be an identifier (parser checks this)
            let member_ident = bin_op.rhs.expr.as_ident()
                .cloned()
                .expect("bin-op with member operator should always have an ident on the rhs");

            let base_ty = &lhs.annotation.ty;
            let member_ty = base_ty.find_member(&member_ident)
                .cloned()
                .ok_or_else(|| TypecheckError::MemberNotFound {
                    base: lhs.annotation.ty.clone(),
                    member: member_ident.clone(),
                    span: span.clone(),
                })?;

            let annotation = TypeAnnotation {
                value_kind: lhs.annotation.value_kind.clone(),
                ty: member_ty,
                span,
            };

            let rhs = ast::Expression::Ident(member_ident);

            Ok(BinOp {
                lhs,
                op: Operator::Member,
                rhs: ExpressionNode::new(rhs, annotation.clone()),
                annotation,
            })
        },

        _ => {
            let rhs = typecheck_expr(&bin_op.rhs, &Type::Nothing, ctx)?;

            // check valid ops etc, result type etc
            let result_ty = lhs.annotation.ty.clone();

            let annotation = match result_ty {
                Type::Nothing => TypeAnnotation::untyped(span),
                ty => TypeAnnotation::typed_value(ty, ValueKind::Temporary, span),
            };

            Ok(ast::BinOp {
                lhs,
                op: bin_op.op,
                rhs,
                annotation
            })
        },
    }
}

pub type UnaryOp = ast::UnaryOp<TypeAnnotation>;

pub fn typecheck_unary_op(
    unary_op: &ast::UnaryOp<Span>,
    ctx: &mut Context)
    -> TypecheckResult<UnaryOp>
{
    let span = unary_op.span().clone();
    let operand = typecheck_expr(&unary_op.operand, &Type::Nothing, ctx)?;

    let annotation = match unary_op.op {
        Operator::AddressOf => {
            let addr_ty = match (&operand.annotation.ty, operand.annotation.value_kind) {
                (Type::Pointer(_), Some(ValueKind::Mutable)) |
                (Type::Record(_), Some(ValueKind::Mutable)) |
                (Type::Primitive(_), Some(ValueKind::Mutable)) => operand.annotation.ty.clone().ptr(),

                (ty, kind) => return Err(TypecheckError::NotAddressable {
                    ty: ty.clone(),
                    value_kind: kind,
                    span
                }),
            };

            TypeAnnotation::typed_value(addr_ty, ValueKind::Temporary, span)
        },

        Operator::Deref => {
            let deref_ty = operand.annotation.ty.deref_ty()
                .cloned()
                .ok_or_else(|| TypecheckError::NotDerefable {
                    ty: operand.annotation.ty.clone(),
                    span: span.clone(),
                })?;

            let value_kind = ValueKind::Mutable;

            TypeAnnotation::typed_value(deref_ty, value_kind, span)
        }

        _ => {
            panic!("invalid operation in AST {:#?}", unary_op)
        }
    };

    Ok(UnaryOp {
        operand,
        op: unary_op.op,
        annotation,
    })
}