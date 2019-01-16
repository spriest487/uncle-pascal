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
    let lhs = typecheck_expr(&bin_op.lhs, &Type::None, ctx)?;
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
            let rhs = typecheck_expr(&bin_op.rhs, &Type::None, ctx)?;

            // check valid ops etc, result type etc
            let result_ty = lhs.annotation.ty.clone();

            let annotation = match result_ty {
                Type::None => TypeAnnotation::untyped(span),
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