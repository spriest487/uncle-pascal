use {
    pas_syn::{
        ast,
        Span,
    },
    crate::{
        Type,
        TypeAnnotation,
        result::*,
        context::*,
    },
};

pub type ExpressionNode = ast::ExpressionNode<TypeAnnotation>;

pub fn typecheck_expr(expr_node: &ast::ExpressionNode<Span>, ctx: &mut Context) -> TypecheckResult<ExpressionNode> {
    let span = expr_node.annotation.clone();

    match expr_node.expr.as_ref() {
        ast::Expression::LiteralInt(i) => {
            let binding = ValueKind::Immutable;
            let annotation = TypeAnnotation::typed_value(Type::Integer, binding, span.clone());

            let expr = ast::Expression::LiteralInt(i.clone());

            Ok(ast::ExpressionNode::new(expr, annotation))
        }

        ast::Expression::Ident(ident) => {
            let binding = ctx.find_binding(ident)?;
            let annotation = TypeAnnotation::typed_value(binding.ty.clone(), binding.kind, span);

            let expr = ast::Expression::Ident(ident.clone());

            Ok(ast::ExpressionNode::new(expr, annotation))
        }

        ast::Expression::BinOp(bin_op) => {
            let lhs = typecheck_expr(&bin_op.lhs, ctx)?;
            let rhs = typecheck_expr(&bin_op.lhs, ctx)?;

            // check valid ops etc, result type etc
            let result_type = lhs.annotation.ty.clone();

            let annotation = TypeAnnotation::typed_value(result_type, ValueKind::Immutable, span);
            let expr = ast::Expression::BinOp(ast::BinOp {
                lhs,
                op: bin_op.op,
                rhs,
            });

            Ok(ast::ExpressionNode::new(expr, annotation))
        }
    }
}
