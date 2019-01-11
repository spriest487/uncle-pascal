use {
    crate::{
        context::*,
        FunctionSig,
        result::*,
        Type,
        TypeAnnotation,
    },
    pas_syn::{
        ast,
        Span,
        Spanned,
    },
};

pub type Call = ast::Call<TypeAnnotation>;
pub type ExpressionNode = ast::ExpressionNode<TypeAnnotation>;
pub type BinOp = ast::BinOp<TypeAnnotation>;

fn invalid_args(actual_args: Vec<ExpressionNode>, sig: &FunctionSig, span: Span) -> TypecheckError {
    TypecheckError::InvalidArgs {
        expected: sig.params.clone(),
        actual: actual_args.into_iter()
            .map(|arg| arg.annotation.ty)
            .collect(),
        span,
    }
}

fn typecheck_args(
    sig: &FunctionSig,
    call: &ast::Call<Span>,
    ctx: &mut Context)
    -> TypecheckResult<Vec<ExpressionNode>>
{
    let mut actual_args = Vec::new();
    for arg in &call.args {
        actual_args.push(typecheck_expr(arg, ctx)?);
    }

    if call.args.len() != sig.params.len() {
        // arg counts don't match
        return Err(invalid_args(actual_args, sig, call.annotation.clone()));
    }

    if !actual_args.iter().zip(sig.params.iter())
        .all(|(arg, param)| arg.annotation.ty == *param) {
        // arg types don't match
        return Err(invalid_args(actual_args, sig, call.annotation.clone()));
    }

    Ok(actual_args)
}

pub fn typecheck_call(call: &ast::Call<Span>, ctx: &mut Context) -> TypecheckResult<Call> {
    let target = typecheck_expr(&call.target, ctx)?;
    let sig = match &target.annotation.ty {
        Type::Function(sig) => sig.as_ref(),
        _ => return Err(TypecheckError::NotCallable(Box::new(target))),
    };

    let args = typecheck_args(sig, call, ctx)?;

    let span = call.annotation.span().clone();

    let annotation = match sig.return_ty.clone() {
        Some(return_ty) => TypeAnnotation::typed_value(return_ty, ValueKind::Temporary, span),
        None => TypeAnnotation::untyped(span)
    };

    Ok(Call {
        annotation,
        args,
        target,
    })
}

pub fn typecheck_expr(expr_node: &ast::ExpressionNode<Span>, ctx: &mut Context) -> TypecheckResult<ExpressionNode> {
    let span = expr_node.annotation.clone();

    match expr_node.expr.as_ref() {
        ast::Expression::LiteralInt(i) => {
            let binding = ValueKind::Immutable;
            let annotation = TypeAnnotation::typed_value(Type::Integer, binding, span.clone());

            let expr = ast::Expression::LiteralInt(i.clone());

            Ok(ast::ExpressionNode::new(expr, annotation))
        }

        ast::Expression::LiteralString(_s) => {
            unimplemented!("string literal typechecking")
        }

        ast::Expression::Ident(ident) => {
            let binding = ctx.find_named(ident)?;
            let annotation = TypeAnnotation::typed_value(binding.ty.clone(), binding.kind, span);

            let expr = ast::Expression::Ident(ident.clone());

            Ok(ast::ExpressionNode::new(expr, annotation))
        }

        ast::Expression::BinOp(bin_op) => {
            let lhs = typecheck_expr(&bin_op.lhs, ctx)?;
            let rhs = typecheck_expr(&bin_op.rhs, ctx)?;

            // check valid ops etc, result type etc
            let result_type = lhs.annotation.ty.clone();

            let annotation = TypeAnnotation::typed_value(result_type, ValueKind::Temporary, span);
            let expr = ast::Expression::BinOp(ast::BinOp {
                lhs,
                op: bin_op.op,
                rhs,
            });

            Ok(ast::ExpressionNode::new(expr, annotation))
        }

        ast::Expression::Call(call) => {
            let call = typecheck_call(call, ctx)?;
            if call.annotation.ty == Type::None {
                return Err(TypecheckError::InvalidCallInExpression(call.clone()));
            }

            let annotation = call.annotation.clone();
            let expr = ast::Expression::Call(call);

            Ok(ast::ExpressionNode::new(expr, annotation))
        }
    }
}
