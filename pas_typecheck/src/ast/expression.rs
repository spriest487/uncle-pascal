use {
    crate::{
        ast::prelude::*,
    },
    pas_common::{
        span::*,
    },
    pas_syn::{
        ast,
    },
};

pub type Call = ast::Call<TypeAnnotation>;
pub type ExpressionNode = ast::ExpressionNode<TypeAnnotation>;

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
        Type::None => TypeAnnotation::untyped(span),
        return_ty => TypeAnnotation::typed_value(return_ty, ValueKind::Temporary, span),
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
        ast::Expression::Literal(ast::Literal::String(s)) => {
            let binding = ValueKind::Immutable;
            let annotation = TypeAnnotation::typed_value(ctx.string_type(), binding, span.clone());
            let expr = ast::Expression::Literal(ast::Literal::String(s.clone()));

            Ok(ast::ExpressionNode::new(expr, annotation))
        }

        ast::Expression::Literal(ast::Literal::Boolean(b)) => {
            let binding = ValueKind::Immutable;
            let annotation = TypeAnnotation::typed_value(Type::Boolean, binding, span.clone());
            let expr = ast::Expression::Literal(ast::Literal::Boolean(*b));

            Ok(ast::ExpressionNode::new(expr, annotation))
        }

        ast::Expression::Literal(ast::Literal::Integer(i)) => {
            let binding = ValueKind::Immutable;

            let ty = if i.as_i32().is_some() {
                Type::Integer
            } else {
                unimplemented!("integers outside range of i32")
            };
            let annotation = TypeAnnotation::typed_value(ty, binding, span.clone());
            let expr = ast::Expression::Literal(ast::Literal::Integer(*i));

            Ok(ast::ExpressionNode::new(expr, annotation))
        }

        ast::Expression::Literal(ast::Literal::Real(x)) => {
            let binding = ValueKind::Immutable;

            let ty = if x.as_f32().is_some() {
                Type::Real32
            } else {
                unimplemented!("real literal outside range of f32")
            };

            let annotation = TypeAnnotation::typed_value(ty, binding, span.clone());
            let expr = ast::Expression::Literal(ast::Literal::Real(x.clone()));

            Ok(ast::ExpressionNode::new(expr, annotation))
        }

        ast::Expression::Ident(ident) => {
            let binding = ctx.find_named(ident)?;
            let annotation = TypeAnnotation::typed_value(binding.ty.clone(), binding.kind, span);

            let expr = ast::Expression::Ident(ident.clone());

            Ok(ast::ExpressionNode::new(expr, annotation))
        }

        ast::Expression::BinOp(bin_op) => {
            let bin_op = typecheck_bin_op(bin_op, ctx)?;
            let annotation = bin_op.annotation.clone();
            let expr = ast::Expression::BinOp(bin_op);
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

        ast::Expression::ObjectCtor(ctor) => {
            let ctor = typecheck_object_ctor(ctor, ctx)?;
            let annotation = ctor.annotation.clone();
            let expr = ast::Expression::ObjectCtor(ctor);
            Ok(ast::ExpressionNode::new(expr, annotation))
        }

        ast::Expression::IfCond(if_cond) => {
            let if_cond = typecheck_if_cond(if_cond, ctx)?;
            let annotation = if_cond.annotation.clone();
            let expr = ast::Expression::IfCond(if_cond);
            Ok(ast::ExpressionNode::new(expr, annotation))
        }
    }
}
