use {
    crate::{
        ast::prelude::*,
    },
    pas_common::{
        span::*,
    },
    pas_syn::ast,
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

    for (arg, param) in call.args.iter().zip(sig.params.iter()) {
        actual_args.push(typecheck_expr(arg, param, ctx)?);
    }

    if actual_args.len() != sig.params.len() {
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
    let target = typecheck_expr(&call.target, &Type::Nothing, ctx)?;
    let sig = match &target.annotation.ty {
        Type::Function(sig) => sig.as_ref(),
        _ => return Err(TypecheckError::NotCallable(Box::new(target))),
    };

    let args = typecheck_args(sig, call, ctx)?;

    let span = call.annotation.span().clone();

    let annotation = match sig.return_ty.clone() {
        Type::Nothing => TypeAnnotation::untyped(span),
        return_ty => TypeAnnotation::typed_value(return_ty, ValueKind::Temporary, span),
    };

    Ok(Call {
        annotation,
        args,
        target,
    })
}

pub fn typecheck_expr(
    expr_node: &ast::ExpressionNode<Span>,
    expect_ty: &Type,
    ctx: &mut Context)
    -> TypecheckResult<ExpressionNode>
{
    let span = expr_node.annotation.clone();

    match expr_node.expr.as_ref() {
        ast::Expression::Literal(ast::Literal::String(s)) => {
            let binding = ValueKind::Immutable;
            let annotation = TypeAnnotation::typed_value(ctx.string_type(), binding, span.clone());

            Ok(ast::ExpressionNode::new(ast::Literal::String(s.clone()), annotation))
        }

        ast::Expression::Literal(ast::Literal::Boolean(b)) => {
            let binding = ValueKind::Immutable;
            let annotation = TypeAnnotation::typed_value(Primitive::Boolean, binding, span.clone());

            Ok(ast::ExpressionNode::new(ast::Literal::Boolean(*b), annotation))
        }

        ast::Expression::Literal(ast::Literal::Integer(i)) => {
            let binding = ValueKind::Immutable;

            let ty = if i.as_i32().is_some() {
                Type::from(Primitive::Int32)
            } else {
                unimplemented!("integers outside range of i32")
            };
            let annotation = TypeAnnotation::typed_value(ty, binding, span.clone());

            Ok(ast::ExpressionNode::new(ast::Literal::Integer(*i), annotation))
        }

        ast::Expression::Literal(ast::Literal::Real(x)) => {
            let binding = ValueKind::Immutable;

            let ty = if x.as_f32().is_some() {
                Type::from(Primitive::Real32)
            } else {
                unimplemented!("real literal outside range of f32")
            };

            let annotation = TypeAnnotation::typed_value(ty, binding, span.clone());

            Ok(ast::ExpressionNode::new(ast::Literal::Real(x.clone()), annotation))
        }

        ast::Expression::Literal(ast::Literal::Nil) => {
            let annotation = TypeAnnotation::typed_value(Type::Nil, ValueKind::Temporary, span);
            Ok(ast::ExpressionNode::new(ast::Literal::Nil, annotation))
        }

        ast::Expression::Ident(ident) => {
            let binding = ctx.find_named(ident)?;
            let annotation = TypeAnnotation::typed_value(binding.ty.clone(), binding.kind, span);

            Ok(ast::ExpressionNode::new(ident.clone(), annotation))
        }

        ast::Expression::BinOp(bin_op) => {
            let bin_op = typecheck_bin_op(bin_op, ctx)?;
            let annotation = bin_op.annotation.clone();
            Ok(ast::ExpressionNode::new(bin_op, annotation))
        }

        ast::Expression::UnaryOp(unary_op) => {
            let unary_op = typecheck_unary_op(unary_op, ctx)?;
            let annotation = unary_op.annotation.clone();
            Ok(ast::ExpressionNode::new(unary_op, annotation))
        }

        ast::Expression::Call(call) => {
            let call = typecheck_call(call, ctx)?;
            if call.annotation.ty == Type::Nothing {
                return Err(TypecheckError::InvalidCallInExpression(call.clone()));
            }

            let annotation = call.annotation.clone();
            Ok(ast::ExpressionNode::new(call, annotation))
        }

        ast::Expression::ObjectCtor(ctor) => {
            let ctor = typecheck_object_ctor(ctor, ctx)?;
            let annotation = ctor.annotation.clone();
            Ok(ast::ExpressionNode::new(ctor, annotation))
        }

        ast::Expression::IfCond(if_cond) => {
            let if_cond = typecheck_if_cond(if_cond, expect_ty, ctx)?;
            let annotation = if_cond.annotation.clone();
            Ok(ast::ExpressionNode::new(if_cond, annotation))
        }

        ast::Expression::Block(block) => {
            let block = typecheck_block(block, expect_ty, ctx)?;
            let annotation = block.annotation.clone();
            Ok(ast::ExpressionNode::new(block, annotation))
        }
    }
}
