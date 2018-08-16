use std::rc::Rc;

use syntax;
use operators;
use semantic::{
    Expression,
    SemanticResult,
    SemanticContext,
    SemanticError,
    Scope,
};
use types::Type;
use node::ExpressionValue;
use super::{
    ops,
    expect_initialized,
};

pub fn annotate_for(from: &syntax::Expression,
                    to: &syntax::Expression,
                    body: &syntax::Expression,
                    context: &SemanticContext)
                    -> SemanticResult<(Expression, Rc<Scope>)> {
    let (from_expr, to_scope) = Expression::annotate(from, None, context.scope.clone())?;
    let (to_expr, body_scope) = Expression::annotate(to, None, to_scope.clone())?;
    expect_initialized(&to_expr)?;

    /*  we don't use the resulting scope of the loop body, because the loop
        may run 0 times
        todo: initializations in the from and to should be in the final scope?
    */
    let (do_expr, _) = Expression::annotate(body, None, body_scope.clone())?;

    let for_loop = Expression::for_loop(from_expr, to_expr, do_expr, context.clone());

    let mut scope_out = context.scope.clone();
    for name in to_scope.initialized_since(context.scope.as_ref()) {
        scope_out = Rc::new(scope_out.as_ref().clone().initialize_symbol(name));
    }
    for name in body_scope.initialized_since(context.scope.as_ref()) {
        scope_out = Rc::new(scope_out.as_ref().clone().initialize_symbol(name));
    }

    Ok((for_loop, scope_out))
}

pub fn for_type(from: &Expression,
                to: &Expression,
                body: &Expression,
                context: &SemanticContext) -> SemanticResult<()> {
    let to_type = to.expr_type()?;

    let _body_type = body.expr_type()?;

    let iter_expr = match &from.value {
        | ExpressionValue::BinaryOperator { op, lhs, .. }
        if *op == operators::Operator::Assignment => {
            lhs.as_ref()
        }

        | ExpressionValue::LetBinding(binding) => {
            binding.value.as_ref()
        }

        //TODO better error
        _ => return Err(SemanticError::unexpected_type(
            to_type.clone(),
            from.expr_type()?,
            context.clone()))
    };

    ops::expect_valid(
        operators::Equals,
        to_type.as_ref(),
        iter_expr,
        context
    )
}

pub fn annotate_while(condition: &syntax::Expression,
                      body: &syntax::Expression,
                      context: SemanticContext)
                      -> SemanticResult<(Expression, Rc<Scope>)> {
    let cond_type = Some(Type::Boolean);
    let (cond_expr, scope) = Expression::annotate(
        condition,
        cond_type.as_ref(),
        context.scope.clone(),
    )?;

    expect_initialized(&cond_expr)?;

    /* anything initialized or declared in the scope of the body is strictly
    confined to the body, because it may never execute */
    let (body_expr, _) = Expression::annotate(body, None, context.scope.clone())?;

    let while_loop = Expression::while_loop(cond_expr, body_expr, context);

    Ok((while_loop, scope))
}

pub fn while_type(condition: &Expression, body: &Expression) -> SemanticResult<()> {
    body.expr_type()?;

    ops::expect_valid(
        operators::Equals,
        Some(&Type::Boolean), 
        condition, 
        &condition.context,
    )
}