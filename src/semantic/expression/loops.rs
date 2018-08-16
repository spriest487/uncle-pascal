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
use super::ops;

pub fn annotate_for(from: &syntax::Expression,
                    to: &syntax::Expression,
                    body: &syntax::Expression,
                    context: SemanticContext)
                    -> SemanticResult<(Expression, Rc<Scope>)> {
    let (from_expr, to_scope) = Expression::annotate(from, None, context.scope.clone())?;
    let (to_expr, body_scope) = Expression::annotate(to, None, to_scope.clone())?;

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

pub fn annotate_if(from: &Expression,
                   to: &Expression,
                   body: &Expression,
                   context: &SemanticContext) -> SemanticResult<()> {
    let from_type = from.expr_type()?;
    let to_type = to.expr_type()?;

    ops::expect_comparable(Some(&Type::Int32), to_type.as_ref(), context)?;

    let _body_type = body.expr_type()?;

    match &from.value {
        | ExpressionValue::BinaryOperator { op, lhs, .. }
        if *op == operators::Operator::Assignment => {
            let lhs_type = lhs.expr_type()?;

            ops::expect_comparable(Some(&Type::Int32), lhs_type.as_ref(),
                                   context)?;
            Ok(())
        }

        | ExpressionValue::LetBinding(binding) => {
            let value_type = binding.value.expr_type()?;

            ops::expect_comparable(Some(&Type::Int32), value_type.as_ref(),
                                   context)?;
            Ok(())
        }

        //TODO better error
        _ => Err(SemanticError::unexpected_type(
            Some(Type::Int32),
            from_type,
            context.clone()))
    }
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

    /* anything initialized or declared in the scope of the body is strictly
    confined to the body, because it may never execute */
    let (body_expr, _) = Expression::annotate(body, None, context.scope.clone())?;

    let while_loop = Expression::while_loop(cond_expr, body_expr, context);

    Ok((while_loop, scope))
}

pub fn while_type(condition: &Expression, body: &Expression) -> SemanticResult<()> {
    let cond_type = condition.expr_type()?;
    body.expr_type()?;

    ops::expect_comparable(Some(&Type::Boolean), cond_type.as_ref(), &condition.context)?;
    Ok(())
}