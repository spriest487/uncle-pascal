use std::rc::Rc;
use types::Type;
use semantic::{
    Expression,
    SemanticContext,
    Scope,
    SemanticResult,
};
use operators;
use super::{
    expect_initialized,
    ops,
};
use syntax;

pub fn annotate(condition: &syntax::Expression,
                then_branch: &syntax::Expression,
                else_branch: Option<&syntax::Expression>,
                context: &SemanticContext)
                -> SemanticResult<(Expression, Rc<Scope>)> {
    let cond_type = Some(Type::Boolean);
    let (cond_expr, scope_after_condition) = Expression::annotate(
        condition,
        cond_type.as_ref(),
        context.scope.clone()
    )?;
    expect_initialized(&cond_expr)?;

    let (then_expr, scope_after_then) = Expression::annotate(then_branch, None, scope_after_condition.clone())?;
    let (else_expr, scope_after_else) = match else_branch {
        Some(expr) => {
            let (else_expr, scope_after_else) = Expression::annotate(expr, None, scope_after_condition.clone())?;
            (Some(else_expr), scope_after_else)
        }
        None => (None, scope_after_condition.clone())
    };

    let if_then_else = Expression::if_then_else(cond_expr, then_expr, else_expr, context.clone());

    /*
        the scope for the condition, and each of the branches, doesn't live longer than the
        if-expression. however, variables initialized in the condition expr, or variables initialized
        in both of the branch expressions (if there is an else-branch), should still be considered
        initialized after this if-expression.

        ```
        { function GetThing(out thing: Thing): Boolean }
        if GetThing(thing) then
            x := 1
        else
            x := 2
        ```
        `thing` and and `x` should be initialized
     */

    let mut out_scope = context.scope.clone();
    for name in scope_after_condition.initialized_since(context.scope.as_ref()) {
        out_scope = Rc::new(out_scope.as_ref().clone()
            .initialize_symbol(name));
    }

    if else_branch.is_some() {
        let then_initialized = scope_after_then.initialized_since(scope_after_condition.as_ref());

        for name in scope_after_else.initialized_since(scope_after_condition.as_ref()) {
            if then_initialized.contains(&name) {
                out_scope = Rc::new(out_scope.as_ref().clone()
                    .initialize_symbol(&name));
            }
        }
    }

    Ok((if_then_else, out_scope))
}

pub fn expr_type(condition: &Expression,
                 then_branch: &Expression,
                 else_branch: Option<&Expression>,
                 context: &SemanticContext) -> SemanticResult<()> {
    ops::expect_valid(
        operators::Equals,
        Some(&Type::Boolean),
        condition,
        context
    )?;

    let _then_type = then_branch.expr_type()?;

    if let Some(else_expr) = else_branch {
        else_expr.expr_type()?;
    }

    Ok(())
}
