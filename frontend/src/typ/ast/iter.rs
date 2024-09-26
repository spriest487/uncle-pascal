use crate::ast;
use crate::typ::ast::typecheck_expr;
use crate::typ::ast::typecheck_local_binding;
use crate::typ::ast::typecheck_stmt;
use crate::typ::Context;
use crate::typ::Environment;
use crate::typ::Primitive;
use crate::typ::Type;
use crate::typ::TypeError;
use crate::typ::TypeResult;
use crate::typ::Typed;
use common::span::Span;
use common::span::Spanned;

pub type ForLoop = ast::ForLoop<Typed>;
pub type WhileLoop = ast::WhileLoop<Typed>;

pub fn typecheck_for_loop(
    for_loop: &ast::ForLoop<Span>,
    ctx: &mut Context,
) -> TypeResult<ForLoop> {
    let annotation = Typed::Untyped(for_loop.annotation.clone());

    let inner_scope = ctx.push_scope(Environment::Block {
        allow_unsafe: false,
    });

    let (init, counter_ty) = match &for_loop.init {
        ast::ForLoopInit::Binding(init_binding) => {
            let init_binding = typecheck_local_binding(init_binding, ctx)?;
            let counter_ty = init_binding.ty.clone();

            let init = ast::ForLoopInit::Binding(Box::new(init_binding));
            (init, counter_ty)
        },

        ast::ForLoopInit::Assignment { counter, value } => {
            let counter = typecheck_expr(counter, &Primitive::Int32.into(), ctx)?;
            if let ast::Expr::Ident(ident, ..) = &counter {
                if ctx.get_decl_scope(ident).is_some() {
                    ctx.initialize(ident)
                }
            }

            let counter_ty = counter.annotation().ty().into_owned();
            let value = typecheck_expr(value, &counter_ty, ctx)?;
            let init = ast::ForLoopInit::Assignment {
                counter: Box::new(counter),
                value: Box::new(value),
            };

            (init, counter_ty)
        },
    };

    if !counter_ty
        .as_primitive()
        .map(|p| p.is_integer())
        .unwrap_or(false)
    {
        return Err(TypeError::InvalidLoopCounterType {
            counter_ty,
            span: annotation.span().clone(),
        });
    }

    let to_expr = typecheck_expr(&for_loop.to_expr, &counter_ty, ctx)?;
    to_expr.annotation().expect_value(&counter_ty)?;

    // loops bodies never have values
    let body_expect_ty = Type::Nothing;

    ctx.push_loop(for_loop.span().clone());
    let body = typecheck_stmt(&for_loop.body, &body_expect_ty, ctx).map(Box::new)?;
    ctx.pop_loop();

    ctx.pop_scope(inner_scope);

    Ok(ForLoop {
        init,
        to_expr,
        body,
        annotation,
    })
}

pub fn typecheck_while_loop(
    while_loop: &ast::WhileLoop<Span>,
    ctx: &mut Context,
) -> TypeResult<WhileLoop> {
    let annotation = Typed::Untyped(while_loop.span().clone());

    let bool_ty = Type::Primitive(Primitive::Boolean);
    let condition = typecheck_expr(&while_loop.condition, &bool_ty, ctx)?;

    condition.annotation().expect_value(&bool_ty)?;

    // loops bodies never have values
    let body_expect_ty = Type::Nothing;

    ctx.push_loop(while_loop.span().clone());
    let body = typecheck_stmt(&while_loop.body, &body_expect_ty, ctx).map(Box::new)?;
    ctx.pop_loop();

    Ok(WhileLoop {
        condition,
        body,
        annotation,
    })
}
