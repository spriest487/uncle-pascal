use crate::ast::prelude::*;

pub type ForLoop = ast::ForLoop<TypeAnnotation>;
pub type WhileLoop = ast::WhileLoop<TypeAnnotation>;

pub fn typecheck_for_loop(
    for_loop: &ast::ForLoop<Span>,
    ctx: &mut Context,
) -> TypecheckResult<ForLoop> {
    let annotation = TypeAnnotation::Untyped(for_loop.annotation.clone());

    let inner_scope = ctx.push_scope(Environment::Block { allow_unsafe: false });
    let init_binding = typecheck_local_binding(&for_loop.init_binding, ctx)?;

    if init_binding.val_ty != Type::Primitive(Primitive::Int32) {
        unimplemented!("non-int32 loops");
    }

    let to_expr = typecheck_expr(&for_loop.to_expr, &Type::Primitive(Primitive::Boolean), ctx)?;
    if *to_expr.annotation().ty() != init_binding.val_ty {
        return Err(TypecheckError::TypeMismatch {
            expected: init_binding.val_ty,
            actual: to_expr.annotation().ty().into_owned(),
            span: annotation.span().clone(),
        });
    }

    // loops bodies never have values
    let body_expect_ty = Type::Nothing;

    ctx.push_loop(for_loop.span().clone());
    let body = typecheck_stmt(&for_loop.body, &body_expect_ty, ctx).map(Box::new)?;
    ctx.pop_loop();

    ctx.pop_scope(inner_scope);

    Ok(ForLoop {
        init_binding,
        to_expr,
        body,
        annotation,
    })
}

pub fn typecheck_while_loop(
    while_loop: &ast::WhileLoop<Span>,
    ctx: &mut Context,
) -> TypecheckResult<WhileLoop> {
    let annotation = TypeAnnotation::Untyped(while_loop.span().clone());

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
