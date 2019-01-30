use {
    crate::ast::prelude::*,
};

pub type ForLoop = ast::ForLoop<TypeAnnotation>;

pub fn typecheck_for_loop(
    for_loop: &ast::ForLoop<Span>,
    ctx: &mut Context) -> TypecheckResult<ForLoop>
{
    let annotation = TypeAnnotation::untyped(for_loop.annotation.clone());

    let inner_scope = ctx.push_scope();
    let init_binding = typecheck_local_binding(&for_loop.init_binding, ctx)?;

    if init_binding.val_ty != Some(Type::Primitive(Primitive::Int32)) {
        unimplemented!("non-int32 loops");
    }

    let to_expr = typecheck_expr(&for_loop.to_expr, &Type::Primitive(Primitive::Boolean), ctx)?;
    if to_expr.annotation.ty != *init_binding.val_ty.as_ref().unwrap() {
        return Err(TypecheckError::TypeMismatch {
            expected: init_binding.val_ty.unwrap(),
            actual: to_expr.annotation.ty,
            span: annotation.span,
        })
    }

    let body = typecheck_stmt(&for_loop.body, ctx).map(Box::new)?;

    ctx.pop_scope(inner_scope);

    Ok(ForLoop {
        init_binding,
        to_expr,
        body,
        annotation,
    })
}