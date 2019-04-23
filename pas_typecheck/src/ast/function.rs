use {
    crate::ast::prelude::*,
};

pub type FunctionDecl = ast::FunctionDecl<TypeAnnotation>;
pub type FunctionDef = ast::FunctionDef<TypeAnnotation>;
pub type FunctionParam = ast::FunctionParam<TypeAnnotation>;

fn typecheck_param(
    param: &ast::FunctionParam<Span>,
    ctx: &mut Context)
    -> TypecheckResult<FunctionParam>
{
    let ty = ctx.find_type(&param.ty)?.clone();

    Ok(FunctionParam {
        ident: param.ident.clone(),
        span: param.span.clone(),
        ty,
    })
}

pub fn typecheck_func_decl(
    decl: &ast::FunctionDecl<Span>,
    ctx: &mut Context)
    -> TypecheckResult<FunctionDecl>
{
    let return_ty = match &decl.return_ty {
        Some(ty_name) => ctx.find_type(ty_name)?.clone(),
        None => Type::Nothing,
    };

    let mut params = Vec::new();
    for param in &decl.params {
        let param = typecheck_param(param, ctx)?;
        params.push(param);
    }

    Ok(FunctionDecl {
        ident: decl.ident.clone(),
        params,
        return_ty: Some(return_ty),
        span: decl.span.clone(),
    })
}

pub fn typecheck_func_def(
    def: &ast::FunctionDef<Span>,
    ctx: &mut Context)
    -> TypecheckResult<FunctionDef>
{
    let decl = typecheck_func_decl(&def.decl, ctx)?;

    let body_scope = ctx.push_scope();

    for param in &decl.params {
        ctx.declare_binding(param.ident.clone(), Binding {
            ty: param.ty.clone(),
            kind: ValueKind::Immutable,
            def: Some(param.span().clone()),
        })?;
    }

    let body = typecheck_block(&def.body, decl.return_ty.as_ref().unwrap(), ctx)?;

    ctx.pop_scope(body_scope);

    Ok(FunctionDef {
        decl,
        body,
        span: def.span.clone(),
    })
}