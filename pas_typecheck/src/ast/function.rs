use {
    std::rc::Rc,
    crate::ast::prelude::*
};

pub type FunctionDecl = ast::FunctionDecl<TypeAnnotation>;
pub type FunctionParam = ast::FunctionParam<TypeAnnotation>;

fn typecheck_param(
    param: &ast::FunctionParam<Span>,
    ctx: &mut Context)
    -> TypecheckResult<FunctionParam>
{
    let ty = ctx.find_type(&param.ty)?.clone();

    let annotation = TypeAnnotation::typed_value(
        ty.clone(),
        ValueKind::Immutable,
        param.annotation.clone()
    );

    Ok(FunctionParam {
        ident: param.ident.clone(),
        ty,
        annotation,
    })
}

pub fn typecheck_func_decl(
    decl: &ast::FunctionDecl<Span>,
    ctx: &mut Context)
    -> TypecheckResult<FunctionDecl>
{
    let return_ty = match &decl.return_ty {
        Some(ty_name) => Some(ctx.find_type(ty_name)?.clone()),
        None => None,
    };

    let body_scope = ctx.push_scope();

    let mut params = Vec::new();
    for param in &decl.params {
        let param = typecheck_param(param, ctx)?;
        ctx.declare_binding(param.ident.clone(), Binding {
            ty: param.ty.clone(),
            kind: ValueKind::Immutable,
        })?;

        params.push(param);
    }

    let body = typecheck_block(&decl.body, return_ty.as_ref(), ctx)?;

    ctx.pop_scope(body_scope);

    let sig = FunctionSig {
        params: params.iter().map(|p| p.ty.clone()).collect(),
        return_ty: return_ty.clone(),
    };

    let annotation = TypeAnnotation::typed_value(
        Type::Function(Rc::new(sig)),
        ValueKind::Function,
        decl.annotation.clone(),
    );

    Ok(FunctionDecl {
        ident: decl.ident.clone(),
        return_ty,
        annotation,
        params,
        body
    })
}