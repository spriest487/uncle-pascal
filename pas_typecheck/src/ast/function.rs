use {
    crate::ast::prelude::*,
    pas_syn::Ident,
};

pub type FunctionDecl = ast::FunctionDecl<TypeAnnotation>;
pub type FunctionDef = ast::FunctionDef<TypeAnnotation>;
pub type FunctionParam = ast::FunctionParam<TypeAnnotation>;
pub type InterfaceImpl = ast::InterfaceImpl<TypeAnnotation>;

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

    let impl_iface = match &decl.impl_iface {
        Some(iface_impl) => {
            let method_sig = FunctionSig {
                return_ty: return_ty.clone(),
                params: params.iter().map(|p| p.ty.clone()).collect(),
            };
            Some(find_iface_impl(&iface_impl.iface, &decl.ident, &method_sig, ctx)?)
        },
        None => None,
    };

    Ok(FunctionDecl {
        ident: decl.ident.clone(),
        impl_iface,
        params,
        return_ty: Some(return_ty),
        span: decl.span.clone(),
    })
}

fn find_iface_impl(iface: &Ident, method_ident: &Ident, sig: &FunctionSig, ctx: &Context) -> TypecheckResult<InterfaceImpl> {
    let iface = ctx.find_iface(iface)?;

    let impl_for_types: Vec<_> = iface.methods.iter()
        .filter(|method| method.ident == *method_ident)
        .filter_map(|method| FunctionSig::of_decl(method).impl_ty(&sig))
        .collect();

    match impl_for_types.len() {
        0 => {
            return Err(NameError::MemberNotFound {
                base: Type::Interface(iface.clone().into()),
                span: method_ident.span().clone(),
                member: method_ident.clone(),
            }.into())
        },

        1 => Ok(InterfaceImpl {
            iface: iface.ident.clone(),
            for_ty: impl_for_types[0].clone(),
        }),

        _ => {
            unreachable!("interfaces can't have multiple methods with the same name")
        }
    }
}

pub fn typecheck_func_def(
    def: &ast::FunctionDef<Span>,
    ctx: &mut Context)
    -> TypecheckResult<FunctionDef>
{
    let decl = typecheck_func_decl(&def.decl, ctx)?;

    let body_scope = ctx.push_scope(None);

    for param in &decl.params {
        ctx.declare_binding(param.ident.clone(), Binding {
            ty: param.ty.clone(),
            kind: ValueKind::Immutable,
            def: Some(param.span().clone()),
        })?;
    }

    let body = typecheck_block(&def.body, decl.return_ty.as_ref().unwrap(), ctx)?;

    ctx.pop_scope(body_scope)?;

    Ok(FunctionDef {
        decl,
        body,
        span: def.span.clone(),
    })
}