use pas_syn::Ident;

use crate::ast::prelude::*;

use std::rc::Rc;
use pas_syn::ast::DeclMod;

pub type FunctionDecl = ast::FunctionDecl<TypeAnnotation>;
pub type FunctionDef = ast::FunctionDef<TypeAnnotation>;
pub type FunctionParam = ast::FunctionParam<TypeAnnotation>;
pub type InterfaceImpl = ast::InterfaceImpl<TypeAnnotation>;

fn typecheck_param(
    param: &ast::FunctionParam<Span>,
    ctx: &mut Context,
) -> TypecheckResult<FunctionParam> {
    let ty = typecheck_type(&param.ty, ctx)?.clone();

    Ok(FunctionParam {
        modifier: param.modifier.clone(),
        ident: param.ident.clone(),
        span: param.span.clone(),
        ty,
    })
}

pub fn typecheck_func_decl(
    decl: &ast::FunctionDecl<Span>,
    ctx: &mut Context,
) -> TypecheckResult<FunctionDecl> {
    let decl_scope = ctx.push_scope(Environment::FunctionDecl);

    if let Some(extern_mod) = decl.mods.iter().find(|m| m.keyword() == DeclMod::EXTERNAL_WORD) {
        if !decl.type_params.is_empty() {
            let ty_args_span = decl.type_params[0].ident.span().to(decl.type_params.last().unwrap().ident.span());
            return Err(TypecheckError::ExternalGenericFunction {
                func: decl.ident.last().clone(),
                extern_modifier: extern_mod.span().clone(),
                ty_args: ty_args_span,
            });
        }
    }

    let type_params = typecheck_type_params(&decl.type_params, ctx)?;
    ctx.declare_type_params(&type_params)?;

    let return_ty = match &decl.return_ty {
        Some(ty_name) => typecheck_type(ty_name, ctx)?.clone(),
        None => Type::Nothing,
    };

    let mut params = Vec::new();
    for param in &decl.params {
        let param = typecheck_param(param, ctx)?;
        params.push(param);
    }

    let (ident, impl_iface) = match &decl.impl_iface {
        Some(iface_impl) => {
            let method_sig = FunctionSig {
                return_ty: return_ty.clone(),
                type_params_len: decl.type_params.len(),
                params: params
                    .iter()
                    .map(|p| FunctionParamSig {
                        ty: p.ty.clone(),
                        modifier: p.modifier.clone(),
                    })
                    .collect(),
            };

            let iface_def = match typecheck_type(&iface_impl.iface, ctx)? {
                Type::Interface(iface) => {
                    ctx.find_iface_def(&iface)?
                },

                not_iface => {
                    return Err(TypecheckError::InvalidMethodInterface {
                        ty: not_iface,
                        span: iface_impl.iface.span().clone(),
                    })
                }
            };

            let iface_impl = find_iface_impl(iface_def, decl.ident.single(), &method_sig)?;
            (decl.ident.clone(), Some(iface_impl))
        }
        None => {
            let name = ctx.qualify_name(decl.ident.single().clone());
            (name, None)
        }
    };

    ctx.pop_scope(decl_scope);

    Ok(FunctionDecl {
        ident,
        impl_iface,
        params,
        type_params: type_params.clone(),
        return_ty: Some(return_ty),
        span: decl.span.clone(),
        mods: decl.mods.clone(),
    })
}

fn find_iface_impl(
    iface_def: Rc<Interface>,
    method_ident: &Ident,
    sig: &FunctionSig,
) -> TypecheckResult<InterfaceImpl> {
    let impl_for_types: Vec<_> = iface_def
        .methods
        .iter()
        .filter(|method| *method.ident.single() == *method_ident)
        .filter_map(|method| FunctionSig::of_decl(method).impl_ty(&sig))
        .collect();

    match impl_for_types.len() {
        0 => Err(NameError::MemberNotFound {
            base: Type::Interface(iface_def.name.qualified.clone()),
            span: method_ident.span().clone(),
            member: method_ident.clone(),
        }
        .into()),

        1 => Ok(InterfaceImpl {
            iface: Type::Interface(iface_def.name.qualified.clone()),
            for_ty: impl_for_types[0].clone(),
        }),

        _ => unreachable!("interfaces can't have multiple methods with the same name"),
    }
}

pub fn typecheck_func_def(
    def: &ast::FunctionDef<Span>,
    ctx: &mut Context,
) -> TypecheckResult<FunctionDef> {
    let decl = typecheck_func_decl(&def.decl, ctx)?;

    let body_scope = ctx.push_scope(Environment::FunctionBody {
        result_ty: decl.return_ty.clone().unwrap_or(Type::Nothing)
    });

    // functions are always declared within their own bodies (allowing recursive calls)
    ctx.declare_function(decl.ident.last().clone(), &decl, Visibility::Private)?;

    let type_params = typecheck_type_params(&def.decl.type_params, ctx)?;

    ctx.declare_type_params(&type_params)?;

    for param in &decl.params {
        let (kind, init) = match param.modifier {
            Some(ast::FunctionParamMod::Var) => (ValueKind::Mutable, true),
            Some(ast::FunctionParamMod::Out) => (ValueKind::Uninitialized, false),
            None => (ValueKind::Immutable, false),
        };

        ctx.declare_binding(
            param.ident.clone(),
            Binding {
                ty: param.ty.clone(),
                kind,
                def: Some(param.span().clone()),
            },
        )?;

        if init {
            ctx.initialize(&param.ident);
        }
    }

    let body = typecheck_block(&def.body, decl.return_ty.as_ref().unwrap(), ctx)?;

    ctx.pop_scope(body_scope);

    Ok(FunctionDef {
        decl,
        body,
        span: def.span.clone(),
    })
}

pub fn specialize_func_decl(decl: &FunctionDecl, args: &[Type]) -> TypecheckResult<FunctionDecl> {
    assert_eq!(args.len(), decl.type_params.len());

    let params: Vec<_> = decl
        .params
        .iter()
        .map(|param| {
            let ty = param.ty.clone().substitute_type_args(args);

            Ok(FunctionParam {
                ty,
                ..param.clone()
            })
        })
        .collect::<TypecheckResult<_>>()?;

    let return_ty = match &decl.return_ty {
        Some(return_ty) => Some(return_ty.clone().substitute_type_args(args)),
        None => None,
    };

    Ok(FunctionDecl {
        params,
        return_ty,
        ..decl.clone()
    })
}
