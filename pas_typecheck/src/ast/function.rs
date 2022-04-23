use crate::{
    ast::{const_eval_string, prelude::ast::TypeList, typecheck_block, typecheck_expr, InterfaceDecl},
    string_type, typecheck_type, typecheck_type_params, Binding, Context, Environment,
    FunctionParamSig, FunctionSig, NameContainer, NameError, NameResult, Type, TypeAnnotation,
    TypecheckError, TypecheckResult, TypedValueAnnotation, ValueKind,
};
use linked_hash_map::LinkedHashMap;
use pas_common::span::{Span, Spanned};
use pas_syn::ast::Visibility;
use pas_syn::{ast, Ident};
use std::rc::Rc;
use crate::ast::const_eval::ConstEval;

pub type FunctionDecl = ast::FunctionDecl<TypeAnnotation>;
pub type DeclMod = ast::DeclMod<TypeAnnotation>;
pub type FunctionDef = ast::FunctionDef<TypeAnnotation>;
pub type FunctionParam = ast::FunctionParam<TypeAnnotation>;
pub type InterfaceImpl = ast::InterfaceImpl<TypeAnnotation>;
pub type InterfaceMethodDecl = ast::InterfaceMethodDecl<TypeAnnotation>;
pub type AnonymousFunctionDef = ast::AnonymousFunctionDef<TypeAnnotation>;
pub type FunctionLocalDecl = ast::FunctionLocalDecl<TypeAnnotation>;

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

    if let Some(extern_mod) = decl
        .mods
        .iter()
        .find(|m| m.keyword() == DeclMod::EXTERNAL_WORD)
    {
        if let Some(decl_type_params) = &decl.type_params {
            let ty_args_span = decl_type_params.items[0].ident.span().to(decl_type_params
                .items
                .last()
                .unwrap()
                .ident
                .span());
            return Err(TypecheckError::ExternalGenericFunction {
                func: decl.ident.last().clone(),
                extern_modifier: extern_mod.span().clone(),
                ty_args: ty_args_span,
            });
        }
    }

    let type_params = match decl.type_params.as_ref() {
        Some(decl_type_params) => {
            let type_params = typecheck_type_params(decl_type_params, ctx)?;
            ctx.declare_type_params(&type_params)?;
            Some(type_params)
        },
        None => None,
    };

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
            let param_sigs = params.iter().cloned().map(FunctionParamSig::from).collect();

            let method_sig = FunctionSig::new(return_ty.clone(), param_sigs, type_params.clone());

            let iface_def = match typecheck_type(&iface_impl.iface, ctx)? {
                Type::Interface(iface) => ctx.find_iface_def(&iface).map_err(|err| {
                    TypecheckError::from_name_err(err, iface_impl.iface.span().clone())
                })?,

                not_iface => {
                    return Err(TypecheckError::InvalidMethodInterface {
                        ty: not_iface,
                        span: iface_impl.iface.span().clone(),
                    });
                },
            };

            let iface_impl =
                find_iface_impl(iface_def, decl.ident.single(), &method_sig).map_err(|err| {
                    TypecheckError::from_name_err(err, iface_impl.iface.span().clone())
                })?;
            (decl.ident.clone(), Some(iface_impl))
        },
        None => {
            let name = ctx.qualify_name(decl.ident.single().clone());
            (name, None)
        },
    };

    ctx.pop_scope(decl_scope);

    let decl_mods = typecheck_decl_mods(&decl.mods, ctx)?;

    Ok(FunctionDecl {
        ident,
        impl_iface,
        params,
        type_params: type_params.clone(),
        return_ty: Some(return_ty),
        span: decl.span.clone(),
        mods: decl_mods,
    })
}

fn typecheck_decl_mods(
    decl_mods: &[ast::DeclMod<Span>],
    ctx: &mut Context,
) -> TypecheckResult<Vec<DeclMod>> {
    let mut results = Vec::new();

    for decl_mod in decl_mods {
        let result = match decl_mod {
            ast::DeclMod::External { src, span } => {
                let string_ty = string_type(ctx)?;

                let src = typecheck_expr(src, &string_ty, ctx)?;
                let src_str = const_eval_string(&src, ctx)?;

                DeclMod::External {
                    src: src_str,
                    span: span.clone(),
                }
            },
        };

        results.push(result);
    }

    Ok(results)
}

fn find_iface_impl(
    iface_def: Rc<InterfaceDecl>,
    method_ident: &Ident,
    sig: &FunctionSig,
) -> NameResult<InterfaceImpl> {
    let impl_for_types: Vec<_> = iface_def
        .methods
        .iter()
        .filter(|method| *method.ident() == *method_ident)
        .filter_map(|method| FunctionSig::of_decl(&method.decl).impl_ty(&sig))
        .collect();

    match impl_for_types.len() {
        0 => {
            let iface_name = iface_def.name.qualified.clone();
            let iface_ty = Type::Interface(Box::new(iface_name));

            Err(NameError::MemberNotFound {
                base: NameContainer::Type(iface_ty),
                member: method_ident.clone(),
            })
        },

        1 => Ok(InterfaceImpl {
            iface: Type::Interface(Box::new(iface_def.name.qualified.clone())),
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
        result_ty: decl.return_ty.clone().unwrap_or(Type::Nothing),
    });

    let mut locals = Vec::new();
    for local in &def.locals {
        let ty = typecheck_type(&local.ty, ctx)?;

        let initial_val = match &local.initial_val {
            Some(expr) => {
                let expr = typecheck_expr(expr, &ty, ctx)?;
                let value = expr.const_eval(ctx).ok_or_else(|| TypecheckError::InvalidConstExpr {
                    expr: Box::new(expr.clone()),
                })?;

                Some(value)
            },
            None => None,
        };

        match local.kind {
            ast::FunctionLocalDeclKind::Const => {
                let val = match &initial_val {
                    Some(val) => val,
                    None => {
                        return Err(TypecheckError::ConstDeclWithNoValue { span: local.span.clone() });
                    },
                };

                // the visibility doesn't really matter here because it's not a unit-level decl
                let visiblity = Visibility::Interface;
                ctx.declare_const(local.ident.clone(), val.clone(), ty.clone(), visiblity, local.span.clone())?;
            },

            ast::FunctionLocalDeclKind::Var => {
                let binding_kind = match &initial_val {
                    Some(..) => ValueKind::Mutable,
                    None => ValueKind::Uninitialized,
                };

                ctx.declare_binding(local.ident.clone(), Binding {
                    kind: binding_kind,
                    ty: ty.clone(),
                    def: Some(local.ident.clone()),
                })?;
            },
        }

        locals.push(FunctionLocalDecl {
            ident: local.ident.clone(),
            kind: local.kind,
            initial_val,
            ty,
            span: local.span.clone(),
        });
    }

    // functions are always declared within their own bodies (allowing recursive calls)
    // but forward-declared functions may already be present in the scope - in which case we
    // don't need to declare it again
    let find_existing_decl = ctx.find_function(&decl.ident);
    if find_existing_decl.is_err() {
        ctx.declare_function(decl.ident.last().clone(), &decl, Visibility::Implementation)?;
    }

    // declare decl's type params within the body too
    if let Some(decl_type_params) = decl.type_params.as_ref() {
        ctx.declare_type_params(&decl_type_params)?;
    };

    declare_func_params_in_body(&decl.params, ctx)?;

    let body = typecheck_block(&def.body, decl.return_ty.as_ref().unwrap(), ctx)?;

    ctx.pop_scope(body_scope);

    Ok(FunctionDef {
        decl,
        locals,
        body,
        span: def.span.clone(),
    })
}

fn declare_func_params_in_body(params: &[FunctionParam], ctx: &mut Context) -> TypecheckResult<()> {
    for param in params {
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
                def: Some(param.ident.clone()),
            },
        )?;

        if init {
            ctx.initialize(&param.ident);
        }
    }

    Ok(())
}

pub fn specialize_func_decl(
    decl: &FunctionDecl,
    args: &TypeList<Type>,
    span: &Span,
    ctx: &Context,
) -> TypecheckResult<FunctionDecl> {
    FunctionSig::of_decl(&decl)
        .validate_type_args(args, ctx)
        .map_err(|err| TypecheckError::from_generic_err(err, span.clone()))?;

    let mut params = Vec::new();
    for param in decl.params.iter() {
        let ty = param.ty.clone().substitute_type_args(args);
        params.push(FunctionParam {
            ty,
            ..param.clone()
        });
    }

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

pub fn typecheck_func_expr(
    src_def: &ast::AnonymousFunctionDef<Span>,
    _expect_ty: &Type,
    ctx: &mut Context,
) -> TypecheckResult<AnonymousFunctionDef> {
    let mut params = Vec::new();
    for param in &src_def.params {
        let param = typecheck_param(param, ctx)?;
        params.push(param);
    }

    let return_ty = match &src_def.return_ty {
        Some(src_return_ty) => typecheck_type(src_return_ty, ctx)?,
        None => Type::Nothing,
    };

    let sig_params = params.iter().map(|p| p.clone().into()).collect();

    let sig = Rc::new(FunctionSig {
        return_ty: return_ty.clone(),
        params: sig_params,
        type_params: None,
    });

    let body_scope_id = ctx.push_scope(Environment::ClosureBody {
        result_ty: return_ty.clone(),
        captures: LinkedHashMap::new(),
    });

    declare_func_params_in_body(&params, ctx)?;
    let body = typecheck_block(&src_def.body, &return_ty, ctx)?;

    let captures = match ctx.pop_scope(body_scope_id).into_env() {
        Environment::ClosureBody { captures, .. } => captures,
        _ => unreachable!(),
    };

    let annotation = TypedValueAnnotation {
        decl: None,
        span: src_def.span().clone(),
        ty: Type::Function(sig),
        value_kind: ValueKind::Temporary,
    }
    .into();

    Ok(AnonymousFunctionDef {
        params,
        return_ty: Some(return_ty),
        annotation,
        body,
        captures,
    })
}
