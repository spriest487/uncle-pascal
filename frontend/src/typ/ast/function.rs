use crate::ast;
use crate::ast::IdentPath;
use crate::ast::TypeAnnotation;
use crate::ast::Visibility;
use crate::ast::{FunctionName, Ident};
use crate::typ::ast::const_eval::ConstEval;
use crate::typ::ast::const_eval_string;
use crate::typ::ast::typecheck_block;
use crate::typ::ast::typecheck_expr;
use crate::typ::ast::InterfaceDecl;
use crate::typ::string_type;
use crate::typ::typecheck_type_params;
use crate::typ::Binding;
use crate::typ::ClosureBodyEnvironment;
use crate::typ::Context;
use crate::typ::Environment;
use crate::typ::FunctionBodyEnvironment;
use crate::typ::FunctionParamSig;
use crate::typ::GenericResult;
use crate::typ::NameContainer;
use crate::typ::NameError;
use crate::typ::NameResult;
use crate::typ::Type;
use crate::typ::TypeList;
use crate::typ::TypeError;
use crate::typ::TypecheckResult;
use crate::typ::Typed;
use crate::typ::TypedValue;
use crate::typ::ValueKind;
use crate::typ::{typecheck_type, FunctionSig};
use common::span::Span;
use common::span::Spanned;
use derivative::Derivative;
use linked_hash_map::LinkedHashMap;
use std::fmt;
use std::fmt::Formatter;
use std::rc::Rc;

pub const SELF_PARAM_NAME: &str = "self";
pub const SELF_TY_NAME: &str = "Self";

pub type FunctionDecl = ast::FunctionDecl<Typed>;
pub type DeclMod = ast::DeclMod<Typed>;
pub type FunctionDef = ast::FunctionDef<Typed>;
pub type FunctionParam = ast::FunctionParam<Typed>;
pub type InterfaceMethodDecl = ast::InterfaceMethodDecl<Typed>;
pub type AnonymousFunctionDef = ast::AnonymousFunctionDef<Typed>;
pub type FunctionLocalDecl = ast::FunctionLocalBinding<Typed>;

#[derive(Clone, Eq, Derivative)]
#[derivative(Debug, PartialEq, Hash)]
pub struct TypedFunctionName {
    pub ident: Ident,
    
    // if the function is a method, the type that implements this method.
    // either an interface type for interface method implementations, or the enclosing type
    // that is declaring its own method
    pub owning_ty: Option<Type>,

    #[derivative(Debug = "ignore")]
    #[derivative(Hash = "ignore")]
    #[derivative(PartialEq = "ignore")]
    pub span: Span,
}

impl TypedFunctionName {
    pub fn new_method(ident: impl Into<Ident>, owning_ty: impl Into<Type>, span: impl Into<Span>) -> Self {
        Self {
            ident: ident.into(),
            span: span.into(),
            owning_ty: Some(owning_ty.into()),
        }
    }
    
    pub fn new_free_func(ident: impl Into<Ident>, span: impl Into<Span>) -> Self {
        Self {
            ident: ident.into(),
            span: span.into(),
            owning_ty: None,
        }
    }
}

impl FunctionName for TypedFunctionName {    
    fn ident(&self) -> &Ident {
        &self.ident
    }
}

impl fmt::Display for TypedFunctionName {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if let Some(explicit_impl) = &self.owning_ty {
            write!(f, "{}.", explicit_impl)?;
        }
        write!(f, "{}", self.ident)
    }
}

pub fn typecheck_func_decl(
    decl: &ast::FunctionDecl<Span>,
    is_def: bool,
    ctx: &mut Context,
) -> TypecheckResult<FunctionDecl> {
    ctx.scope(Environment::FunctionDecl, |ctx| {
        let decl_mods = typecheck_decl_mods(&decl.mods, ctx)?;

        if is_def {
            if !decl.mods.is_empty() {
                return Err(TypeError::InvalidMethodModifiers {
                    mods: decl_mods,
                    span: decl.span.clone(),
                })
            }
        } else {
            if let Some(extern_mod) = decl.get_mod(DeclMod::EXTERNAL_WORD) {
                if let Some(decl_type_params) = &decl.type_params {
                    let ty_args_span = decl_type_params.items[0].name.span().to(decl_type_params
                        .items
                        .last()
                        .unwrap()
                        .name
                        .span());
                    return Err(TypeError::ExternalGenericFunction {
                        func: decl.name.ident.clone(),
                        extern_modifier: extern_mod.span().clone(),
                        ty_args: ty_args_span,
                    });
                }
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

        let params;
        let owning_ty;
        
        match (&decl.name.owning_ty_qual, ctx.current_enclosing_ty()) {
            // free function with no owning type
            (None, None) => {
                params = typecheck_params(decl, None, ctx)?;
                owning_ty = None;
            },
            
            // method definition or free interface implementation decl
            (Some(owning_ty_qual), None) => {
                let explicit_ty_span = owning_ty_qual.span();
                let explicit_owning_ty = typecheck_type(owning_ty_qual, ctx)?;

                // if this is an interface, it's the *implementation* of one, so it has the 
                // real types substituted into for any `Self` occurrences. if it's a method
                // definition, Self is a known type and should already be translated as the
                // actual type itself
                let implicit_self_ty = Some(explicit_owning_ty.clone());
                
                params = typecheck_params(decl, implicit_self_ty, ctx)?;

                let param_sigs = params.iter()
                    .cloned()
                    .map(FunctionParamSig::from)
                    .collect();
                let method_sig = FunctionSig::new(
                    return_ty.clone(),
                    param_sigs,
                    type_params.clone());

                match &explicit_owning_ty {
                    // free interface implementation decl
                    Type::Interface(iface) => {
                        validate_iface_method(
                            iface.as_ref(),
                            &decl.name.ident,
                            &method_sig,
                            ctx,
                            explicit_ty_span
                        )?;
                    }

                    // method definition
                    _ => match explicit_owning_ty.full_path() {
                        Some(ty_name) => {
                            validate_method(
                                &ty_name,
                                &decl.name.ident,
                                &method_sig,
                                ctx,
                                decl.span(),
                            ).map_err(|err| {
                                TypeError::from_name_err(err, decl.span.clone())
                            })?;
                        }
                        
                        None => {
                            return Err(TypeError::InvalidMethodInstanceType {
                                ty: explicit_owning_ty.clone(),
                                span: explicit_ty_span.clone(),
                            });
                        }
                    }
                }

                owning_ty = Some(explicit_owning_ty);
            }

            // method decl
            (None, Some(enclosing_ty)) => {
                let enclosing_ty = enclosing_ty.clone();

                // if the owning type is an interface, this is an interface method definition,
                // and the type of `self` is the generic `Self` (to stand in for implementing
                // types in static interfaces). if it's NOT an interface, it must be a concrete
                // type, in which case the type of self is just that type itself
                let self_param_ty = match &enclosing_ty {
                    Type::Interface(..) => Type::MethodSelf,
                    _ => enclosing_ty.clone(),
                };
                
                owning_ty = Some(enclosing_ty.clone());
                params = typecheck_params(decl, Some(self_param_ty), ctx)?;
            }

            (Some(owning_ty_qual), Some(..)) => {
                return Err(TypeError::InvalidMethodExplicitInterface {
                    method_ident: decl.name.ident.clone(),
                    span: owning_ty_qual.span().clone()
                });
            }
        };

        Ok(FunctionDecl {
            name: TypedFunctionName {
                ident: decl.name.ident.clone(),
                owning_ty,
                span: decl.name.span(),
            },
            params,
            type_params: type_params.clone(),
            return_ty: Some(return_ty),
            span: decl.span.clone(),
            mods: decl_mods,
        })
    })
}

impl FunctionDecl {
    pub fn is_implementation_of(&self, iface_ty: &Type, ctx: &Context) -> TypecheckResult<bool> {
        let owning_ty = match &self.name.owning_ty {
            // not a method/can't be an implementation
            None | Some(Type::Interface(..)) => return Ok(false),
            
            Some(ty) => ty,
        };

        let implements = owning_ty.implemented_ifaces_at(ctx, &self.span)?;
        if !implements.contains(iface_ty) {
            return Ok(false);
        }
        
        let sig = FunctionSig::of_decl(self);

        let methods = iface_ty.methods_at(ctx, &self.span)?;
        for impl_method in methods {
            if impl_method.name.ident == self.name.ident {
                let iface_sig = FunctionSig::of_decl(&impl_method).with_self(owning_ty);

                if iface_sig == sig {
                    return Ok(true);
                }
            }
        }
        
        Ok(false)
    }
}

fn typecheck_params(
    decl: &ast::FunctionDecl,
    implicit_self: Option<Type>,
    ctx: &mut Context
) -> TypecheckResult<Vec<FunctionParam>> {
    let mut params = Vec::new();

    if let Some(self_ty) = implicit_self {
        let self_span = decl.name.span().clone();

        params.push(FunctionParam {
            ty: self_ty,
            ident: Ident::new(SELF_PARAM_NAME, self_span.clone()),
            modifier: None,
            span: self_span,
        });
    }
    
    for param in &decl.params {
        let find_name_dup = params
            .iter()
            .find(|p| p.ident == param.ident);

        if let Some(prev) = find_name_dup {
            return Err(TypeError::DuplicateNamedArg {
                name: param.ident.clone(),
                span: param.span.clone(),
                previous: prev.span().clone(),
            });
        }

        let ty = typecheck_type(&param.ty, ctx)?;

        let param = FunctionParam {
            modifier: param.modifier.clone(),
            ident: param.ident.clone(),
            span: param.span.clone(),
            ty,
        };
        params.push(param);
    }

    Ok(params)
}

fn validate_iface_method(
    iface: &IdentPath,
    method_name: &Ident,
    method_sig: &FunctionSig,
    ctx: &Context,
    span: &Span
) -> TypecheckResult<()> {
    let iface_def = ctx.find_iface_def(&iface)
        .map_err(|err| {
            TypeError::from_name_err(err, span.clone())
        })?;

    find_iface_impl(iface_def, method_name, &method_sig)
        .map_err(|err| {
            TypeError::from_name_err(err, span.clone())
        })?;

    Ok(())
}

fn validate_method(
    owning_ty_name: &IdentPath,
    method_ident: &Ident,
    method_sig: &FunctionSig,
    ctx: &Context,
    def_span: &Span,
) -> NameResult<()> {
    let (_, owning_ty) = ctx.find_type(owning_ty_name)?;

    match owning_ty.get_method(method_ident, ctx)? {
        None => Err(NameError::MemberNotFound {
            base: NameContainer::Type(owning_ty.clone()),
            member: method_ident.clone(),
        }),
        Some(method_decl) => {
            let method_decl_sig = FunctionSig::of_decl(method_decl);

            if *method_sig != method_decl_sig {
                Err(NameError::DefDeclMismatch {
                    def: def_span.clone(),
                    decl: method_decl.span.clone(),
                    ident: owning_ty_name.clone().child(method_ident.clone()),
                })
            } else {
                Ok(())
            }
        }
    }
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

            ast::DeclMod::Inline(span) => DeclMod::Inline(span.clone()),

            ast::DeclMod::Forward(span) => DeclMod::Forward(span.clone()),

            ast::DeclMod::Overload(span) => {
                unimplemented!("function overload @ {}", span)
            }
        };

        results.push(result);
    }

    Ok(results)
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
struct InterfaceImpl {
    pub iface: Type,
    pub for_ty: Type,
}

fn find_iface_impl(
    iface_def: &InterfaceDecl,
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
    let decl = typecheck_func_decl(&def.decl, true, ctx)?;

    let body_env = FunctionBodyEnvironment {
        result_ty: decl.return_ty.clone().unwrap_or(Type::Nothing),
        ty_params: decl.type_params.clone(),
    };
    
    ctx.scope(body_env, |ctx| {
        let mut locals = Vec::new();
        for local in &def.locals {
            let ty = typecheck_type(&local.ty, ctx)?;
    
            let initial_val = match &local.initial_val {
                Some(expr) => {
                    let expr = typecheck_expr(expr, &ty, ctx)?;
                    let value = expr.const_eval(ctx).ok_or_else(|| TypeError::InvalidConstExpr {
                        expr: Box::new(expr.clone()),
                    })?;
    
                    Some(value)
                },
                None => None,
            };
    
            match local.kind {
                ast::BindingDeclKind::Const => {
                    let val = match &initial_val {
                        Some(val) => val,
                        None => {
                            return Err(TypeError::ConstDeclWithNoValue { span: local.span.clone() });
                        },
                    };
    
                    // the visibility doesn't really matter here because it's not a unit-level decl
                    let visiblity = Visibility::Interface;
                    ctx.declare_const(local.ident.clone(), val.clone(), ty.clone(), visiblity, local.span.clone())?;
                },
    
                ast::BindingDeclKind::Var => {
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
    
        if decl.name.owning_ty.is_none() {
            // functions are always declared within their own bodies (allowing recursive calls)
            // but forward-declared functions may already be present in the scope - in which case we
            // don't need to declare it again
            let find_existing_decl = ctx.find_function(&IdentPath::from(decl.name.ident().clone()));
            if find_existing_decl.is_err() {
                ctx.declare_function(decl.name.ident().clone(), &decl, Visibility::Implementation)?;
            }
        }
    
        // declare decl's type params within the body too
        if let Some(decl_type_params) = decl.type_params.as_ref() {
            ctx.declare_type_params(&decl_type_params)?;
        };
    
        declare_func_params_in_body(&decl.params, ctx)?;
    
        let body = typecheck_block(&def.body, decl.return_ty.as_ref().unwrap(), ctx)?;

        Ok(FunctionDef {
            decl,
            locals,
            body,
            span: def.span.clone(),
        })
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
    args: &TypeList,
    ctx: &Context,
) -> GenericResult<FunctionDecl> {
    FunctionSig::of_decl(&decl)
        .validate_type_args(args, ctx)?;

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
    expect_ty: &Type,
    ctx: &mut Context,
) -> TypecheckResult<AnonymousFunctionDef> {
    let expect_sig = expect_ty.as_func().ok();

    let mut params = Vec::new();
    for (i, param) in src_def.params.iter().enumerate() {
        let ty = if param.ty.is_known() {
            typecheck_type(&param.ty, ctx)?
        } else {
            match expect_sig.and_then(|sig| sig.params.get(i)) {
                Some(expect_param) if expect_param.modifier.is_none() => expect_param.ty.clone(),
                _ => {
                    return Err(TypeError::UnableToInferFunctionExprType {
                        func: Box::new(src_def.clone()),
                    });
                }
            }
        };

        params.push(FunctionParam {
            modifier: param.modifier.clone(),
            ident: param.ident.clone(),
            span: param.span.clone(),
            ty,
        });
    }

    let return_ty = match &src_def.return_ty {
        Some(ast::TypeName::Unknown(..)) => match expect_sig.map(|sig| &sig.return_ty){
            Some(expect_return_ty) => expect_return_ty.clone(),
            None => {
                return Err(TypeError::UnableToInferFunctionExprType {
                    func: Box::new(src_def.clone()),
                })
            }
        },
        Some(src_return_ty) => typecheck_type(src_return_ty, ctx)?,
        None => Type::Nothing,
    };

    let sig_params = params.iter().map(|p| p.clone().into()).collect();

    let sig = Rc::new(FunctionSig {
        return_ty: return_ty.clone(),
        params: sig_params,
        type_params: None,
    });

    let body_scope_id = ctx.push_scope(ClosureBodyEnvironment {
        result_ty: Some(return_ty.clone()),
        captures: LinkedHashMap::new(),
    });

    let body_result = declare_func_params_in_body(&params, ctx).and_then(|_| {
        typecheck_block(&src_def.body, &return_ty, ctx)
    });

    let closure_env = match ctx.pop_scope(body_scope_id).into_env() {
        Environment::ClosureBody(body) => body,
        _ => unreachable!(),
    };
    
    let body = body_result?;

    let annotation = TypedValue {
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
        captures: closure_env.captures,
    })
}
