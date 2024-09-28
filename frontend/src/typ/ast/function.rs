use crate::ast;
use crate::ast::FunctionName;
use crate::ast::Ident;
use crate::ast::IdentPath;
use crate::ast::TypeAnnotation;
use crate::ast::Visibility;
use crate::typ::ast::const_eval::ConstEval;
use crate::typ::ast::const_eval_string;
use crate::typ::ast::typecheck_block;
use crate::typ::ast::typecheck_expr;
use crate::typ::{typecheck_type, TypeArgList};
use crate::typ::typecheck_type_params;
use crate::typ::typecheck_type_path;
use crate::typ::Binding;
use crate::typ::ClosureBodyEnvironment;
use crate::typ::Context;
use crate::typ::Environment;
use crate::typ::FunctionBodyEnvironment;
use crate::typ::FunctionParamSig;
use crate::typ::FunctionSig;
use crate::typ::GenericResult;
use crate::typ::NameContainer;
use crate::typ::NameError;
use crate::typ::NameResult;
use crate::typ::Type;
use crate::typ::TypeError;
use crate::typ::TypeResult;
use crate::typ::Typed;
use crate::typ::TypedValue;
use crate::typ::ValueKind;
use crate::typ::{string_type, TypeParamType};
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
pub type FunctionLocalBinding = ast::FunctionLocalBinding<Typed>;

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
) -> TypeResult<FunctionDecl> {
    let enclosing_ty = ctx.current_enclosing_ty().cloned();

    let owning_ty = match (&decl.name.owning_ty_qual, &enclosing_ty) {
        (Some(owning_ty_name), None) => {
            Some(typecheck_type_path(owning_ty_name, ctx)?)
        },

        (Some(type_qual), Some(..)) => {
            return Err(TypeError::InvalidMethodExplicitInterface {
                method_ident: decl.name.ident.clone(),
                span: type_qual.span().clone()
            });
        }

        (None, Some(enclosing_ty)) => Some(enclosing_ty.clone()),

        _ => None,
    };

    let env = Environment::FunctionDecl {
        owning_ty_params: owning_ty
            .as_ref()
            .and_then(|ty| ty.type_params().cloned()),
    };
    
    ctx.scope(env, |ctx| {
        // declare type params from the declaring type, if any
        // e.g. for method `MyClass[T].A()`, `T` is declared here for the function scope
        // if this decl is inside an enclosing type, they should already be declared in the body
        // scope of the type, and can't be redeclared
        if enclosing_ty.is_none() {
            if let Some(params) = owning_ty.as_ref().and_then(Type::type_params) {
                ctx.declare_type_params(params)?;
            }
        }
        
        let decl_mods = typecheck_decl_mods(&decl.mods, ctx)?;

        if is_def {
            if decl.name.owning_ty_qual.is_some() && !decl.mods.is_empty() {
                return Err(TypeError::InvalidMethodModifiers {
                    mods: decl_mods,
                    span: decl.span.clone(),
                })
            }
        } else if let Some(extern_mod) = decl.get_mod(DeclMod::EXTERNAL_WORD) {
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

        let params: Vec<FunctionParam>;
        
        match (&owning_ty, &enclosing_ty) {
            // free function with no owning type
            (None, None) => {
                params = typecheck_params(decl, None, ctx)?;
            },
            
            // method definition or free interface implementation decl
            (Some(explicit_owning_ty), None) => {
                let explicit_ty_span = decl
                    .name
                    .owning_ty_qual
                    .as_ref()
                    .unwrap()
                    .span();

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
                    Type::Interface(..) => {
                        return Err(TypeError::AbstractMethodDefinition {
                            span: decl.span.clone(),
                            owning_ty: explicit_owning_ty.clone(),
                            method: decl.name.ident.clone(),
                        });
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
            }

            // method decl
            (_, Some(enclosing_ty)) => {
                // it may be possible later to specify an explicit implemented interface within the
                // body of another type, in which case this assertion would no longer be valid
                assert_eq!(
                    owning_ty.as_ref(), 
                    Some(enclosing_ty), 
                    "if an enclosing type is found, the owning type should always be the enclosing type"
                );
                
                // if the owning type is an interface, this is an interface method definition,
                // and the type of `self` is the generic `Self` (to stand in for implementing
                // types in static interfaces). if it's NOT an interface, it must be a concrete
                // type, in which case the type of self is just that type itself
                let self_param_ty = match &enclosing_ty {
                    Type::Interface(..) => Type::MethodSelf,
                    _ => enclosing_ty.clone(),
                };

                params = typecheck_params(decl, Some(self_param_ty), ctx)?;
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
    pub fn is_implementation_of(&self, iface_ty: &Type, ctx: &Context) -> TypeResult<bool> {
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
) -> TypeResult<Vec<FunctionParam>> {
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
) -> TypeResult<Vec<DeclMod>> {
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

pub fn typecheck_func_def(
    def: &ast::FunctionDef<Span>,
    ctx: &mut Context,
) -> TypeResult<FunctionDef> {
    let mut decl = typecheck_func_decl(&def.decl, true, ctx)?;

    // in the body of a method definition, the type parameters of the enclosing type are
    // used to specialize the types in the decl
    if let Some(outer_ty_params) = decl.name.owning_ty.as_ref().and_then(|ty| ty.type_params()) {
        let implicit_ty_args = outer_ty_params
            .clone()
            .map(|item, pos| Type::GenericParam(Box::new(TypeParamType {
                name: item.name,
                pos,
                is_iface: item
                    .constraint
                    .map(|constraint| Box::new(constraint.is_ty))
            })));
        
        decl = specialize_func_decl(&decl, &implicit_ty_args, ctx)
            .map_err(|err| TypeError::from_generic_err(err, decl.span.clone()))?;
    }
    
    let return_ty = decl.return_ty.clone().unwrap_or(Type::Nothing);

    let body_env = FunctionBodyEnvironment {
        result_ty: return_ty,
        ty_params: decl.type_params.clone(),
    };
    
    ctx.scope(body_env, |ctx| {
        match &decl.name.owning_ty {
            // free functions are always declared within their own bodies (allowing recursive calls)
            // but forward-declared functions may already be present in the scope - in which case we
            // don't need to declare it again
            None => {
                let find_existing_decl = ctx.find_function(&IdentPath::from(decl.name.ident().clone()));
                if find_existing_decl.is_err() {
                    ctx.declare_function(decl.name.ident().clone(), &decl, Visibility::Implementation)?;
                }
            }

            // declare type parameters from the owning type, if this is a method
            Some(owning_ty) => {
                if let Some(enclosing_ty_params) = owning_ty.type_params() {
                    ctx.declare_type_params(enclosing_ty_params)?;
                }
            }
        }
    
        // declare decl's own type params within the body too
        if let Some(decl_type_params) = decl.type_params.as_ref() {
            ctx.declare_type_params(&decl_type_params)?;
        }
    
        declare_func_params_in_body(&decl.params, ctx)?;

        let locals = declare_locals_in_body(&def, ctx)?;
    
        let body = typecheck_block(&def.body, decl.return_ty.as_ref().unwrap(), ctx)?;

        Ok(FunctionDef {
            decl,
            locals,
            body,
            span: def.span.clone(),
        })
    })
}

fn declare_locals_in_body(
    def: &ast::FunctionDef,
    ctx: &mut Context
) -> TypeResult<Vec<FunctionLocalBinding>> {
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

        locals.push(FunctionLocalBinding {
            ident: local.ident.clone(),
            kind: local.kind,
            initial_val,
            ty,
            span: local.span.clone(),
        });
    }
    
    Ok(locals)
}

fn declare_func_params_in_body(params: &[FunctionParam], ctx: &mut Context) -> TypeResult<()> {
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
    args: &TypeArgList,
    ctx: &Context,
) -> GenericResult<FunctionDecl> {
    let mut params = Vec::new();
    for param in decl.params.iter() {
        let ty = param.ty.specialize_generic(args, ctx)?.into_owned();
        params.push(FunctionParam {
            ty,
            ..param.clone()
        });
    }

    let return_ty = match &decl.return_ty {
        Some(return_ty) => {
            let ty = return_ty.specialize_generic(args, ctx)?.into_owned();
            Some(ty)
        },
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
) -> TypeResult<AnonymousFunctionDef> {
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
