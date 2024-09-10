#[cfg(test)]
mod test;

use crate::{
    ast::cast::implicit_conversion,
    ast::{
        check_implicit_conversion, typecheck_expr, typecheck_object_ctor, Expr, FunctionDecl,
        ObjectCtor,
    },
    context::InstanceMethod,
    typecheck_type, Context, FunctionAnnotation, FunctionParamSig, FunctionSig, GenericError,
    GenericTarget, GenericTypeHint, InterfaceMethodAnnotation, NameContainer, NameError,
    OverloadAnnotation, Specializable, Type, TypeAnnotation, TypeArgsResolver, TypeArgsResult,
    TypeParamType, TypecheckError, TypecheckResult, TypedValueAnnotation, UfcsFunctionAnnotation,
    ValueKind,
};
use pas_common::span::{Span, Spanned as _};
use pas_syn::{
    ast,
    ast::{FunctionParamMod, TypeList},
    Ident, IdentPath,
};
use std::{borrow::Cow, fmt, rc::Rc};

pub type MethodCall = ast::MethodCall<TypeAnnotation>;
pub type FunctionCall = ast::FunctionCall<TypeAnnotation>;
pub type FunctionCallNoArgs = ast::FunctionCallNoArgs<TypeAnnotation>;
pub type MethodCallNoArgs = ast::MethodCallNoArgs<TypeAnnotation>;
pub type VariantCtorCall = ast::VariantCtorCall<TypeAnnotation>;
pub type Call = ast::Call<TypeAnnotation>;

// it's possible that during typechecking we discover what was parsed as a call with zero args
// is actually the ctor for a class with no fields
pub enum Invocation {
    Call(Box<Call>),
    Ctor(Box<ObjectCtor>),
}

fn invalid_args(
    actual_args: Vec<Expr>,
    expected: &[FunctionParamSig],
    span: Span,
) -> TypecheckError {
    TypecheckError::InvalidArgs {
        expected: expected.iter().map(|p| p.ty.clone()).collect(),
        actual: actual_args
            .into_iter()
            .map(|arg| arg.annotation().ty().into_owned())
            .collect(),
        span,
    }
}

fn build_args_for_params(
    params: &[FunctionParamSig],
    src_args: &[ast::Expr<Span>],
    self_arg: Option<&Expr>,
    span: &Span,
    ctx: &mut Context,
) -> TypecheckResult<Vec<Expr>> {
    let mut checked_args = Vec::new();

    let rest_args = if let Some(self_arg) = self_arg {
        let self_ty = &params[0].ty;

        let self_arg = implicit_conversion(self_arg.clone(), self_ty, ctx)?;
        checked_args.push(self_arg);

        &params[1..]
    } else {
        params
    };

    // typecheck each arg (don't do conversions yet, we haven't figured out the self ty yet)
    for (arg, expected_param) in src_args.iter().zip(rest_args.iter()) {
        let arg_expr = typecheck_expr(arg, &expected_param.ty, ctx)?;
        checked_args.push(arg_expr);
    }

    // does arg count match expected arg count?
    if checked_args.len() != params.len() {
        return Err(invalid_args(checked_args, params, span.clone()));
    }

    // find the self ty - take the actual type of the first arg that is passed to a Self-typed param
    let mut self_ty: Option<Type> = None;
    let mut params = params.to_vec();

    for i in 0..params.len() {
        let expected = &params[i];
        if expected.ty != Type::MethodSelf {
            continue;
        }

        match &self_ty {
            // this is the first arg passed as a Self param, use this as the self ty from now on
            None => {
                let actual_self_ty = checked_args[i].annotation().ty().into_owned();
                params[i].ty = actual_self_ty.clone();
                self_ty = Some(actual_self_ty);
            },

            // we have already deduced a self ty and are using that one
            Some(actual_self_ty) => {
                params[i].ty = actual_self_ty.clone();
            },
        }
    }

    validate_args(&mut checked_args, &params, span, ctx)?;

    Ok(checked_args)
}

fn typecheck_type_args(
    type_args: &TypeList<ast::TypeName>,
    ctx: &mut Context,
) -> TypecheckResult<TypeList<Type>> {
    let items: Vec<_> = type_args
        .items
        .iter()
        .map(|arg_ty| typecheck_type(arg_ty, ctx))
        .collect::<TypecheckResult<_>>()?;

    Ok(TypeList::new(items, type_args.span().clone()))
}

pub fn typecheck_call(
    call: &ast::Call<Span>,
    expect_ty: &Type,
    ctx: &mut Context,
) -> TypecheckResult<Invocation> {
    let func_call = match call {
        ast::Call::Function(func_call) => func_call,
        _ => unreachable!("parsing cannot result in anything except FunctionCall"),
    };

    // if the call target is a no-args call itself and this is also a no-args call, we are just
    // applying an empty argument list to the same call, so just unwrap it here
    // e.g. if we call `procedure X;` with `X()`, `X` is already a valid call on its own
    // there are edge cases here like functions that return callable types themselves, but
    // they can be disambiguated by various means in the code
    let target = match typecheck_expr(&func_call.target, &Type::Nothing, ctx)? {
        Expr::Call(call) => match *call {
            Call::FunctionNoArgs(call) => call.target,
            call => Expr::from(call),
        }

        target => target,
    };

    let expr = match target.annotation() {
        TypeAnnotation::TypedValue(val) => match &val.ty {
            Type::Function(sig) => {
                let sig = sig.clone();
                typecheck_func_call(target, &func_call, &sig, ctx)
                    .map(Box::new)
                    .map(Invocation::Call)?
            },

            _ => match target {
                // when making a function call with an empty args list, and the target is a
                // call to a no-args function, this "inner" call replaces the outer call entirely
                // since the extra arg list is redundant
                Expr::Call(inner_call) 
                    if func_call.args.len() == 0 && inner_call.args().len() == 0 => {
                    Invocation::Call(inner_call)
                }
                
                _ => return Err(TypecheckError::NotCallable(Box::new(target))),
            },
        },

        TypeAnnotation::Function(func) => {
            let sig = func.sig.clone();
            typecheck_func_call(target, &func_call, &sig, ctx)
                .map(Box::new)
                .map(Invocation::Call)?
        },

        TypeAnnotation::UfcsFunction(ufcs_call) => {
            let typecheck_call = typecheck_ufcs_call(
                &ufcs_call,
                &func_call.args,
                func_call.annotation.span(),
                &func_call.args_span,
                ctx,
            );

            typecheck_call.map(Box::new).map(Invocation::Call)?
        },

        TypeAnnotation::Overload(overloaded) => {
            typecheck_func_overload(ctx, func_call, &target, &overloaded)
                .map(Box::new)
                .map(Invocation::Call)?
        },

        TypeAnnotation::InterfaceMethod(iface_method) => {
            typecheck_iface_method_call(iface_method, func_call, ctx)
                .map(Box::new)
                .map(Invocation::Call)?
        },

        TypeAnnotation::Type(ty, ..) if func_call.args.is_empty() && ty.full_path().is_some() => {
            let args_span = func_call.args_span.clone();
            let ctor = ast::ObjectCtor {
                ident: Some(ty.full_path().unwrap()),
                annotation: call.span().clone(),
                args: ast::ObjectCtorArgs {
                    span: args_span,
                    members: Vec::new(),
                },
            };

            let span = ctor.annotation.span().clone();

            typecheck_object_ctor(&ctor, span, expect_ty, ctx)
                .map(Box::new)
                .map(Invocation::Ctor)?
        },

        TypeAnnotation::VariantCtor(variant, ..) => typecheck_variant_ctor_call(
            &variant.variant_name,
            &variant.case,
            &func_call.args,
            call.span().clone(),
            expect_ty,
            ctx,
        )
        .map(Box::new)
        .map(Invocation::Call)?,

        _ => {
            return Err(TypecheckError::NotCallable(Box::new(target)))
        },
    };

    Ok(expr)
}

fn typecheck_func_overload(
    ctx: &mut Context,
    func_call: &ast::FunctionCall<Span>,
    target: &Expr,
    overloaded: &OverloadAnnotation,
) -> TypecheckResult<Call> {
    let overload = resolve_overload(
        &overloaded.candidates,
        &func_call.args,
        overloaded.self_arg.as_ref().map(|arg| arg.as_ref()),
        &overloaded.span,
        ctx,
    )?;

    let args_span = func_call.args_span.clone();

    let call = match &overloaded.candidates[overload.selected_sig] {
        OverloadCandidate::Method {
            iface_ty,
            ident,
            sig,
            ..
        } => {
            let self_type = match &overloaded.self_arg {
                Some(self_arg) => self_arg.annotation().ty().into_owned(),

                None => {
                    let arg_tys: Vec<_> = overload
                        .args
                        .iter()
                        .map(|a| a.annotation().ty().into_owned())
                        .collect();

                    match sig.self_ty_from_args(&arg_tys) {
                        Some(self_ty) => self_ty.clone(),
                        None => {
                            return Err(TypecheckError::AmbiguousSelfType {
                                span: overloaded.span.clone(),
                                method: ident.clone(),
                                iface: iface_ty.clone(),
                            })
                        },
                    }
                },
            };

            let sig = Rc::new(sig.with_self(&self_type));

            let return_annotation = TypedValueAnnotation {
                span: overloaded.span.clone(),
                ty: sig.return_ty.clone(),
                decl: Some(ident.clone()),
                value_kind: ValueKind::Temporary,
            }
            .into();

            let method_call = MethodCall {
                annotation: return_annotation,
                ident: ident.clone(),
                args: overload.args,
                type_args: overload.type_args,
                self_type,
                func_type: Type::Function(sig.clone()),
                iface_type: iface_ty.clone(),
                args_span,
            };

            ast::Call::Method(method_call)
        },

        OverloadCandidate::Function { decl_name, sig } => {
            let return_annotation = TypedValueAnnotation {
                span: overloaded.span.clone(),
                ty: sig.return_ty.clone(),
                decl: Some(decl_name.last().clone()),
                value_kind: ValueKind::Temporary,
            }
            .into();

            let func_call = FunctionCall {
                annotation: return_annotation,
                args: overload.args,
                type_args: overload.type_args.clone(),
                target: target.clone(),
                args_span,
            };

            ast::Call::Function(func_call)
        },
    };

    Ok(call)
}

fn typecheck_iface_method_call(
    iface_method: &InterfaceMethodAnnotation,
    func_call: &ast::FunctionCall<Span>,
    ctx: &mut Context,
) -> TypecheckResult<Call> {
    // not yet supported
    if let Some(call_type_args) = &func_call.type_args {
        return Err(TypecheckError::from_generic_err(
            GenericError::ArgsLenMismatch {
                expected: 0,
                actual: call_type_args.len(),
                target: GenericTarget::FunctionSig((*iface_method.method_sig).clone()),
            },
            call_type_args.span().clone(),
        ));
    }

    // branch the context to check the self-arg, because we're about to re-check it in a second
    let self_type = {
        let mut ctx = ctx.clone();
        let first_self_pos = iface_method
            .method_sig
            .params
            .iter()
            .position(|param| param.ty == Type::MethodSelf)
            .expect("method function must have at least one argument with Self type");

        // note that this isn't the "self arg" used for typecheck_args, because we're not passing
        // it implicitly as a separate arg (like the self-arg `x` of `x.Y()` in a UFCS call).
        // it's just the first arg from which we can infer the self-type
        let first_self_arg =
            typecheck_expr(&func_call.args[first_self_pos], &Type::Nothing, &mut ctx)?;

        first_self_arg.annotation().ty().into_owned()
    };

    let iface_ident = match iface_method.iface_ty.as_iface() {
        Ok(iface_ident) => iface_ident,
        Err(not_iface) => panic!(
            "expect all method-defining types to be interfaces currently, found: {}",
            not_iface
        ),
    };

    if !ctx.is_iface_impl(&self_type, iface_ident) {
        return Err(TypecheckError::InterfaceNotImplemented {
            iface_ty: iface_method.iface_ty.clone(),
            span: func_call.span().clone(),
            self_ty: self_type,
        });
    }

    let sig = iface_method.method_sig.with_self(&self_type);

    let typechecked_args =
        build_args_for_params(&sig.params, &func_call.args, None, func_call.span(), ctx)?;

    Ok(ast::Call::Method(MethodCall {
        annotation: TypedValueAnnotation {
            ty: sig.return_ty.clone(),
            span: func_call.span().clone(),
            value_kind: ValueKind::Temporary,
            decl: None,
        }
        .into(),
        args_span: func_call.args_span.clone(),
        func_type: Type::Function(Rc::new(sig)),
        self_type,
        iface_type: iface_method.iface_ty.clone(),
        ident: iface_method.method_ident.clone(),
        type_args: None,
        args: typechecked_args,
    }))
}

fn typecheck_ufcs_call(
    ufcs_call: &UfcsFunctionAnnotation,
    rest_args: &[ast::Expr<Span>],
    span: &Span,
    args_span: &Span,
    ctx: &mut Context,
) -> TypecheckResult<Call> {
    let mut specialized_call_args = specialize_call_args(
        &ufcs_call.sig,
        &rest_args,
        Some(&ufcs_call.self_arg),
        None, // ufcs call can't have explicit type args
        &span,
        ctx,
    )?;
    validate_args(
        &mut specialized_call_args.actual_args,
        &specialized_call_args.sig.params,
        span,
        ctx,
    )?;

    let func_annotation = FunctionAnnotation {
        sig: Rc::new(specialized_call_args.sig.clone()),
        ident: ufcs_call.function_name.clone(),
        span: span.clone(),
        type_args: specialized_call_args.type_args.clone(),
    }
    .into();

    // todo: this should construct a fully qualified path expr instead
    let target = ast::Expr::Ident(ufcs_call.function_name.last().clone(), func_annotation);

    let annotation = TypedValueAnnotation {
        ty: specialized_call_args.sig.return_ty.clone(),
        span: span.clone(),
        decl: None,
        value_kind: ValueKind::Temporary,
    }
    .into();

    Ok(ast::Call::Function(FunctionCall {
        args: specialized_call_args.actual_args,
        args_span: args_span.clone(),
        annotation,
        type_args: specialized_call_args.type_args,
        target,
    }))
}

struct SpecializedCallArgs {
    sig: FunctionSig,
    type_args: Option<TypeList<Type>>,
    actual_args: Vec<Expr>,
}

struct PartiallySpecializedTypeArgsList<'a> {
    items: &'a [Option<Type>],
}

impl<'a> TypeArgsResolver for PartiallySpecializedTypeArgsList<'a> {
    fn resolve(&self, param: &TypeParamType) -> Cow<Type> {
        match self.items.get(param.pos) {
            Some(Some(specialized_ty)) => Cow::Borrowed(specialized_ty),
            _ => Cow::Owned(Type::GenericParam(Box::new(param.clone()))),
        }
    }

    fn get_specialized(&self, pos: usize) -> Option<&Type> {
        self.items.get(pos)?.as_ref()
    }

    fn len(&self) -> usize {
        self.items.len()
    }
}

fn specialize_arg<ArgProducer>(
    param_ty: &Type,
    inferred_ty_args: &mut [Option<Type>],
    produce_expr: ArgProducer,
    _span: &Span,
    ctx: &mut Context,
) -> TypecheckResult<Expr>
// (expected type, ctx) -> expr val
where
    ArgProducer: FnOnce(&Type, &mut Context) -> TypecheckResult<Expr>,
{
    let partial_args_resolver = PartiallySpecializedTypeArgsList {
        items: inferred_ty_args,
    };

    let mut expect_ty = param_ty
        .clone()
        .substitute_type_args(&partial_args_resolver);
    
    if expect_ty.is_generic_param() || expect_ty.is_unspecialized_generic() {
        // not enough info to resolve this generic type fully yet: arg expr ty drives param type
        expect_ty = Type::Nothing;
    }

    let actual_arg =  produce_expr(&expect_ty, ctx)?;
    let actual_ty = actual_arg.annotation().ty().clone();
    
    infer_from_structural_ty_args(param_ty, &actual_ty, inferred_ty_args);

    Ok(actual_arg)
}

fn infer_from_structural_ty_args(
    param_ty: &Type,
    actual_ty: &Type,
    inferred_ty_args: &mut [Option<Type>],
) {
    // in param_ty, find all the references to generic params within the type and their
    // corresponding values in the actual type, in the order they appear.
    // for example if the expected type is `array of T`, and the actual
    // arg is `array of Int32`, `T` is added to the param ty args and `Int32` is added to the 
    // actual ty args.
    // at this point, if the types are fundamentally incompatible we should bail
    let (param_ty_args, actual_ty_args) = match (param_ty, actual_ty) {
        // plain generic param can be substituted for anything
        (Type::GenericParam(..), actual) => {
            (vec![param_ty.clone()], vec![actual.clone()])
        }

        // static-length arrays can be substituted into a generic array of the same length 
        (Type::Array(param_array_ty), Type::Array(actual_array_ty)) => {
            if actual_array_ty.dim != param_array_ty.dim {
                return;
            }

            (
                vec![param_array_ty.element_ty.clone()],
                vec![actual_array_ty.element_ty.clone()],
            )
        },

        // dynarrays only need element type to match
        (Type::DynArray { element: param_el }, Type::DynArray { element: actual_el }) => {
            (vec![*param_el.clone()], vec![*actual_el.clone()])
        },

        _ => {
            // all other types: must be the exact same declaration (struct, enum etc)
            if !param_ty.same_decl_type(actual_ty) {
                return;
            }

            let param_ty_args = match param_ty.type_args() {
                TypeArgsResult::Specialized(args) => args.items.clone(),
                _ => return,
            };
            let actual_ty_args = match actual_ty.type_args() {
                TypeArgsResult::Specialized(args) => args.items.clone(),
                _ => return,
            };
            (param_ty_args, actual_ty_args)
        },
    };

    let all_ty_args = param_ty_args.iter().zip(actual_ty_args.iter());
    for (param_ty_arg, actual_ty_arg) in all_ty_args {
        match param_ty_arg {
            Type::GenericParam(param_generic) => {
                let pos = param_generic.pos;
                let inferred = &mut inferred_ty_args[pos];
                assert!(
                    inferred.is_none() || inferred.as_ref().unwrap() == actual_ty_arg, 
                    "previously inferred type for this position ({}) must either be None for the first occurrence, or the same type as this inference ({})",
                    pos,
                    actual_ty_arg
                );

                *inferred = Some(actual_ty_arg.clone());
            },

            _ => infer_from_structural_ty_args(param_ty_arg, actual_ty_arg, inferred_ty_args),
        }
    }
}

/// for a function decl, figure out the actual arg types and values from the provided
/// expressions. evaluate arguments left-to-right until we either resolve a signature that
/// matches the args, or conclude that the args don't match the call and return an error.
///
/// for example `function A[T](t: T; t2: array[2] of T)`' s actual signature can be deduced
/// from the actual type of `t`, and `t2`'s value can be checked against that.
fn specialize_call_args(
    decl_sig: &FunctionSig,
    args: &[ast::Expr<Span>],
    self_arg: Option<&Expr>,
    explicit_ty_args: Option<TypeList<Type>>,
    span: &Span,
    ctx: &mut Context,
) -> TypecheckResult<SpecializedCallArgs> {
    if decl_sig.type_params.is_none() {
        if let Some(explicit_ty_args) = explicit_ty_args {
            return Err(TypecheckError::from_generic_err(
                GenericError::ArgsLenMismatch {
                    expected: 0,
                    actual: explicit_ty_args.len(),
                    target: GenericTarget::FunctionSig(decl_sig.clone()),
                },
                span.clone(),
            ));
        }

        // no type params
        let actual_args = build_args_for_params(&decl_sig.params, args, self_arg, span, ctx)?;

        return Ok(SpecializedCallArgs {
            sig: decl_sig.clone(),
            actual_args,
            type_args: None,
        });
    }

    if let Some(explicit_ty_args) = explicit_ty_args {
        // we have explicit args, don't try to infer types
        let sig = decl_sig
            .clone()
            .specialize_generic(&explicit_ty_args, ctx)
            .map_err(|err| TypecheckError::from_generic_err(err, span.clone()))?;

        let actual_args = build_args_for_params(&sig.params, args, self_arg, span, ctx)?;

        Ok(SpecializedCallArgs {
            sig,
            actual_args,
            type_args: Some(explicit_ty_args),
        })
    } else {
        // we haven't checked arg length matches yet because we haven't typechecked args
        let self_arg_len = if self_arg.is_some() { 1 } else { 0 };
        if args.len() + self_arg_len != decl_sig.params.len() {
            // this is an inferral error because we don't have enough information to report
            // it as an arg type mismatch
            return Err(TypecheckError::from_generic_err(
                GenericError::CannotInferArgs {
                    target: GenericTarget::FunctionSig(decl_sig.clone()),
                    hint: GenericTypeHint::Unknown,
                },
                span.clone(),
            ));
        }

        // try to infer type from args, left to right
        let mut inferred_ty_args = vec![None; decl_sig.type_params.as_ref().unwrap().len()];
        let mut actual_args = Vec::new();

        if let Some(self_arg) = self_arg.cloned() {
            let decl_self_param = &decl_sig.params[0];
            let actual_self_arg = specialize_arg(
                &decl_self_param.ty,
                &mut inferred_ty_args,
                |_expect_ty, _ctx| Ok(self_arg),
                span,
                ctx,
            )?;

            actual_args.push(actual_self_arg);
        }

        for (i, arg) in args.iter().enumerate() {
            let param_ty = &decl_sig.params[i + self_arg_len].ty;
            let actual_arg = specialize_arg(
                param_ty,
                &mut inferred_ty_args,
                |expect_ty, ctx| typecheck_expr(arg, expect_ty, ctx),
                span,
                ctx,
            )?;

            actual_args.push(actual_arg);
        }

        let inferred_ty_args =
            unwrap_inferred_args(decl_sig, inferred_ty_args, &actual_args, span)?;

        let actual_sig = decl_sig
            .clone()
            .specialize_generic(&inferred_ty_args, ctx)
            .map_err(|err| TypecheckError::from_generic_err(err, span.clone()))?;

        // eprintln!("INFERRED ARGS:\n\tdecl: {}\n\tinferred:{}\n\tfinal sig: {}", decl_sig, inferred_ty_args, actual_sig);

        Ok(SpecializedCallArgs {
            type_args: Some(inferred_ty_args),
            sig: actual_sig,
            actual_args,
        })
    }
}

/// unwrap all the type args or raise an error if any of them can't be unwrapped because
/// we haven't successfully inferred them
fn unwrap_inferred_args(
    decl_sig: &FunctionSig,
    inferred_args: Vec<Option<Type>>,
    actual_args: &[Expr],
    span: &Span,
) -> TypecheckResult<TypeList<Type>> {
    let mut items = Vec::new();

    for arg in inferred_args {
        match arg {
            Some(arg) => {
                items.push(arg);
            },

            None => {
                let arg_tys = actual_args
                    .iter()
                    .map(|a| a.annotation().ty().into_owned())
                    .collect();

                return Err(TypecheckError::from_generic_err(
                    GenericError::CannotInferArgs {
                        target: GenericTarget::FunctionSig(decl_sig.clone()),
                        hint: GenericTypeHint::ArgTypes(arg_tys),
                    },
                    span.clone(),
                ));
            },
        }
    }

    Ok(TypeList::new(items, span.clone()))
}

/// final arg validation when all arg types are known and specialized correctly, and any
/// self-args have been processed into normal arguments to match the sig
/// - insert any required implicit type conversions
/// - validate length
/// - ensure that the expressions provided for out/var refs are mutable l-values
/// - mark the names referenced in out vars as initialized
fn validate_args(
    args: &mut [Expr],
    params: &[FunctionParamSig],
    span: &Span,
    ctx: &mut Context,
) -> TypecheckResult<()> {
    if args.len() != params.len() {
        return Err(invalid_args(args.to_vec(), params, span.clone()));
    }

    for i in 0..params.len() {
        // only do implicit conversion for non-ref params
        if params[i].is_by_ref() {
            continue;
        }

        args[i] =
            implicit_conversion(args[i].clone(), &params[i].ty, ctx).map_err(|err| match err {
                TypecheckError::TypeMismatch { .. } => {
                    invalid_args(args.to_vec(), params, span.clone())
                },
                err => err,
            })?;
    }

    let args_and_params = args.iter().zip(params.iter());
    for (arg, param) in args_and_params {
        let (is_in_ref, is_out_ref) = match &param.modifier {
            None => (false, false),
            Some(FunctionParamMod::Out) => (false, true),
            Some(FunctionParamMod::Var) => (true, true),
        };

        if is_in_ref || is_out_ref {
            match arg.annotation().value_kind() {
                // in-refs shouldn't really allow uninitialized values here but we do init checking
                // in a separate pass
                Some(ValueKind::Mutable) | Some(ValueKind::Uninitialized) => {},
                _ => {
                    return Err(TypecheckError::NotMutable {
                        expr: Box::new(arg.clone()),
                        decl: None,
                    });
                },
            }

            if is_out_ref {
                if let Expr::Ident(ref_ident, ..) = &arg {
                    ctx.initialize(ref_ident);
                }
            }
        }
    }

    Ok(())
}

fn typecheck_func_call(
    target: Expr,
    func_call: &ast::FunctionCall<Span>,
    sig: &FunctionSig,
    ctx: &mut Context,
) -> TypecheckResult<Call> {
    let span = func_call.span().clone();

    let type_args = match func_call.type_args.as_ref() {
        Some(call_type_args) => Some(typecheck_type_args(call_type_args, ctx)?),
        None => None,
    };

    let mut specialized_call_args =
        specialize_call_args(sig, &func_call.args, None, type_args, &span, ctx)?;
    validate_args(
        &mut specialized_call_args.actual_args,
        &specialized_call_args.sig.params,
        func_call.span(),
        ctx,
    )?;

    let return_ty = specialized_call_args.sig.return_ty.clone();

    let annotation = match return_ty {
        Type::Nothing => TypeAnnotation::Untyped(span),
        return_ty => TypedValueAnnotation {
            ty: return_ty,
            value_kind: ValueKind::Temporary,
            span,
            decl: None,
        }
        .into(),
    };

    Ok(ast::Call::Function(FunctionCall {
        annotation,
        args: specialized_call_args.actual_args,
        type_args: specialized_call_args.type_args,
        target,
        args_span: func_call.args_span.clone(),
    }))
}

fn typecheck_variant_ctor_call(
    variant: &IdentPath,
    case: &Ident,
    args: &[ast::Expr<Span>],
    span: Span,
    expect_ty: &Type,
    ctx: &mut Context,
) -> TypecheckResult<Call> {
    let unspecialized_def = ctx
        .find_variant_def(variant)
        .map_err(|err| TypecheckError::from_name_err(err, span.clone()))?;

    // infer the specialized generic type if the written one is generic and the hint is a specialized
    // version of that same generic variant
    let variant_sym = match expect_ty {
        Type::Variant(expect_variant)
            if expect_variant.is_specialization_of(&unspecialized_def.name) =>
        {
            &**expect_variant
        },

        _ => &unspecialized_def.name,
    };

    if variant_sym.is_unspecialized_generic() {
        return Err(TypecheckError::from_generic_err(
            GenericError::CannotInferArgs {
                target: GenericTarget::Name(variant_sym.qualified.clone()),
                hint: GenericTypeHint::ExpectedValueType(expect_ty.clone()),
            },
            span,
        ));
    }

    let variant_def = ctx
        .instantiate_variant_def(&variant_sym)
        .map_err(|err| TypecheckError::from_name_err(err, span.clone()))?;

    let case_index = match variant_def.case_position(case) {
        Some(index) => index,

        None => {
            return Err(TypecheckError::from_name_err(
                NameError::MemberNotFound {
                    member: case.clone(),
                    base: NameContainer::Type(Type::Variant(Box::new(variant_sym.clone()))),
                },
                span,
            ));
        },
    };

    let arg = match &variant_def.cases[case_index].data_ty {
        None => {
            if !args.is_empty() {
                let bad_args: Vec<_> = args
                    .iter()
                    .map(|arg| {
                        typecheck_expr(arg, &Type::Nothing, ctx)
                            .map(|arg| arg.annotation().ty().into_owned())
                    })
                    .collect::<TypecheckResult<_>>()?;

                return Err(TypecheckError::InvalidArgs {
                    expected: Vec::new(),
                    actual: bad_args,
                    span,
                });
            }

            None
        },

        Some(data_ty) => {
            let args: Vec<Expr> = args
                .iter()
                .map(|arg| typecheck_expr(arg, data_ty, ctx))
                .collect::<TypecheckResult<_>>()?;

            if args.len() != 1 {
                let bad_args: Vec<_> = args
                    .into_iter()
                    .map(|arg| arg.annotation().ty().into_owned())
                    .collect();

                return Err(TypecheckError::InvalidArgs {
                    expected: Vec::new(),
                    actual: bad_args,
                    span,
                });
            }

            args[0].annotation().expect_value(data_ty)?;

            Some(args.into_iter().next().unwrap())
        },
    };

    let case = variant_def.cases[case_index].ident.clone();

    let annotation = TypedValueAnnotation {
        decl: None,
        span,
        ty: Type::Variant(variant_sym.clone().into()),
        value_kind: ValueKind::Temporary,
    }
    .into();

    Ok(ast::Call::VariantCtor(ast::VariantCtorCall {
        variant: variant_def.name.clone(),
        annotation,
        arg,
        case,
    }))
}

pub struct Overload {
    pub selected_sig: usize,
    pub args: Vec<Expr>,
    pub type_args: Option<TypeList<Type>>,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum OverloadCandidate {
    Function {
        decl_name: IdentPath,
        sig: Rc<FunctionSig>,
    },
    Method {
        iface_ty: Type,
        ident: Ident,
        decl: FunctionDecl,
        sig: Rc<FunctionSig>,
    },
}

impl OverloadCandidate {
    pub fn from_instance_method(im: InstanceMethod) -> Self {
        match im {
            InstanceMethod::Method { iface_ty, decl } => OverloadCandidate::Method {
                iface_ty,
                ident: decl.ident.last().clone(),
                sig: Rc::new(FunctionSig::of_decl(&decl)),
                decl,
            },

            InstanceMethod::FreeFunction { func_name, sig } => OverloadCandidate::Function {
                decl_name: func_name,
                sig,
            },
        }
    }

    pub fn sig(&self) -> &Rc<FunctionSig> {
        match self {
            OverloadCandidate::Function { sig, .. } => sig,
            OverloadCandidate::Method { sig, .. } => sig,
        }
    }

    pub fn ident(&self) -> &IdentPath {
        match self {
            OverloadCandidate::Method { decl, .. } => &decl.ident,
            OverloadCandidate::Function { decl_name, .. } => decl_name,
        }
    }

    pub fn span(&self) -> &Span {
        match self {
            OverloadCandidate::Function { decl_name, .. } => decl_name.span(),
            OverloadCandidate::Method { decl, .. } => decl.span(),
        }
    }
}

impl fmt::Display for OverloadCandidate {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            OverloadCandidate::Function { decl_name, .. } => {
                write!(f, "function {}", decl_name)
            },
            OverloadCandidate::Method {
                iface_ty, ident, ..
            } => {
                write!(f, "method {}.{}", iface_ty, ident)
            },
        }
    }
}

pub fn resolve_overload(
    candidates: &[OverloadCandidate],
    args: &[ast::Expr<Span>],
    self_arg: Option<&Expr>,
    span: &Span,
    ctx: &mut Context,
) -> TypecheckResult<Overload> {
    if candidates.is_empty() {
        panic!("overload resolution requires at least 1 candidate");
    }

    let candidate_sigs: Vec<_> = match self_arg {
        Some(self_arg) => {
            let self_ty = self_arg.annotation().ty();
            candidates
                .iter()
                .map(|c| c.sig().with_self(&self_ty))
                .collect()
        },

        None => candidates.iter().map(|c| (**c.sig()).clone()).collect(),
    };

    // no overload resolution needed, we can use the param type hint for all args
    if candidates.len() == 1 {
        let sig = &candidate_sigs[0];

        let args = build_args_for_params(&sig.params, args, self_arg, span, ctx)?;
        let actual_arg_tys: Vec<_> = args
            .iter()
            .map(|arg_expr| arg_expr.annotation().ty().into_owned())
            .collect();

        // if it's a direct interface method invocation (e.g. `Comparable.Compare(1, 2)`),
        // we don't actually know at this point whether that implementation exists!
        if self_arg.is_none() {
            if let OverloadCandidate::Method {
                iface_ty,
                ident: method_ident,
                ..
            } = &candidates[0]
            {
                let self_ty = sig.self_ty_from_args(&actual_arg_tys).ok_or_else(|| {
                    TypecheckError::AmbiguousSelfType {
                        iface: iface_ty.clone(),
                        span: span.clone(),
                        method: method_ident.clone(),
                    }
                })?;

                let iface_name = iface_ty.as_iface().expect(
                    "can't be a self-less method if the iface type isn't a declared interface!",
                );
                if !ctx.is_iface_impl(self_ty, iface_name) {
                    return Err(TypecheckError::InterfaceNotImplemented {
                        iface_ty: iface_ty.clone(),
                        self_ty: self_ty.clone(),
                        span: span.clone(),
                    });
                }
            }
        }

        return Ok(Overload {
            selected_sig: 0,
            args,
            type_args: None, //todo
        });
    }

    // full overload resolution: while > 1 candidates remain, typecheck an additional arg
    // left-to-right, without a type hint, and look at the type of the resulting expr to eliminate
    // candidates. as soon as 1 candidate remains, process the rest of the arguments using that
    // sig. if 0 candidates remain after an arg is processed, the call is ambiguous
    let mut actual_args = Vec::new();
    let mut valid_candidates: Vec<_> = (0..candidates.len()).collect();

    let mut param_index = 0;

    // do the self-arg (which has a known type already) first
    if let Some(self_arg) = self_arg {
        actual_args.push(self_arg.clone());

        valid_candidates.retain(|i| {
            let sig = &candidate_sigs[*i];

            let self_arg_ty = self_arg.annotation().ty();

            if sig.params.len() < 1 {
                //                println!("discarding {} as candidate for {}, not enough params", sig, span);
                false
            } else {
                let self_param_ty = &sig.params[0].ty;

                check_implicit_conversion(&self_arg_ty, &self_param_ty, span, ctx).is_ok()
            }
        });

        param_index += 1;
    }

    let arg_count = args.len();
    let mut arg_index = 0;

    loop {
        // did we find a best match? try to typecheck args as if this is the sig to be called
        if valid_candidates.len() == 1 {
            let selected_sig = valid_candidates[0];
            //            println!("selected {} as candidate for {}", candidates[selected_sig].sig(), span);

            break Ok(Overload {
                selected_sig,
                args: actual_args,
                type_args: None, //todo
            });
        }

        // ran out of candidates or arguments, couldn't resolve a single sig
        if arg_index >= arg_count || valid_candidates.len() == 0 {
            //            println!("ran out of args or candidates (arg {}/{}), {} candidates remain)", arg_index + 1, arg_count, valid_candidates.len());

            break Err(TypecheckError::AmbiguousFunction {
                candidates: candidates.to_vec(),
                span: span.clone(),
            });
        }

        //        println!("matching {} (arg {}/{}), {} candidates remain)", args[arg_index], arg_index + 1, arg_count, valid_candidates.len());

        let arg = typecheck_expr(&args[arg_index], &Type::Nothing, ctx)?;
        let arg_span = args[arg_index].span();
        let arg_ty = arg.annotation().ty().into_owned();
        actual_args.push(arg);

        valid_candidates.retain(|i| {
            let sig = &candidate_sigs[*i];

            if param_index > sig.params.len() {
                //                println!("discarding {} as candidate for {}: not enough params", sig, span);
                false
            } else {
                let sig_param = &sig.params[param_index];
                check_implicit_conversion(&arg_ty, &sig_param.ty, arg_span, ctx).is_ok()
            }
        });

        param_index += 1;
        arg_index += 1;
    }
}
