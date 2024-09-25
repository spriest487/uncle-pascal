#[cfg(test)]
mod test;
mod overload;
mod args;

use crate::typ::ast::cast::implicit_conversion;
use crate::typ::ast::typecheck_expr;
use crate::typ::ast::typecheck_object_ctor;
use crate::typ::ast::Expr;
use crate::typ::ast::ObjectCtor;
use crate::typ::typecheck_type;
use crate::typ::Context;
use crate::typ::FunctionParamSig;
use crate::typ::FunctionSig;
use crate::typ::FunctionTyped;
use crate::typ::GenericError;
use crate::typ::GenericTarget;
use crate::typ::GenericTypeHint;
use crate::typ::MethodTyped;
use crate::typ::NameContainer;
use crate::typ::NameError;
use crate::typ::OverloadTyped;
use crate::typ::Specializable;
use crate::typ::Type;
use crate::typ::TypecheckError;
use crate::typ::TypecheckResult;
use crate::typ::Typed;
use crate::typ::TypedValue;
use crate::typ::UfcsTyped;
use crate::typ::ValueKind;
pub use args::*;
pub use overload::*;
use common::span::Span;
use common::span::Spanned as _;
use crate::ast;
use crate::ast::TypeList;
use crate::ast::Ident;
use crate::ast::IdentPath;
use std::borrow::Cow;
use std::rc::Rc;

pub type MethodCall = ast::MethodCall<Typed>;
pub type FunctionCall = ast::FunctionCall<Typed>;
pub type FunctionCallNoArgs = ast::FunctionCallNoArgs<Typed>;
pub type MethodCallNoArgs = ast::MethodCallNoArgs<Typed>;
pub type VariantCtorCall = ast::VariantCtorCall<Typed>;
pub type Call = ast::Call<Typed>;

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

    // typ each arg (don't do conversions yet, we haven't figured out the self ty_def yet)
    for (arg, expected_param) in src_args.iter().zip(rest_args.iter()) {
        let arg_expr = typecheck_expr(arg, &expected_param.ty, ctx)?;
        checked_args.push(arg_expr);
    }

    // does arg count match expected arg count?
    if checked_args.len() != params.len() {
        return Err(invalid_args(checked_args, params, span.clone()));
    }

    // find the self ty_def - take the actual type of the first arg that is passed to a Self-typed param
    let mut self_ty: Option<Type> = None;
    let mut params = params.to_vec();

    for i in 0..params.len() {
        let expected = &params[i];
        if expected.ty != Type::MethodSelf {
            continue;
        }

        match &self_ty {
            // this is the first arg passed as a Self param, use this as the self ty_def from now on
            None => {
                let actual_self_ty = checked_args[i].annotation().ty().into_owned();
                params[i].ty = actual_self_ty.clone();
                self_ty = Some(actual_self_ty);
            },

            // we have already deduced a self ty_def and are using that one
            Some(actual_self_ty) => {
                params[i].ty = actual_self_ty.clone();
            },
        }
    }

    args::validate_args(&mut checked_args, &params, span, ctx)?;

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
    
    let target = typecheck_expr(&func_call.target, &Type::Nothing, ctx)?;

    // if the call target is a no-args call itself and this is also a no-args call, we are just
    // applying an empty argument list to the same call, so just unwrap it here
    // e.g. if we call `procedure X;` with `X()`, `X` is already a valid call on its own
    // there are edge cases here like functions that return callable types themselves, but
    // they can be disambiguated by various means in the code
    let (target, self_arg) = if call.args().len() == 0 {
        match target {
            Expr::Call(call) => match *call {
                Call::FunctionNoArgs(call) => (call.target, None),
                Call::MethodNoArgs(call) => (call.target, Some(call.self_arg)),
                other_call => (Expr::from(other_call), None),
            }
            other => (other, None)
        }
    } else {
        (target, None)
    };

    let expr = match target.annotation() {
        Typed::TypedValue(val) => match &val.ty {
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

        Typed::Function(func) => {
            let sig = func.sig.clone();
            typecheck_func_call(target, &func_call, &sig, ctx)
                .map(Box::new)
                .map(Invocation::Call)?
        },

        Typed::UfcsFunction(ufcs_call) => {
            let typecheck_call = typecheck_ufcs_call(
                &ufcs_call,
                &func_call.args,
                func_call.annotation.span(),
                &func_call.args_span,
                ctx,
            );

            typecheck_call.map(Box::new).map(Invocation::Call)?
        },

        Typed::Overload(overloaded) => {
            typecheck_func_overload(ctx, func_call, &target, &overloaded)
                .map(Box::new)
                .map(Invocation::Call)?
        },

        Typed::Method(iface_method) => {
            typecheck_method_call(iface_method, func_call, self_arg, ctx)
                .map(Box::new)
                .map(Invocation::Call)?
        },

        Typed::Type(ty, ..) if func_call.args.is_empty() && ty.full_path().is_some() => {
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

        Typed::VariantCtor(variant, ..) => typecheck_variant_ctor_call(
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
    overloaded: &OverloadTyped,
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

            let return_annotation = TypedValue {
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
            let return_annotation = TypedValue {
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

/// * `iface_method` - Method to be called
/// * `func_call` - Source node of this call
/// * `with_self_arg` - If present, an expression which in a previous phase has been determined
///     to be the self-argument of this call. For example, when converting a call like `a.B` which
///     was previously read as a no-args call and is now being converted into a full call (`a.B()`),
///     the `a` reference is the self-arg, but isn't part of the argument list `()` of the outer
///     call expression, so needs to be provided separately
/// * `ctx` - current typechecking context
fn typecheck_method_call(
    iface_method: &MethodTyped,
    func_call: &ast::FunctionCall<Span>,
    with_self_arg: Option<Expr>,
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
    let self_type = match &with_self_arg {
        Some(self_arg) => self_arg.annotation().ty(),

        None => {
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
            let first_self_arg = typecheck_expr(
                &func_call.args[first_self_pos], 
                &Type::Nothing, 
                &mut ctx
            )?;

            Cow::Owned(first_self_arg.annotation().ty().into_owned())
        }
    };

    let is_impl = ctx
        .is_implementation(self_type.as_ref(), &iface_method.iface_ty)
        .map_err(|err| TypecheckError::from_name_err(err, func_call.span().clone()))?;

    if !is_impl {
        return Err(TypecheckError::from_name_err(NameError::NoImplementationFound {
            owning_ty: iface_method.iface_ty.clone(),
            impl_ty: self_type.into_owned(),
        }, func_call.span().clone()));
    }

    let sig = iface_method.method_sig.with_self(&self_type);

    let typechecked_args = build_args_for_params(
        &sig.params, 
        &func_call.args, 
        with_self_arg.as_ref(), 
        func_call.span(), ctx
    )?;

    Ok(Call::Method(MethodCall {
        annotation: TypedValue {
            ty: sig.return_ty.clone(),
            span: func_call.span().clone(),
            value_kind: ValueKind::Temporary,
            decl: None,
        }
        .into(),
        args_span: func_call.args_span.clone(),
        func_type: Type::Function(Rc::new(sig)),
        self_type: self_type.into_owned(),
        iface_type: iface_method.iface_ty.clone(),
        ident: iface_method.method_ident.clone(),
        type_args: None,
        args: typechecked_args,
    }))
}

fn typecheck_ufcs_call(
    ufcs_call: &UfcsTyped,
    rest_args: &[ast::Expr<Span>],
    span: &Span,
    args_span: &Span,
    ctx: &mut Context,
) -> TypecheckResult<Call> {
    let mut specialized_call_args = args::specialize_call_args(
        &ufcs_call.sig,
        &rest_args,
        Some(&ufcs_call.self_arg),
        None, // ufcs call can't have explicit type args
        &span,
        ctx,
    )?;
    args::validate_args(
        &mut specialized_call_args.actual_args,
        &specialized_call_args.sig.params,
        span,
        ctx,
    )?;

    let func_annotation = FunctionTyped {
        sig: Rc::new(specialized_call_args.sig.clone()),
        ident: ufcs_call.function_name.clone(),
        span: span.clone(),
        type_args: specialized_call_args.type_args.clone(),
    }
    .into();

    // todo: this should construct a fully qualified path expr instead
    let target = ast::Expr::Ident(ufcs_call.function_name.last().clone(), func_annotation);

    let annotation = TypedValue {
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
        args::specialize_call_args(sig, &func_call.args, None, type_args, &span, ctx)?;
    args::validate_args(
        &mut specialized_call_args.actual_args,
        &specialized_call_args.sig.params,
        func_call.span(),
        ctx,
    )?;

    let return_ty = specialized_call_args.sig.return_ty.clone();

    let annotation = match return_ty {
        Type::Nothing => Typed::Untyped(span),
        return_ty => TypedValue {
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

    let annotation = TypedValue {
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

