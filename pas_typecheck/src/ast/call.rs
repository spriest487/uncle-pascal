use crate::ast::{typecheck_expr, typecheck_object_ctor, Expression, ObjectCtor};
use crate::{
    typecheck_type, Context, FunctionParamSig, FunctionSig, GenericError, GenericTarget,
    GenericTypeHint, MethodAnnotation, NameError, Specializable, Type, TypeAnnotation,
    TypecheckError, TypecheckResult, ValueKind,
};
use pas_common::span::{Span, Spanned as _};
use pas_syn::ast::FunctionParamMod;
use pas_syn::{ast, Ident, IdentPath};
use std::rc::Rc;

pub type MethodCall = ast::MethodCall<TypeAnnotation>;
pub type FunctionCall = ast::FunctionCall<TypeAnnotation>;
pub type VariantCtorCall = ast::VariantCtorCall<TypeAnnotation>;
pub type Call = ast::Call<TypeAnnotation>;

// it's possible that during typechecking we discover what was parsed as a call with zero args
// is actually the ctor for a class with no fields
pub enum CallOrCtor {
    Call(Box<Call>),
    Ctor(Box<ObjectCtor>),
}

fn invalid_args(
    actual_args: Vec<Expression>,
    expected: &[FunctionParamSig],
    span: Span,
) -> TypecheckError {
    TypecheckError::InvalidArgs {
        expected: expected.iter().map(|p| p.ty.clone()).collect(),
        actual: actual_args
            .into_iter()
            .map(|arg| arg.annotation().ty().clone())
            .collect(),
        span,
    }
}

fn typecheck_args(
    expected_args: &[FunctionParamSig],
    actual_args: &[ast::Expression<Span>],
    self_arg: Option<&Expression>,
    span: &Span,
    ctx: &mut Context,
) -> TypecheckResult<Vec<Expression>> {
    let mut checked_args = Vec::new();

    let expected_args = if let Some(self_arg) = self_arg {
        let self_param = &expected_args[0];
        let self_compatible = self_param
            .ty
            .assignable_from(self_arg.annotation().ty(), ctx);
        if !self_compatible {
            return Err(TypecheckError::TypeMismatch {
                actual: self_arg.annotation().ty().clone(),
                expected: self_param.ty.clone(),
                span: span.clone(),
            });
        }

        checked_args.push(self_arg.clone());

        &expected_args[1..]
    } else {
        expected_args
    };

    for (arg, expected_param) in actual_args.iter().zip(expected_args.iter()) {
        let arg_expr = typecheck_expr(arg, &expected_param.ty, ctx)?;

        let (is_in_ref, is_out_ref) = match &expected_param.modifier {
            None => (false, false),
            Some(FunctionParamMod::Out) => (false, true),
            Some(FunctionParamMod::Var) => (true, true),
        };

        if is_in_ref || is_out_ref {
            match arg_expr.annotation().value_kind() {
                // in-refs shouldn't really allow uninitialized values here but we do init checking
                // in a separate pass
                Some(ValueKind::Mutable)
                | Some(ValueKind::Ref)
                | Some(ValueKind::Uninitialized) => {}
                _ => {
                    return Err(TypecheckError::NotMutable {
                        expr: Box::new(arg_expr),
                        decl: None,
                    });
                }
            }

            let ref_name = match &arg_expr {
                ast::Expression::Ident(ident, ..) => ident,
                _ => {
                    return Err(TypecheckError::InvalidRefExpression {
                        expr: Box::new(arg_expr),
                    });
                }
            };

            if is_out_ref {
                ctx.initialize(ref_name);
            }
        }

        checked_args.push(arg_expr);
    }

    let expected_len = match self_arg {
        Some(..) => expected_args.len() + 1,
        None => expected_args.len(),
    };

    if checked_args.len() != expected_len {
        // arg count doesn't match expected param count
        return Err(invalid_args(checked_args, expected_args, span.clone()));
    }

    let mut self_ty = None;

    let all_arg_tys = checked_args
        .iter()
        .map(|arg_expr| arg_expr.annotation().ty())
        .zip(expected_args.iter());

    for (actual, expected) in all_arg_tys {
        if expected.ty == Type::MethodSelf {
            if let Some(self_ty) = &self_ty {
                if *self_ty != actual {
                    return Err(invalid_args(checked_args, expected_args, span.clone()));
                }
            } else {
                self_ty = Some(actual);
            }
        } else if *actual != expected.ty {
            return Err(invalid_args(checked_args, expected_args, span.clone()));
        }
    }

    Ok(checked_args)
}

fn typecheck_type_args(
    type_args: &[ast::TypeName],
    ctx: &mut Context,
) -> TypecheckResult<Vec<Type>> {
    type_args
        .iter()
        .map(|arg_ty| typecheck_type(arg_ty, ctx))
        .collect()
}

pub fn typecheck_call(
    call: &ast::Call<Span>,
    expect_ty: &Type,
    ctx: &mut Context,
) -> TypecheckResult<CallOrCtor> {
    let func_call = match call {
        ast::Call::Function(func_call) => func_call,
        _ => unreachable!("parsing cannot result in anything except FunctionCall"),
    };

    let target = typecheck_expr(&func_call.target, &Type::Nothing, ctx)?;

    let expr = match target.annotation() {
        TypeAnnotation::TypedValue {
            ty: Type::Function(sig),
            ..
        } => typecheck_func_call(&func_call, sig.as_ref(), ctx)
            .map(Box::new)
            .map(CallOrCtor::Call)?,

        TypeAnnotation::Function {
            func_ty: Type::Function(sig),
            ..
        } => typecheck_func_call(&func_call, sig.as_ref(), ctx)
            .map(Box::new)
            .map(CallOrCtor::Call)?,

        TypeAnnotation::UFCSCall {
            function,
            func_ty: Type::Function(sig),
            self_arg,
            ..
        } => {
            let arg_brackets = (&func_call.args_brackets.0, &func_call.args_brackets.1);

            let typecheck_call = typecheck_ufcs_call(
                function,
                sig,
                *self_arg.clone(),
                &func_call.args,
                func_call.annotation.span(),
                arg_brackets,
                ctx,
            );

            typecheck_call.map(Box::new).map(CallOrCtor::Call)?
        }

        TypeAnnotation::Method(method_annotation) => {
            typecheck_method_call(&func_call, &method_annotation, ctx)
                .map(Box::new)
                .map(CallOrCtor::Call)?
        }

        TypeAnnotation::Type(ty, ..)
        if func_call.args.is_empty() && ty.full_path().is_some() =>
            {
                let ty: &Type = ty;
                let (open, close) = &func_call.args_brackets;
                let ctor = ast::ObjectCtor {
                    ident: ty.full_path().unwrap(),
                    annotation: call.span().clone(),
                    args: ast::ObjectCtorArgs {
                        open: open.clone(),
                        close: close.clone(),
                        members: Vec::new(),
                    },
                };

                let span = ctor.annotation.span().clone();

                typecheck_object_ctor(&ctor, span, expect_ty, ctx)
                    .map(Box::new)
                    .map(CallOrCtor::Ctor)?
            }

        TypeAnnotation::VariantCtor(variant, ..) => typecheck_variant_ctor_call(
            &variant.variant_name,
            &variant.case,
            &func_call.args,
            call.span().clone(),
            expect_ty,
            ctx,
        )
            .map(Box::new)
            .map(CallOrCtor::Call)?,

        _ => return Err(TypecheckError::NotCallable(Box::new(target))),
    };

    Ok(expr)
}

fn typecheck_method_call(
    func_call: &ast::FunctionCall<Span>,
    method_annotation: &MethodAnnotation,
    ctx: &mut Context,
) -> TypecheckResult<Call> {
    let span = func_call.span().clone();

    let type_args = typecheck_type_args(&func_call.type_args, ctx)?;
    let call_sig = method_annotation
        .decl_sig()
        .clone()
        .specialize_generic(&type_args, &span)?;

    let (self_type, impl_sig, args) = match &method_annotation.self_arg {
        None => {
            // deduce self-ty from args
            let args = typecheck_args(&call_sig.params, &func_call.args, None, &span, ctx)?;

            let arg_tys: Vec<_> = args.iter().map(|a| a.annotation().ty().clone()).collect();

            let self_type = call_sig
                .impl_ty_from_args(&arg_tys)
                .cloned()
                .ok_or_else(|| TypecheckError::AmbiguousMethod {
                    method: method_annotation.method_name.clone(),
                    span: span.clone(),
                })?;

            let impl_sig = call_sig.with_self(&self_type);

            (self_type, impl_sig, args)
        }

        Some(self_arg) => {
            // we have a self arg, we know how to specialize the signature before checking the
            // arg types
            let self_type = self_arg.annotation().ty().clone();
            let impl_sig = call_sig.with_self(&self_type);

            // the self-arg is passed as the first argument
            assert!(!impl_sig.params.is_empty());

            let args = typecheck_args(
                &impl_sig.params,
                &func_call.args,
                Some(self_arg.as_ref()),
                &span,
                ctx,
            )?;

            (self_type, impl_sig, args)
        }
    };

    let annotation = match &impl_sig.return_ty {
        Type::Nothing => TypeAnnotation::Untyped(span),
        return_ty => TypeAnnotation::TypedValue {
            span,
            ty: return_ty.clone(),
            value_kind: ValueKind::Temporary,
            decl: None,
        },
    };

    Ok(ast::Call::Method(MethodCall {
        annotation,
        args,
        type_args,
        self_type,
        func_type: Type::Function(Rc::new(impl_sig)),
        ident: method_annotation.method_name.clone(),
        of_type: method_annotation.iface_ty.clone(),
        args_brackets: func_call.args_brackets.clone(),
    }))
}

fn typecheck_ufcs_call(
    func_name: &IdentPath,
    sig: &Rc<FunctionSig>,
    self_arg: Expression,
    rest_args: &[ast::Expression<Span>],
    span: &Span,
    arg_brackets: (&Span, &Span),
    ctx: &mut Context,
) -> TypecheckResult<Call> {
    let specialized_call_args = specialize_call_args(
        sig,
        &rest_args,
        Some(&self_arg),
        &Vec::new(), // ufcs call can't have explicit type args
        &span,
        ctx,
    )?;

    let func_annotation = TypeAnnotation::Function {
        func_ty: Type::Function(Rc::new(specialized_call_args.sig.clone())),
        ns: func_name.clone().parent().unwrap(),
        name: func_name.last().clone(),
        span: span.clone(),
        type_args: specialized_call_args.type_args.clone(),
    };

    // todo: this should construct a fully qualified path expr instead
    let target = ast::Expression::Ident(func_name.last().clone(), func_annotation);

    let annotation = TypeAnnotation::TypedValue {
        ty: specialized_call_args.sig.return_ty.clone(),
        span: span.clone(),
        decl: None,
        value_kind: ValueKind::Temporary,
    };

    Ok(ast::Call::Function(FunctionCall {
        args: specialized_call_args.actual_args,
        args_brackets: (arg_brackets.0.clone(), arg_brackets.1.clone()),
        annotation,
        type_args: specialized_call_args.type_args,
        target,
    }))
}

struct SpecializedCallArgs {
    sig: FunctionSig,
    type_args: Vec<Type>,
    actual_args: Vec<Expression>,
}

fn specialize_arg<ArgProducer>(
    param_ty: &Type,
    inferred_ty_args: &mut Vec<Option<Type>>,
    produce_expr: ArgProducer,
    ctx: &mut Context,
) -> TypecheckResult<Expression>
// (expected type, ctx) -> expression val
  where
      ArgProducer: FnOnce(&Type, &mut Context) -> TypecheckResult<Expression>,
{
    match param_ty {
        // param is generic: arg expr ty drives param type
        Type::GenericParam(param_ty) => {
            match &inferred_ty_args[param_ty.pos] {
                // type arg in this pos was already inferred from an earlier arg
                Some(already_inferred_ty) => produce_expr(already_inferred_ty, ctx),

                None => {
                    let actual_arg = produce_expr(&Type::Nothing, ctx)?;
                    let actual_ty = actual_arg.annotation().ty().clone();

                    inferred_ty_args[param_ty.pos] = Some(actual_ty);
                    Ok(actual_arg)
                }
            }
        }

        // param is not generic: param type drives expr expected type
        param_ty => {
            let actual_arg = produce_expr(param_ty, ctx)?;
            let actual_ty = actual_arg.annotation().ty();

            infer_from_structural_ty_args(param_ty, actual_ty, inferred_ty_args);

            Ok(actual_arg)
        }
    }
}

fn infer_from_structural_ty_args(
    param_ty: &Type,
    actual_ty: &Type,
    inferred_ty_args: &mut Vec<Option<Type>>,
) {
    if !param_ty.same_decl_type(param_ty) {
        return;
    }

    let param_ty_args = match param_ty.type_args() {
        Some(args) => args,
        None => return,
    };
    let actual_ty_args = match actual_ty.type_args() {
        Some(args) => args,
        None => return,
    };

    let all_ty_args = param_ty_args.iter().zip(actual_ty_args.iter());
    for (param_ty_arg, actual_ty_arg) in all_ty_args {
        match param_ty_arg {
            Type::GenericParam(param_generic) => {
                let inferred = &mut inferred_ty_args[param_generic.pos];
                assert_eq!(None, *inferred);

                *inferred = Some(actual_ty_arg.clone());
            }

            _ => infer_from_structural_ty_args(param_ty_arg, actual_ty_arg, inferred_ty_args),
        }
    }
}

fn specialize_call_args(
    decl_sig: &FunctionSig,
    args: &[ast::Expression<Span>],
    self_arg: Option<&Expression>,
    explicit_ty_args: &[Type],
    span: &Span,
    ctx: &mut Context,
) -> TypecheckResult<SpecializedCallArgs> {
    if !explicit_ty_args.is_empty() {
        // we have explicit args, don't try to infer types
        let sig = decl_sig
            .clone()
            .specialize_generic(explicit_ty_args, span)?;

        let actual_args = typecheck_args(&sig.params, args, self_arg, span, ctx)?;

        Ok(SpecializedCallArgs {
            sig,
            actual_args,
            type_args: explicit_ty_args.to_vec(),
        })
    } else {
        // we haven't checked arg length matches yet because we haven't typechecked args        let self
        let self_arg_len = if self_arg.is_some() { 1 } else { 0 };
        if args.len() + self_arg_len != decl_sig.params.len() {
            // this is an inferral error because we don't have enough information to report
            // it as an arg type mismatch
            return Err(GenericError::CannotInferArgs {
                target: GenericTarget::FunctionSig(decl_sig.clone()),
                hint: GenericTypeHint::Unknown,
                span: span.clone(),
            }
                .into());
        }

        // try to infer type from args, left to right
        let mut inferred_ty_args = vec![None; decl_sig.type_params_len];
        let mut actual_args = Vec::new();

        if let Some(self_arg) = self_arg.cloned() {
            let self_param = &decl_sig.params[0];
            let self_arg = specialize_arg(
                &self_param.ty,
                &mut inferred_ty_args,
                |_expect_ty, _ctx| Ok(self_arg),
                ctx,
            )?;

            actual_args.push(self_arg);
        }

        for (i, arg) in args.iter().enumerate() {
            let param_ty = &decl_sig.params[i + self_arg_len].ty;
            let actual_arg = specialize_arg(
                param_ty,
                &mut inferred_ty_args,
                |expect_ty, ctx| typecheck_expr(arg, expect_ty, ctx),
                ctx,
            )?;

            actual_args.push(actual_arg);
        }

        if inferred_ty_args.iter().any(Option::is_none) {
            let arg_tys = actual_args
                .into_iter()
                .map(|a| a.annotation().ty().clone())
                .collect();

            return Err(GenericError::CannotInferArgs {
                target: GenericTarget::FunctionSig(decl_sig.clone()),
                hint: GenericTypeHint::ArgTypes(arg_tys),
                span: span.clone(),
            }
                .into());
        }

        let inferred_ty_args: Vec<_> =
            inferred_ty_args.into_iter().map(|a| a.unwrap()).collect();

        let actual_sig = decl_sig.specialize_generic(&inferred_ty_args, span)?;

        //        println!("{} -> {}, inferred: {:#?}", decl_sig, actual_sig, inferred_ty_args);

        Ok(SpecializedCallArgs {
            type_args: inferred_ty_args,
            sig: actual_sig,
            actual_args,
        })
    }
}

fn typecheck_func_call(
    func_call: &ast::FunctionCall<Span>,
    sig: &FunctionSig,
    ctx: &mut Context,
) -> TypecheckResult<Call> {
    let span = func_call.span().clone();

    let target = typecheck_expr(&func_call.target, &Type::Nothing, ctx)?;

    let type_args = typecheck_type_args(&func_call.type_args, ctx)?;

    let specialized_call_args =
        specialize_call_args(sig, &func_call.args, None, &type_args, &span, ctx)?;

    let return_ty = specialized_call_args.sig.return_ty.clone();

    let annotation = match return_ty {
        Type::Nothing => TypeAnnotation::Untyped(span),
        return_ty => TypeAnnotation::TypedValue {
            ty: return_ty,
            value_kind: ValueKind::Temporary,
            span,
            decl: None,
        },
    };

    Ok(ast::Call::Function(FunctionCall {
        annotation,
        args: specialized_call_args.actual_args,
        type_args: specialized_call_args.type_args,
        target,
        args_brackets: func_call.args_brackets.clone(),
    }))
}

fn typecheck_variant_ctor_call(
    variant: &IdentPath,
    case: &Ident,
    args: &[ast::Expression<Span>],
    span: Span,
    expect_ty: &Type,
    ctx: &mut Context,
) -> TypecheckResult<Call> {
    let unspecialized_def = ctx.find_variant_def(variant)?;

    // infer the specialized generic type if the written one is generic and the hint is a specialized
    // version of that same generic variant
    let variant = match expect_ty {
        Type::Variant(expect_variant)
        if expect_variant.is_specialization_of(&unspecialized_def.name) =>
            {
                &*expect_variant
            }

        _ => &unspecialized_def.name,
    };

    if variant.is_generic() {
        return Err(GenericError::CannotInferArgs {
            target: GenericTarget::Name(variant.qualified.clone()),
            hint: GenericTypeHint::ExpectedValueType(expect_ty.clone()),
            span,
        }
            .into());
    }

    let variant_def = ctx.instantiate_variant(&variant)?;

    let case_index = match variant_def.case_position(case) {
        Some(index) => index,

        None => {
            return Err(NameError::MemberNotFound {
                span: span.clone(),
                member: case.clone(),
                base: Type::Variant(Box::new(variant.clone())),
            }
                .into())
        }
    };

    let arg = match &variant_def.cases[case_index].data_ty {
        None => {
            if !args.is_empty() {
                let bad_args: Vec<_> = args
                    .iter()
                    .map(|arg| {
                        typecheck_expr(arg, &Type::Nothing, ctx)
                            .map(|arg| arg.annotation().ty().clone())
                    })
                    .collect::<TypecheckResult<_>>()?;

                return Err(TypecheckError::InvalidArgs {
                    expected: Vec::new(),
                    actual: bad_args,
                    span,
                });
            }

            None
        }

        Some(data_ty) => {
            let args: Vec<Expression> = args
                .iter()
                .map(|arg| typecheck_expr(arg, data_ty, ctx))
                .collect::<TypecheckResult<_>>()?;

            if args.len() != 1 {
                let bad_args: Vec<_> = args
                    .into_iter()
                    .map(|arg| arg.annotation().ty().clone())
                    .collect();

                return Err(TypecheckError::InvalidArgs {
                    expected: Vec::new(),
                    actual: bad_args,
                    span,
                });
            }

            args[0].annotation().expect_value(data_ty)?;

            Some(args.into_iter().next().unwrap())
        }
    };

    let case = variant_def.cases[case_index].ident.clone();

    let annotation = TypeAnnotation::TypedValue {
        decl: None,
        span,
        ty: Type::Variant(Box::new(variant.clone())),
        value_kind: ValueKind::Temporary,
    };

    Ok(ast::Call::VariantCtor(ast::VariantCtorCall {
        variant: variant_def.name.clone(),
        annotation,
        arg,
        case,
    }))
}