use pas_common::span::*;
use pas_syn::ast;

use crate::{ast::prelude::*, ty::FunctionParamSig};

use std::rc::Rc;

pub type MethodCall = ast::MethodCall<TypeAnnotation>;
pub type FunctionCall = ast::FunctionCall<TypeAnnotation>;
pub type VariantCtorCall = ast::VariantCtorCall<TypeAnnotation>;
pub type Call = ast::Call<TypeAnnotation>;
pub type Expression = ast::Expression<TypeAnnotation>;

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
                ctx
            );

            typecheck_call.map(Box::new).map(CallOrCtor::Call)?
        }

        TypeAnnotation::Method(method_annotation) => {
            typecheck_method_call(&func_call, &method_annotation, ctx)
                .map(Box::new)
                .map(CallOrCtor::Call)?
        }

        TypeAnnotation::Type(ty, ..) if func_call.args.is_empty() && ty.full_path().is_some() => {
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
        target
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
    where ArgProducer: FnOnce(&Type, &mut Context) -> TypecheckResult<Expression>,
{
    match param_ty {
        // param is generic: arg expr ty drives param type
        Type::GenericParam(param_ty) => {
            match &inferred_ty_args[param_ty.pos] {
                // type arg in this pos was already inferred from an earlier arg
                Some(already_inferred_ty) => {
                    produce_expr(already_inferred_ty, ctx)
                },

                None => {
                    let actual_arg = produce_expr(&Type::Nothing, ctx)?;
                    let actual_ty = actual_arg.annotation().ty().clone();

                    inferred_ty_args[param_ty.pos] = Some(actual_ty);
                    Ok(actual_arg)
                },
            }
        }

        // param is not generic: param type drives expr expected type
        param_ty => {
            let actual_arg = produce_expr(param_ty, ctx)?;
            let actual_ty = actual_arg.annotation().ty();

            infer_from_structural_ty_args(param_ty, actual_ty, inferred_ty_args);

            Ok(actual_arg)
        },
    }
}

fn infer_from_structural_ty_args(
    param_ty: &Type,
    actual_ty: &Type,
    inferred_ty_args: &mut Vec<Option<Type>>
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
            },

            _ => infer_from_structural_ty_args(
                param_ty_arg,
                actual_ty_arg,
                inferred_ty_args,
            ),
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
        let sig = decl_sig.clone().specialize_generic(explicit_ty_args, span)?;

        let actual_args = typecheck_args(
            &sig.params,
            args,
            self_arg,
            span,
            ctx,
        )?;

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
            }.into());
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
                ctx
            )?;

            actual_args.push(self_arg);
        }

        for (i, arg) in args.iter().enumerate() {
            let param_ty = &decl_sig.params[i + self_arg_len].ty;
            let actual_arg = specialize_arg(
                param_ty,
                &mut inferred_ty_args,
                |expect_ty, ctx| {
                    typecheck_expr(arg, expect_ty, ctx)
                },
                ctx,
            )?;

            actual_args.push(actual_arg);
        }

        if inferred_ty_args.iter().any(Option::is_none) {
            let arg_tys = actual_args.into_iter()
                .map(|a| a.annotation().ty().clone())
                .collect();

            return Err(GenericError::CannotInferArgs {
                target: GenericTarget::FunctionSig(decl_sig.clone()),
                hint: GenericTypeHint::ArgTypes(arg_tys),
                span: span.clone(),
            }.into());
        }

        let inferred_ty_args: Vec<_> = inferred_ty_args.into_iter()
            .map(|a| a.unwrap())
            .collect();

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

    let specialized_call_args = specialize_call_args(
        sig,
        &func_call.args,
        None,
        &type_args,
        &span,
        ctx,
    )?;

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
        }.into());
    }

    let variant_def = ctx.instantiate_variant(&variant)?;

    let case_index = match variant_def.case_position(case) {
        Some(index) => index,

        None => return Err(NameError::MemberNotFound {
            span: span.clone(),
            member: case.clone(),
            base: Type::Variant(Box::new(variant.clone())),
        }.into()),
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

pub fn typecheck_expr(
    expr_node: &ast::Expression<Span>,
    expect_ty: &Type,
    ctx: &mut Context,
) -> TypecheckResult<Expression> {
    let span = expr_node.annotation().clone();

    match expr_node {
        ast::Expression::Literal(ast::Literal::String(s), _) => {
            let binding = ValueKind::Immutable;
            let annotation = TypeAnnotation::TypedValue {
                ty: string_type(ctx)?,
                value_kind: binding,
                span: span.clone(),
                decl: None,
            };

            Ok(ast::Expression::Literal(
                ast::Literal::String(s.clone()),
                annotation,
            ))
        }

        ast::Expression::Literal(ast::Literal::Boolean(b), _) => {
            let annotation = TypeAnnotation::TypedValue {
                ty: Type::Primitive(Primitive::Boolean),
                value_kind: ValueKind::Immutable,
                span,
                decl: None,
            };

            Ok(ast::Expression::Literal(
                ast::Literal::Boolean(*b),
                annotation,
            ))
        }

        ast::Expression::Literal(ast::Literal::Integer(i), _) => {
            let annotation = TypeAnnotation::TypedValue {
                ty: if i.as_i32().is_some() {
                    Type::from(Primitive::Int32)
                } else {
                    unimplemented!("integers outside range of i32")
                },
                value_kind: ValueKind::Immutable,
                span,
                decl: None,
            };

            Ok(ast::Expression::Literal(
                ast::Literal::Integer(*i),
                annotation,
            ))
        }

        ast::Expression::Literal(ast::Literal::Real(x), _) => {
            let ty = if x.as_f32().is_some() {
                Type::from(Primitive::Real32)
            } else {
                unimplemented!("real literal outside range of f32")
            };

            let annotation = TypeAnnotation::TypedValue {
                ty,
                value_kind: ValueKind::Immutable,
                span,
                decl: None,
            };

            Ok(ast::Expression::Literal(
                ast::Literal::Real(x.clone()),
                annotation,
            ))
        }

        ast::Expression::Literal(ast::Literal::Nil, _) => {
            let ty = match expect_ty {
                ptr @ Type::Pointer(..) => ptr.clone(),
                _ => Type::Nil,
            };

            let annotation = TypeAnnotation::TypedValue {
                ty,
                value_kind: ValueKind::Temporary,
                span,
                decl: None,
            };
            Ok(ast::Expression::Literal(ast::Literal::Nil, annotation))
        }

        ast::Expression::Ident(ident, _) => {
            let annotation = match ctx.find(&ident) {
                Some(member) => ns_member_ref_to_annotation(member, ident.span().clone(), ctx),

                _ => {
                    return Err(NameError::NotFound(ident.clone()).into());
                }
            };

            Ok(ast::Expression::Ident(ident.clone(), annotation))
        }

        ast::Expression::BinOp(bin_op) => typecheck_bin_op(bin_op, expect_ty, ctx),

        ast::Expression::UnaryOp(unary_op) => {
            let unary_op = typecheck_unary_op(unary_op, ctx)?;
            Ok(ast::Expression::from(unary_op))
        }

        ast::Expression::Call(call) => {
            let expr = match typecheck_call(call, expect_ty, ctx)? {
                CallOrCtor::Call(call) => ast::Expression::from(*call),
                CallOrCtor::Ctor(ctor) => ast::Expression::from(*ctor),
            };
            Ok(expr)
        }

        ast::Expression::ObjectCtor(ctor) => {
            let span = ctor.annotation.span().clone();
            let ctor = typecheck_object_ctor(ctor, span, expect_ty, ctx)?;
            Ok(ast::Expression::from(ctor))
        }

        ast::Expression::CollectionCtor(ctor) => {
            let ctor = typecheck_collection_ctor(ctor, expect_ty, ctx)?;
            Ok(ast::Expression::from(ctor))
        }

        ast::Expression::IfCond(if_cond) => {
            let if_cond = typecheck_if_cond(if_cond, expect_ty, ctx)?;
            Ok(ast::Expression::from(if_cond))
        }

        ast::Expression::Block(block) => {
            let block = typecheck_block(block, expect_ty, ctx)?;
            Ok(ast::Expression::from(block))
        }

        ast::Expression::Indexer(indexer) => {
            let indexer = typecheck_indexer(indexer, ctx)?;
            Ok(ast::Expression::from(indexer))
        }
    }
}

pub fn ns_member_ref_to_annotation(
    member: MemberRef<Scope>,
    span: Span,
    ctx: &Context,
) -> TypeAnnotation {
    match member {
        MemberRef::Value {
            value: Decl::Alias(aliased),
            ..
        } => {
            let alias_ref = ctx
                .resolve(aliased)
                .unwrap_or_else(|| panic!("invalid alias to {}", aliased));

            ns_member_ref_to_annotation(alias_ref, span, ctx)
        }

        MemberRef::Value {
            value: Decl::BoundValue(binding),
            ..
        } => TypeAnnotation::TypedValue {
            span,
            ty: binding.ty.clone(),
            value_kind: binding.kind,
            decl: binding.def.clone(),
        },

        MemberRef::Value {
            value: Decl::Function { sig, .. },
            ref parent_path,
            key,
        } => {
            if parent_path.as_slice().is_empty() {
                panic!("empty path for decl {}", key);
            }

            TypeAnnotation::Function {
                span,
                ns: IdentPath::from_parts(parent_path.keys().cloned()),
                name: key.clone(),
                func_ty: Type::Function(sig.clone()),
                // the named version of the function never has type args, the caller will have
                // to specialize the expression to add some
                type_args: Vec::new(),
            }
        }

        MemberRef::Value {
            value: Decl::Type { ty, .. },
            ..
        } => TypeAnnotation::Type(ty.clone(), span),

        MemberRef::Namespace { path } => {
            TypeAnnotation::Namespace(IdentPath::from_parts(path.keys().cloned()), span)
        }
    }
}

pub fn expect_stmt_initialized(stmt: &Statement, ctx: &Context) -> TypecheckResult<()> {
    match stmt {
        ast::Statement::Call(call) => expect_call_initialized(call, ctx),

        ast::Statement::If(if_stmt) => {
            expect_expr_initialized(&if_stmt.cond, ctx)?;
            expect_expr_initialized(&if_stmt.then_branch, ctx)?;
            if let Some(else_branch) = &if_stmt.else_branch {
                expect_expr_initialized(else_branch, ctx)?;
            }
            Ok(())
        }

        ast::Statement::Block(block) => expect_block_initialized(block, ctx),

        ast::Statement::LocalBinding(binding) => expect_binding_initialized(binding, ctx),

        ast::Statement::ForLoop(for_loop) => {
            expect_binding_initialized(&for_loop.init_binding, ctx)?;
            expect_expr_initialized(&for_loop.to_expr, ctx)?;
            expect_stmt_initialized(&for_loop.body, ctx)?;
            Ok(())
        }

        ast::Statement::WhileLoop(while_loop) => {
            expect_expr_initialized(&while_loop.condition, ctx)?;
            expect_stmt_initialized(&while_loop.body, ctx)?;
            Ok(())
        }

        ast::Statement::Exit(exit) => match exit {
            ast::Exit::WithoutValue(_) => Ok(()),
            ast::Exit::WithValue(exit_val) => expect_expr_initialized(exit_val, ctx),
        },

        ast::Statement::Assignment(assignment) => expect_expr_initialized(&assignment.rhs, ctx),

        ast::Statement::Break(..) | ast::Statement::Continue(..) => Ok(()),
    }
}

pub fn expect_expr_initialized(expr: &Expression, ctx: &Context) -> TypecheckResult<()> {
    match expr {
        ast::Expression::Ident(ident, ..) => match expr.annotation().value_kind() {
            Some(ValueKind::Uninitialized) => {
                let decl_ident = ctx.find_decl(ident).unwrap_or(ident);
                Err(TypecheckError::NotInitialized {
                    ident: decl_ident.clone(),
                    usage: ident.span().clone(),
                })
            }

            _ => Ok(()),
        },

        ast::Expression::Literal(..) => Ok(()),

        ast::Expression::Indexer(indexer) => {
            expect_expr_initialized(&indexer.base, ctx)?;
            expect_expr_initialized(&indexer.index, ctx)?;
            Ok(())
        }

        ast::Expression::Block(block) => expect_block_initialized(block, ctx),

        ast::Expression::IfCond(cond) => {
            expect_expr_initialized(&cond.cond, ctx)?;
            expect_expr_initialized(&cond.then_branch, ctx)?;
            if let Some(else_branch) = &cond.else_branch {
                expect_expr_initialized(else_branch, ctx)?;
            }
            Ok(())
        }

        ast::Expression::ObjectCtor(ctor) => {
            for member in &ctor.args.members {
                expect_expr_initialized(&member.value, ctx)?;
            }
            Ok(())
        }

        ast::Expression::CollectionCtor(ctor) => {
            for el in &ctor.elements {
                expect_expr_initialized(&el, ctx)?;
            }
            Ok(())
        }

        ast::Expression::Call(call) => expect_call_initialized(call, ctx),

        ast::Expression::BinOp(bin_op) => {
            expect_expr_initialized(&bin_op.lhs, ctx)?;
            expect_expr_initialized(&bin_op.rhs, ctx)?;
            Ok(())
        }

        ast::Expression::UnaryOp(unary_op) => expect_expr_initialized(&unary_op.operand, ctx),
    }
}

fn expect_binding_initialized(binding: &LocalBinding, ctx: &Context) -> TypecheckResult<()> {
    if let Some(init_val) = &binding.val {
        expect_expr_initialized(init_val, ctx)?;
    }
    Ok(())
}

fn expect_args_initialized(
    params: &[FunctionParamSig],
    args: &[Expression],
    ctx: &Context,
) -> TypecheckResult<()> {
    assert_eq!(
        params.len(),
        args.len(),
        "function call with wrong number of args shouldn't pass type checking. got:\n{}\nexpected:\n{}",
        args.iter().map(Expression::to_string).collect::<Vec<_>>().join("; "),
        params.iter().map(|param| param.ty.to_string()).collect::<Vec<_>>().join("; "),
    );

    for (arg, param) in args.iter().zip(params.iter()) {
        if param.modifier != Some(FunctionParamMod::Out) {
            expect_expr_initialized(arg, ctx)?;
        }
    }

    Ok(())
}

fn expect_call_initialized(call: &Call, ctx: &Context) -> TypecheckResult<()> {
    match call {
        ast::Call::Function(func_call) => {
            expect_expr_initialized(&func_call.target, ctx)?;

            let params = match func_call.target.annotation().ty() {
                Type::Function(sig) => &sig.params,
                _ => panic!(
                    "function call with wrong target type shouldn't pass type checking (was: {:#?})",
                    func_call.target.annotation().ty()
                ),
            };
            expect_args_initialized(params, &func_call.args, ctx)?;
        }

        ast::Call::Method(method_call) => {
            let params = match &method_call.func_type {
                Type::Function(sig) => &sig.params,
                _ => panic!(
                    "function call with wrong target type shouldn't pass type checking (was: {:#?})",
                    method_call.func_type,
                ),
            };

            expect_args_initialized(params, &method_call.args, ctx)?;
        }

        ast::Call::VariantCtor(ctor_call) => {
            if let Some(arg_expr) = &ctor_call.arg {
                expect_expr_initialized(&arg_expr, ctx)?;
            }
        }
    }

    Ok(())
}

fn expect_block_initialized(block: &Block, ctx: &Context) -> TypecheckResult<()> {
    for stmt in &block.statements {
        expect_stmt_initialized(stmt, ctx)?;
    }
    if let Some(output) = &block.output {
        expect_expr_initialized(output, ctx)?;
    }
    Ok(())
}
