use crate::ast::prelude::*;
use pas_common::span::*;
use pas_syn::ast;

pub type MethodCall = ast::MethodCall<TypeAnnotation>;
pub type FunctionCall = ast::FunctionCall<TypeAnnotation>;
pub type Call = ast::Call<TypeAnnotation>;
pub type ExpressionNode = ast::ExpressionNode<TypeAnnotation>;

fn invalid_args(actual_args: Vec<ExpressionNode>, expected: &[Type], span: Span) -> TypecheckError {
    TypecheckError::InvalidArgs {
        expected: expected.to_vec(),
        actual: actual_args
            .into_iter()
            .map(|arg| arg.annotation.value_ty().clone())
            .collect(),
        span,
    }
}

fn typecheck_args(
    expected_args: &[Type],
    actual_args: &[ast::ExpressionNode<Span>],
    span: &Span,
    ctx: &mut Context,
) -> TypecheckResult<Vec<ExpressionNode>> {
    let mut checked_args = Vec::new();

    for (arg, param) in actual_args.iter().zip(expected_args.iter()) {
        checked_args.push(typecheck_expr(arg, param, ctx)?);
    }

    if checked_args.len() != expected_args.len() {
        // arg counts don't match
        return Err(invalid_args(checked_args, expected_args, span.clone()));
    }

    let mut self_ty = None;

    let all_arg_tys = checked_args
        .iter()
        .map(|arg_expr| arg_expr.annotation.value_ty())
        .zip(expected_args.iter());

    for (actual, expected) in all_arg_tys {
        if *expected == Type::GenericSelf {
            if let Some(self_ty) = &self_ty {
                if *self_ty != actual {
                    return Err(invalid_args(checked_args, expected_args, span.clone()));
                }
            } else {
                self_ty = Some(actual);
            }
        } else if *actual != *expected {
            return Err(invalid_args(checked_args, expected_args, span.clone()));
        }
    }

    Ok(checked_args)
}

pub fn typecheck_call(call: &ast::Call<Span>, ctx: &mut Context) -> TypecheckResult<Call> {
    let func_call = match call {
        ast::Call::Function(func_call) => func_call,
        _ => unreachable!("parsing cannot result in anything except FunctionCall"),
    };

    let target = typecheck_expr(&func_call.target, &Type::Nothing, ctx)?;

    let call = match &target.annotation {
        TypeAnnotation::TypedValue {
            ty: Type::Function(sig),
            ..
        } => typecheck_func_call(&func_call, sig.as_ref(), ctx)?,

        TypeAnnotation::Function {
            ty: Type::Function(sig),
            ..
        } => typecheck_func_call(&func_call, sig.as_ref(), ctx)?,

        TypeAnnotation::Method(method_annotation) => {
            typecheck_method_call(&func_call, &method_annotation, ctx)?
        },

        _ => return Err(TypecheckError::NotCallable(Box::new(target))),
    };

    Ok(call)
}

fn typecheck_method_call(
    func_call: &ast::FunctionCall<Span>,
    method_annotation: &MethodAnnotation,
    ctx: &mut Context,
) -> TypecheckResult<Call> {
    let span = func_call.span().clone();

    let (self_type, impl_sig, args) = match &method_annotation.self_arg {
        None => {
            // deduce self-ty from args
            let args = typecheck_args(
                &method_annotation.decl_sig().params,
                &func_call.args,
                &span,
                ctx,
            )?;

            let arg_tys: Vec<_> = args
                .iter()
                .map(|a| a.annotation.value_ty().clone())
                .collect();

            let self_type = method_annotation
                .decl_sig()
                .impl_ty_from_args(&arg_tys)
                .cloned()
                .ok_or_else(|| TypecheckError::AmbiguousMethod {
                    method: method_annotation.method.ident.clone(),
                    span: span.clone(),
                })?;

            let impl_sig = method_annotation.decl_sig().with_self(&self_type);

            (self_type, impl_sig, args)
        },

        Some(self_arg) => {
            // we have a self arg, we know how to specialize the signature before checking the
            // arg types
            let self_type = self_arg.annotation.value_ty().clone();
            let mut impl_sig = method_annotation.decl_sig().with_self(&self_type);

            // the self-arg is passed as the first argument
            assert!(!impl_sig.params.is_empty());
            let self_param = impl_sig.params.remove(0);
            if !self_param.assignable_from(self_arg.annotation.value_ty()) {
                return Err(TypecheckError::TypeMismatch {
                    actual: self_arg.annotation.value_ty().clone(),
                    expected: self_param,
                    span,
                });
            }

            let mut args = typecheck_args(&impl_sig.params, &func_call.args, &span, ctx)?;
            args.insert(0, (**self_arg).clone());

            (self_type, impl_sig, args)
        },
    };

    let annotation = match &impl_sig.return_ty {
        Type::Nothing => TypeAnnotation::Untyped(span),
        return_ty => TypeAnnotation::TypedValue {
            span,
            ty: return_ty.clone(),
            value_kind: ValueKind::Temporary,
        },
    };

    Ok(ast::Call::Method(MethodCall {
        annotation,
        args,
        self_type,
        ident: method_annotation.method.ident.clone(),
        of_type: method_annotation.iface_ty.clone(),
    }))
}

fn typecheck_func_call(
    func_call: &ast::FunctionCall<Span>,
    sig: &FunctionSig,
    ctx: &mut Context,
) -> TypecheckResult<Call> {
    let span = func_call.span().clone();

    let target = typecheck_expr(&func_call.target, &Type::Nothing, ctx)?;
    let args = typecheck_args(&sig.params, &func_call.args, &span, ctx)?;

    let return_ty = sig.return_ty.clone();

    let annotation = match return_ty {
        Type::Nothing => TypeAnnotation::Untyped(span),
        return_ty => TypeAnnotation::TypedValue {
            ty: return_ty,
            value_kind: ValueKind::Temporary,
            span,
        },
    };

    Ok(ast::Call::Function(FunctionCall {
        annotation,
        args,
        target,
    }))
}

pub fn typecheck_expr(
    expr_node: &ast::ExpressionNode<Span>,
    expect_ty: &Type,
    ctx: &mut Context,
) -> TypecheckResult<ExpressionNode> {
    let span = expr_node.annotation.clone();

    match expr_node.expr.as_ref() {
        ast::Expression::Literal(ast::Literal::String(s)) => {
            let binding = ValueKind::Immutable;
            let annotation = TypeAnnotation::TypedValue {
                ty: ctx.string_type()?,
                value_kind: binding,
                span: span.clone(),
            };

            Ok(ast::ExpressionNode::new(
                ast::Literal::String(s.clone()),
                annotation,
            ))
        },

        ast::Expression::Literal(ast::Literal::Boolean(b)) => {
            let annotation = TypeAnnotation::TypedValue {
                ty: Type::Primitive(Primitive::Boolean),
                value_kind: ValueKind::Immutable,
                span,
            };

            Ok(ast::ExpressionNode::new(
                ast::Literal::Boolean(*b),
                annotation,
            ))
        },

        ast::Expression::Literal(ast::Literal::Integer(i)) => {
            let annotation = TypeAnnotation::TypedValue {
                ty: if i.as_i32().is_some() {
                    Type::from(Primitive::Int32)
                } else {
                    unimplemented!("integers outside range of i32")
                },
                value_kind: ValueKind::Immutable,
                span,
            };

            Ok(ast::ExpressionNode::new(
                ast::Literal::Integer(*i),
                annotation,
            ))
        },

        ast::Expression::Literal(ast::Literal::Real(x)) => {
            let ty = if x.as_f32().is_some() {
                Type::from(Primitive::Real32)
            } else {
                unimplemented!("real literal outside range of f32")
            };

            let annotation = TypeAnnotation::TypedValue {
                ty,
                value_kind: ValueKind::Immutable,
                span,
            };

            Ok(ast::ExpressionNode::new(
                ast::Literal::Real(x.clone()),
                annotation,
            ))
        },

        ast::Expression::Literal(ast::Literal::Nil) => {
            let annotation = TypeAnnotation::TypedValue {
                ty: Type::Nil,
                value_kind: ValueKind::Temporary,
                span,
            };
            Ok(ast::ExpressionNode::new(ast::Literal::Nil, annotation))
        },

        ast::Expression::Ident(ident) => {
            let annotation = match ctx.find(ident) {
                Some(member) => ns_member_ref_to_annotation(member, ident.span().clone(), ctx),

                _ => {
                    return Err(NameError::NotFound(ident.clone()).into());
                },
            };

            Ok(ast::ExpressionNode::new(ident.clone(), annotation))
        },

        ast::Expression::BinOp(bin_op) => {
            typecheck_bin_op(bin_op, ctx)
        },

        ast::Expression::UnaryOp(unary_op) => {
            let unary_op = typecheck_unary_op(unary_op, ctx)?;
            let annotation = unary_op.annotation.clone();
            Ok(ast::ExpressionNode::new(unary_op, annotation))
        },

        ast::Expression::Call(call) => {
            let call = typecheck_call(call, ctx)?;
            let annotation = call.annotation().clone();
            Ok(ast::ExpressionNode::new(call, annotation))
        },

        ast::Expression::ObjectCtor(ctor) => {
            let ctor = typecheck_object_ctor(ctor, ctx)?;
            let annotation = ctor.annotation.clone();
            Ok(ast::ExpressionNode::new(ctor, annotation))
        },

        ast::Expression::IfCond(if_cond) => {
            let if_cond = typecheck_if_cond(if_cond, expect_ty, ctx)?;
            let annotation = if_cond.annotation.clone();
            Ok(ast::ExpressionNode::new(if_cond, annotation))
        },

        ast::Expression::Block(block) => {
            let block = typecheck_block(block, expect_ty, ctx)?;
            let annotation = block.annotation.clone();
            Ok(ast::ExpressionNode::new(block, annotation))
        },
    }
}

pub fn ns_member_ref_to_annotation(
    member: MemberRef<Scope>,
    span: Span,
    ctx: &Context
) -> TypeAnnotation {
    match member {
        MemberRef::Value {
            value: Decl::Alias(aliased),
            ..
        } => {
            let alias_ref = ctx.resolve(aliased)
                .unwrap_or_else(|| panic!("invalid alias to {}", aliased));

            ns_member_ref_to_annotation(alias_ref, span, ctx)
        },

        MemberRef::Value {
            value: Decl::BoundValue(binding),
            ..
        } => TypeAnnotation::TypedValue {
            span,
            ty: binding.ty.clone(),
            value_kind: binding.kind,
        },

        MemberRef::Value {
            value: Decl::Function(sig),
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
                ty: Type::Function(sig.clone()),
            }
        },

        MemberRef::Value {
            value: Decl::Type(ty),
            ..
        } => TypeAnnotation::Type(ty.clone(), span),

        MemberRef::Namespace { path } => {
            TypeAnnotation::Namespace(IdentPath::from_parts(path.keys().cloned()), span)
        },
    }
}
