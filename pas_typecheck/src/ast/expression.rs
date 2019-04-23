use crate::{ast::prelude::*, ty::FunctionParamSig};
use pas_common::span::*;
use pas_syn::ast;

pub type MethodCall = ast::MethodCall<TypeAnnotation>;
pub type FunctionCall = ast::FunctionCall<TypeAnnotation>;
pub type Call = ast::Call<TypeAnnotation>;
pub type Expression = ast::Expression<TypeAnnotation>;

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
                /* in-refs shouldn't really allow uninitialized values here but we do init checking
                in a separate pass */
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
        if expected.ty == Type::GenericSelf {
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

pub fn typecheck_call(call: &ast::Call<Span>, ctx: &mut Context) -> TypecheckResult<Call> {
    let func_call = match call {
        ast::Call::Function(func_call) => func_call,
        _ => unreachable!("parsing cannot result in anything except FunctionCall"),
    };

    let target = typecheck_expr(&func_call.target, &Type::Nothing, ctx)?;

    let call = match target.annotation() {
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
        }

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
                None,
                &span,
                ctx,
            )?;

            let arg_tys: Vec<_> = args.iter().map(|a| a.annotation().ty().clone()).collect();

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
        }

        Some(self_arg) => {
            // we have a self arg, we know how to specialize the signature before checking the
            // arg types
            let self_type = self_arg.annotation().ty().clone();
            let impl_sig = method_annotation.decl_sig().with_self(&self_type);

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
        self_type,
        func_type: Type::Function(impl_sig.into()),
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
    let args = typecheck_args(&sig.params, &func_call.args, None, &span, ctx)?;

    let return_ty = sig.return_ty.clone();

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
        args,
        target,
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
                ty: ctx.string_type()?,
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
            let annotation = TypeAnnotation::TypedValue {
                ty: Type::Nil,
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

        ast::Expression::BinOp(bin_op) => typecheck_bin_op(bin_op, ctx),

        ast::Expression::UnaryOp(unary_op) => {
            let unary_op = typecheck_unary_op(unary_op, ctx)?;
            Ok(ast::Expression::from(unary_op))
        }

        ast::Expression::Call(call) => {
            let call = typecheck_call(call, ctx)?;
            Ok(ast::Expression::from(call))
        }

        ast::Expression::ObjectCtor(ctor) => {
            let ctor = typecheck_object_ctor(ctor, ctx)?;
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
        }

        MemberRef::Value {
            value: Decl::Type(ty),
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

        ast::Statement::Exit(exit) => match exit {
            ast::Exit::WithoutValue(_) => Ok(()),
            ast::Exit::WithValue(exit_val) => expect_expr_initialized(exit_val, ctx),
        },

        ast::Statement::Assignment(assignment) => expect_expr_initialized(&assignment.rhs, ctx),
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
            },

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
        args.iter().map(|arg| arg.to_string()).collect::<Vec<_>>().join("; "),
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
