use pas_common::span::*;
use pas_syn::ast;

use crate::{ast::prelude::*, ty::FunctionParamSig};

pub type Expression = ast::Expression<TypeAnnotation>;

pub use call::{typecheck_call, Call, CallOrCtor, FunctionCall, MethodCall, VariantCtorCall};

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
            let ty = match expect_ty {
                Type::Primitive(Primitive::Byte) => {
                    match i.as_u8() {
                        Some(_) => Type::from(Primitive::Byte),
                        None => Type::Primitive(Primitive::Int32),
                    }
                },
                Type::Primitive(Primitive::Real32) => {
                    match i.as_f32() {
                        Some(_) => Type::from(Primitive::Real32),
                        None => Type::Primitive(Primitive::Int32),
                    }
                },
                _ => {
                    match i.as_f32() {
                        Some(_) => Type::from(Primitive::Int32),
                        None => unimplemented!("integer literals outside range of i32"),
                    }
                }
            };

            let annotation = TypeAnnotation::TypedValue {
                ty,
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
