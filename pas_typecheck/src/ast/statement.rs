use crate::ast::prelude::*;
use pas_syn::Operator;
use crate::ast::expression::{CallOrCtor, typecheck_call};

pub type LocalBinding = ast::LocalBinding<TypeAnnotation>;
pub type Statement = ast::Statement<TypeAnnotation>;
pub type Exit = ast::Exit<TypeAnnotation>;

pub fn typecheck_local_binding(
    binding: &ast::LocalBinding<Span>,
    ctx: &mut Context,
) -> TypecheckResult<LocalBinding> {
    let (val, binding_ty) = match &binding.val_ty {
        ast::TypeName::Unknown(_) => match &binding.val {
            None => {
                return Err(TypecheckError::UninitBindingWithNoType {
                    binding: Box::new(binding.clone()),
                });
            }

            Some(val) => {
                let val = typecheck_expr(val, &Type::Nothing, ctx)?;
                let val_ty = val.annotation().ty().clone();
                (Some(val), val_ty)
            }
        },

        val_ty => {
            let explicit_ty = typecheck_type(val_ty, ctx)?;
            if explicit_ty.is_unspecialized_generic() {
                return Err(GenericError::IllegalUnspecialized {
                    ty: explicit_ty,
                    span: binding.val_ty.span().clone(),
                }.into());
            }

            let val = match &binding.val {
                Some(val) => {
                    let val = typecheck_expr(val, &explicit_ty, ctx)?;

                    explicit_ty.implicit_conversion_from(val.annotation().ty(), val.span(), ctx)
                        .map_err(|err| match err {
                            TypecheckError::TypeMismatch { expected, actual, span, .. } => {
                                TypecheckError::InvalidBinOp {
                                    lhs: expected,
                                    rhs: actual,
                                    op: Operator::Assignment,
                                    span,
                                }
                            },

                            err => err
                        })?;

                    Some(val)
                }
                None => None,
            };

            (val, explicit_ty)
        }
    };

    if binding_ty == Type::Nothing {
        return Err(TypecheckError::BindingWithNoType {
            binding: Box::new(binding.clone()),
        });
    }

    if binding_ty.is_unspecialized_generic() {
        return Err(GenericError::IllegalUnspecialized {
            ty: binding_ty,
            span: binding.val_ty.span().clone(),
        }.into());
    }

    let name = binding.name.clone();
    let mutable = binding.mutable;
    let span = binding.annotation.span().clone();

    let binding = Binding {
        kind: match (binding.mutable, val.is_some()) {
            (true, true) => ValueKind::Mutable,
            (true, false) => ValueKind::Uninitialized,
            (false, _) => ValueKind::Immutable,
        },
        ty: binding_ty.clone(),
        def: Some(binding.annotation.clone()),
    };

    ctx.declare_binding(name.clone(), binding)?;

    let annotation = TypeAnnotation::Untyped(span);

    let local_binding = LocalBinding {
        name,
        val_ty: binding_ty,
        val,
        annotation,
        mutable,
    };

    Ok(local_binding)
}

pub type Assignment = ast::Assignment<TypeAnnotation>;

pub fn typecheck_assignment(
    assignment: &ast::Assignment<Span>,
    ctx: &mut Context,
) -> TypecheckResult<Assignment> {
    let lhs = typecheck_expr(&assignment.lhs, &Type::Nothing, ctx)?;

    // lhs must evaluate to a mutable typed value
    match lhs.annotation() {
        TypeAnnotation::TypedValue {
            value_kind, decl, ..
        } => {
            if !value_kind.mutable() {
                return Err(TypecheckError::NotMutable {
                    decl: decl.clone(),
                    expr: Box::new(lhs),
                });
            }
        }
        _ => {
            return Err(TypecheckError::NotMutable {
                expr: Box::new(lhs),
                decl: None,
            });
        }
    }

    let rhs = typecheck_expr(&assignment.rhs, lhs.annotation().ty(), ctx)?;
    let rhs_ty = rhs.annotation().ty();

    lhs.annotation().ty().implicit_conversion_from(rhs_ty, assignment.span(), ctx).map_err(|err| {
        match err {
            TypecheckError::TypeMismatch { expected, actual, span } => TypecheckError::InvalidBinOp {
                lhs: expected,
                rhs: actual,
                op: Operator::Assignment,
                span,
            },

            err => err,
        }
    })?;

    if let ast::Expression::Ident(ident, ..) = &lhs {
        if ctx.is_local(ident) {
            ctx.initialize(ident);
        }
    }

    Ok(Assignment {
        lhs,
        rhs,
        annotation: TypeAnnotation::Untyped(assignment.annotation.clone()),
    })
}

pub fn typecheck_stmt(
    stmt: &ast::Statement<Span>,
    expect_ty: &Type,
    ctx: &mut Context,
) -> TypecheckResult<Statement> {
    match stmt {
        ast::Statement::LocalBinding(binding) => {
            typecheck_local_binding(binding, ctx).map(ast::Statement::LocalBinding)
        }

        ast::Statement::Call(call) => match typecheck_call(call, expect_ty, ctx)? {
            CallOrCtor::Call(call) => Ok(ast::Statement::Call(*call)),
            CallOrCtor::Ctor(ctor) => {
                let ctor_expr = Expression::from(*ctor);
                let invalid_stmt = InvalidStatement::from(ctor_expr);
                Err(TypecheckError::InvalidStatement(Box::new(invalid_stmt)))
            }
        },

        ast::Statement::Block(block) => {
            let block = typecheck_block(block, expect_ty, ctx)?;

            Ok(ast::Statement::Block(block))
        }

        ast::Statement::ForLoop(for_loop) => {
            typecheck_for_loop(for_loop, ctx).map(ast::Statement::ForLoop)
        }

        ast::Statement::WhileLoop(while_loop) => {
            typecheck_while_loop(while_loop, ctx).map(ast::Statement::WhileLoop)
        }

        ast::Statement::Assignment(assignment) => {
            typecheck_assignment(assignment, ctx).map(ast::Statement::Assignment)
        }

        ast::Statement::Exit(exit) => {
            let exit = typecheck_exit(exit, ctx)?;
            Ok(ast::Statement::Exit(exit))
        }

        ast::Statement::Break(span) => {
            expect_in_loop(stmt, ctx)?;
            let annotation = TypeAnnotation::Untyped(span.clone());
            Ok(ast::Statement::Break(annotation))
        }

        ast::Statement::Continue(span) => {
            expect_in_loop(stmt, ctx)?;
            let annotation = TypeAnnotation::Untyped(span.clone());
            Ok(ast::Statement::Continue(annotation))
        }

        ast::Statement::If(if_cond) => {
            let if_cond = typecheck_if_cond(if_cond, expect_ty, ctx)?;
            Ok(ast::Statement::If(Box::new(if_cond)))
        }

        ast::Statement::Raise(raise) => {
            let raise = typecheck_raise(raise, expect_ty, ctx)?;
            Ok(ast::Statement::Raise(Box::new(raise)))
        }
    }
}

fn expect_in_loop(stmt: &ast::Statement<Span>, ctx: &Context) -> TypecheckResult<()> {
    match ctx.in_loop() {
        Some(..) => Ok(()),

        None => Err(TypecheckError::NoLoopContext {
            stmt: Box::new(stmt.clone()),
        }),
    }
}

fn typecheck_exit(exit: &ast::Exit<Span>, ctx: &mut Context) -> TypecheckResult<Exit> {
    let expect_ty = ctx.current_func_return_ty()
        .ok_or_else(|| {
            TypecheckError::NoFunctionContext {
                stmt: Box::new(ast::Statement::Exit(exit.clone()))
            }
        })?
        .clone();

    let (exit, ret_ty) = match exit {
        ast::Exit::WithoutValue(span) => {
            let exit = ast::Exit::WithoutValue(TypeAnnotation::Untyped(span.clone()));
            (exit, Type::Nothing)
        },

        ast::Exit::WithValue(value, span) => {
            let value = typecheck_expr(value, &expect_ty, ctx)?;
            let ret_ty = value.annotation().ty().clone();

            let exit = ast::Exit::WithValue(value, TypeAnnotation::Untyped(span.clone()));

            (exit, ret_ty)
        }
    };

    expect_ty.implicit_conversion_from(&ret_ty, exit.span(), ctx)?;

    Ok(exit)
}
