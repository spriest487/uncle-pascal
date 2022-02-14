mod assign;

use crate::ast::{
    prelude::*,
    expression::{Invocation, typecheck_call},
    statement::assign::{typecheck_assignment, typecheck_compound_assignment},
};
use pas_syn::Operator;
use crate::ast::cast::implicit_conversion;
pub use self::assign::{
    Assignment,
    CompoundAssignment,
};

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
                let val_ty = val.annotation().ty().into_owned();
                (Some(val), val_ty)
            }
        },

        val_ty => {
            let explicit_ty = typecheck_type(val_ty, ctx)?;
            if explicit_ty.is_unspecialized_generic() {
                return Err(TypecheckError::from_generic_err(GenericError::IllegalUnspecialized {
                    ty: explicit_ty,
                }, binding.span().clone()));
            }

            let val = match &binding.val {
                Some(val) => {
                    let val = typecheck_expr(val, &explicit_ty, ctx)?;

                    let val = implicit_conversion(val, &explicit_ty, ctx)
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
        return Err(TypecheckError::from_generic_err(GenericError::IllegalUnspecialized {
            ty: binding_ty,
        }, binding.span().clone()));
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

pub fn typecheck_stmt(
    stmt: &ast::Statement<Span>,
    expect_ty: &Type,
    ctx: &mut Context,
) -> TypecheckResult<Statement> {
    match stmt {
        ast::Statement::LocalBinding(binding) => {
            typecheck_local_binding(binding, ctx).map(|s| ast::Statement::LocalBinding(Box::new(s)))
        }

        ast::Statement::Call(call) => match typecheck_call(call, expect_ty, ctx)? {
            Invocation::Call(call) => Ok(ast::Statement::Call(call)),
            Invocation::Ctor(ctor) => {
                let ctor_expr = Expression::from(*ctor);
                let invalid_stmt = InvalidStatement::from(ctor_expr);
                Err(TypecheckError::InvalidStatement(Box::new(invalid_stmt)))
            }
        },

        ast::Statement::Block(block) => {
            let block = typecheck_block(block, expect_ty, ctx)?;

            Ok(ast::Statement::Block(Box::new(block)))
        }

        ast::Statement::ForLoop(for_loop) => {
            typecheck_for_loop(for_loop, ctx).map(Box::new).map(ast::Statement::ForLoop)
        }

        ast::Statement::WhileLoop(while_loop) => {
            typecheck_while_loop(while_loop, ctx).map(Box::new).map(ast::Statement::WhileLoop)
        }

        ast::Statement::Assignment(assignment) => {
            typecheck_assignment(assignment, ctx).map(Box::new).map(ast::Statement::Assignment)
        }

        ast::Statement::CompoundAssignment(assignment) => {
            typecheck_compound_assignment(assignment, ctx).map(Box::new).map(ast::Statement::CompoundAssignment)
        }

        ast::Statement::Exit(exit) => {
            let exit = typecheck_exit(exit, &expect_ty, ctx)?;
            Ok(ast::Statement::Exit(Box::new(exit)))
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
            let if_cond = typecheck_if_cond_stmt(if_cond, expect_ty, ctx)?;
            Ok(ast::Statement::If(Box::new(if_cond)))
        }

        ast::Statement::Raise(raise) => {
            let raise = typecheck_raise(raise, expect_ty, ctx)?;
            Ok(ast::Statement::Raise(Box::new(raise)))
        }

        ast::Statement::Case(case) => {
            let case = typecheck_case_stmt(case, expect_ty, ctx)?;
            Ok(ast::Statement::Case(Box::new(case)))
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

pub fn typecheck_exit(exit: &ast::Exit<Span>, expect_ty: &Type, ctx: &mut Context) -> TypecheckResult<Exit> {
    let ret_ty = ctx.current_func_return_ty()
        .ok_or_else(|| {
            TypecheckError::NoFunctionContext {
                stmt: Box::new(ast::Statement::Exit(Box::new(exit.clone())))
            }
        })?
        .clone();

    // exit expressions always count as the expected type, so you can write e.g.
    // `let x := if true then 1 else exit;`
    // since no value will ever be assigned to `x` if the exit expression is reached
    let make_annotation = |span: &Span| match expect_ty {
        Type::Nothing => TypeAnnotation::Untyped(span.clone()),
        _ => TypedValueAnnotation {
            span: span.clone(),
            value_kind: ValueKind::Temporary,
            ty: expect_ty.clone(),
            decl: None,
        }.into()
    };

    let exit = match exit {
        ast::Exit::WithoutValue(span) => {
            Exit::WithoutValue(make_annotation(span).into())
        },

        ast::Exit::WithValue(value, span) => {
            let value = typecheck_expr(value, &ret_ty, ctx)?;
            let value = implicit_conversion(value, &ret_ty, ctx)?;

            Exit::WithValue(value, make_annotation(span).into())
        }
    };

    Ok(exit)
}