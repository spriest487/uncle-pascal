use crate::ast::prelude::*;
use pas_syn::Operator;

pub type LocalBinding = ast::LocalBinding<TypeAnnotation>;
pub type Statement = ast::Statement<TypeAnnotation>;

pub fn typecheck_local_binding(
    binding: &ast::LocalBinding<Span>,
    ctx: &mut Context,
) -> TypecheckResult<LocalBinding> {
    let (val, binding_ty) = match &binding.val_ty {
        ast::TypeName::Unknown(_) => match &binding.val {
            None => {
                return Err(TypecheckError::UninitBindingWithNoType {
                    binding: binding.clone(),
                });
            }

            Some(val) => {
                let val = typecheck_expr(val, &Type::Nothing, ctx)?;
                let val_ty = val.annotation().ty().clone();
                (Some(val), val_ty)
            }
        },

        val_ty => {
            let explicit_ty = ctx.find_type(val_ty)?;
            let val = match &binding.val {
                Some(val) => {
                    let val = typecheck_expr(val, &explicit_ty, ctx)?;

                    if !explicit_ty.assignable_from(val.annotation().ty()) {
                        return Err(TypecheckError::InvalidBinOp {
                            lhs: explicit_ty.clone(),
                            rhs: val.annotation().ty().clone(),
                            op: Operator::Assignment,
                            span: val.annotation().span().clone(),
                        });
                    }

                    Some(val)
                }
                None => None,
            };

            (val, explicit_ty)
        }
    };

    if binding_ty == Type::Nothing {
        return Err(TypecheckError::BindingWithNoType {
            binding: binding.clone(),
        });
    }

    ctx.declare_binding(
        binding.name.clone(),
        Binding {
            kind: if binding.mutable {
                ValueKind::Mutable
            } else {
                ValueKind::Immutable
            },
            ty: binding_ty.clone(),
            def: Some(binding.annotation.clone()),
        },
    )?;

    if val.is_some() && binding.mutable {
        ctx.initialize(&binding.name);
    }

    let annotation = TypeAnnotation::Untyped(binding.annotation.span().clone());

    Ok(LocalBinding {
        name: binding.name.clone(),
        val_ty: binding_ty,
        val,
        annotation,
        mutable: binding.mutable,
    })
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
            value_kind: ValueKind::Mutable,
            ..
        } => {}
        TypeAnnotation::TypedValue { span, .. } => {
            let span = span.clone();
            return Err(TypecheckError::NotMutable {
                expr: Box::new(lhs),
                decl: Some(span),
            });
        }
        _ => {
            return Err(TypecheckError::NotMutable {
                expr: Box::new(lhs),
                decl: None,
            });
        }
    }

    let rhs = typecheck_expr(&assignment.rhs, lhs.annotation().ty(), ctx)?;

    if !lhs.annotation().ty().assignable_from(rhs.annotation().ty()) {
        return Err(TypecheckError::InvalidBinOp {
            lhs: lhs.annotation().ty().clone(),
            rhs: rhs.annotation().ty().clone(),
            op: Operator::Assignment,
            span: rhs.annotation().span().clone(),
        });
    }

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

pub type IfStatement = ast::IfStatement<TypeAnnotation>;

fn typecheck_if_stmt(
    if_stmt: &ast::IfStatement<Span>,
    ctx: &mut Context,
) -> TypecheckResult<IfStatement> {
    let cond = typecheck_expr(&if_stmt.cond, &Type::Nothing, ctx)?;

    let mut then_ctx = ctx.clone();
    let then_branch = typecheck_stmt(&if_stmt.then_branch, &mut then_ctx)?;

    let else_branch = match &if_stmt.else_branch {
        Some(else_branch) => {
            let mut else_ctx = ctx.clone();
            let else_stmt = typecheck_stmt(&else_branch, &mut else_ctx)?;
            ctx.consolidate_branches(&[then_ctx, else_ctx]);
            Some(else_stmt)
        },
        None => {
            ctx.consolidate_branches(&[then_ctx]);
            None
        },
    };

    let annotation = TypeAnnotation::Untyped(if_stmt.span().clone());

    Ok(IfStatement {
        cond,
        then_branch: Box::new(then_branch),
        else_branch: else_branch.map(Box::new),
        annotation,
    })
}

pub fn typecheck_stmt(
    stmt: &ast::Statement<Span>,
    ctx: &mut Context,
) -> TypecheckResult<Statement> {
    match stmt {
        ast::Statement::LocalBinding(binding) => {
            typecheck_local_binding(binding, ctx).map(ast::Statement::LocalBinding)
        }

        ast::Statement::Call(call) => typecheck_call(call, ctx).map(ast::Statement::Call),

        ast::Statement::Block(block) => {
            let block = typecheck_block(block, &Type::Nothing, ctx)?;
            assert_eq!(&Type::Nothing, block.annotation.ty());
            assert!(block.annotation.is_untyped());

            Ok(ast::Statement::Block(block))
        }

        ast::Statement::ForLoop(for_loop) => {
            typecheck_for_loop(for_loop, ctx).map(ast::Statement::ForLoop)
        }

        ast::Statement::Assignment(assignment) => {
            typecheck_assignment(assignment, ctx).map(ast::Statement::Assignment)
        }

        ast::Statement::Exit(_exit) => unimplemented!(),

        ast::Statement::If(if_stmt) => typecheck_if_stmt(if_stmt, ctx).map(ast::Statement::If),
    }
}
