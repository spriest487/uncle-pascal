use {
    pas_syn::Operator,
    crate::{
        ast::prelude::*,
    },
};

pub type LocalBinding = ast::LocalBinding<TypeAnnotation>;
pub type Statement = ast::Statement<TypeAnnotation>;

pub fn typecheck_local_binding(
    binding: &ast::LocalBinding<Span>,
    ctx: &mut Context)
    -> TypecheckResult<LocalBinding>
{
    let val_ty = ctx.find_type(&binding.val_ty)?.clone();
    let val = typecheck_expr(&binding.val, &val_ty, ctx)?;

    ctx.declare_binding(binding.name.clone(), Binding {
        kind: if binding.mutable {
            ValueKind::Mutable
        } else {
            ValueKind::Immutable
        },
        ty: val_ty.clone(),
    })?;

    let annotation = TypeAnnotation::untyped(binding.annotation.clone());

    Ok(LocalBinding {
        name: binding.name.clone(),
        val_ty,
        val,
        annotation,
        mutable: binding.mutable,
    })
}

pub type Assignment = ast::Assignment<TypeAnnotation>;

pub fn typecheck_assignment(
    assignment: &ast::Assignment<Span>,
    ctx: &mut Context)
    -> TypecheckResult<Assignment>
{
    let lhs = typecheck_expr(&assignment.lhs, &Type::Nothing, ctx)?;
    if lhs.annotation.value_kind != Some(ValueKind::Mutable) {
        return Err(TypecheckError::NotMutable(Box::new(lhs)));
    }

    let rhs = typecheck_expr(&assignment.rhs, &lhs.annotation.ty, ctx)?;

    if !lhs.annotation.ty.assignable_from(&rhs.annotation.ty) {
        return Err(TypecheckError::InvalidBinOp {
            lhs: lhs.annotation.ty,
            rhs: rhs.annotation.ty,
            op: Operator::Assignment,
            span: rhs.annotation.span,
        });
    }

    Ok(Assignment {
        lhs,
        rhs,
        annotation: TypeAnnotation::untyped(assignment.annotation.clone()),
    })
}

pub fn typecheck_stmt(stmt: &ast::Statement<Span>, ctx: &mut Context) -> TypecheckResult<Statement> {
    match stmt {
        ast::Statement::LocalBinding(binding) => {
            typecheck_local_binding(binding, ctx)
                .map(ast::Statement::LocalBinding)
        }

        ast::Statement::Call(call) => {
            typecheck_call(call, ctx)
                .map(ast::Statement::Call)
        }

        ast::Statement::Block(block) => {
            let block = typecheck_block(block, &Type::Nothing, ctx)?;
            assert_eq!(Type::Nothing, block.annotation.ty);
            assert_eq!(None, block.annotation.value_kind);

            Ok(ast::Statement::Block(block))
        }

        ast::Statement::ForLoop(for_loop) => {
            typecheck_for_loop(for_loop, ctx)
                .map(ast::Statement::ForLoop)
        }

        ast::Statement::Assignment(assignment) => {
            typecheck_assignment(assignment, ctx)
                .map(ast::Statement::Assignment)
        }

        ast::Statement::Exit(_exit) => {
            unimplemented!()
        }
    }
}
