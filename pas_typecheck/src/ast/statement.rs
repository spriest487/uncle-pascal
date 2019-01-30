use {
    crate::{
        ast::prelude::*,
    },
};

pub type LetBinding = ast::LetBinding<TypeAnnotation>;
pub type Statement = ast::Statement<TypeAnnotation>;

pub fn typecheck_let_binding(
    let_binding: &ast::LetBinding<Span>,
    ctx: &mut Context)
    -> TypecheckResult<LetBinding>
{
    let val_ty = ctx.find_type(&let_binding.val_ty)?.clone();
    let val = typecheck_expr(&let_binding.val, &val_ty, ctx)?;

    let binding = Binding {
        kind: ValueKind::Immutable,
        ty: val_ty.clone(),
    };

    ctx.declare_binding(let_binding.name.clone(), binding)?;

    let annotation = TypeAnnotation::untyped(let_binding.annotation.clone());

    Ok(LetBinding {
        name: let_binding.name.clone(),
        val_ty,
        val,
        annotation,
    })
}

pub fn typecheck_stmt(stmt: &ast::Statement<Span>, ctx: &mut Context) -> TypecheckResult<Statement> {
    match stmt {
        ast::Statement::LetBinding(let_binding) => {
            typecheck_let_binding(let_binding, ctx)
                .map(ast::Statement::LetBinding)
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

        ast::Statement::Exit(_exit) => {
            unimplemented!()
        }
    }
}
