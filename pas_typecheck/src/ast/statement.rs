use {
    crate::{
        ast::prelude::*,
    },
};

pub type LetBinding = ast::LetBinding<TypeAnnotation>;
pub type Statement = ast::Statement<TypeAnnotation>;

pub fn typecheck_stmt(stmt: &ast::Statement<Span>, ctx: &mut Context) -> TypecheckResult<Statement> {
    let span = stmt.annotation().clone();

    match stmt {
        ast::Statement::LetBinding(let_binding) => {
            let val_ty = ctx.find_type(&let_binding.val_ty)?.clone();
            let val = typecheck_expr(&let_binding.val, &val_ty, ctx)?;

            let binding = Binding {
                kind: ValueKind::Immutable,
                ty: val_ty.clone(),
            };

            ctx.declare_binding(let_binding.name.clone(), binding)?;

            let annotation = TypeAnnotation::untyped(span);

            Ok(ast::Statement::LetBinding(LetBinding {
                name: let_binding.name.clone(),
                val_ty,
                val,
                annotation,
            }))
        }

        ast::Statement::Call(call) => {
            let call = typecheck_call(call, ctx)?;
            Ok(ast::Statement::Call(call))
        }

        ast::Statement::Block(block) => {
            let block = typecheck_block(block, &Type::Nothing, ctx)?;
            assert_eq!(Type::Nothing, block.annotation.ty);
            assert_eq!(None, block.annotation.value_kind);

            Ok(ast::Statement::Block(block))
        }

        ast::Statement::Exit(_exit) => {
            unimplemented!()
        }
    }
}
