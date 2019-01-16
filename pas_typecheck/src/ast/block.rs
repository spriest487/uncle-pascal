use crate::ast::prelude::*;

pub type Block = ast::Block<TypeAnnotation>;

pub fn typecheck_block(block: &ast::Block<Span>,
        expect_ty: &Type,
        ctx: &mut Context) -> TypecheckResult<Block> {
    let mut statements = Vec::new();
    for stmt in &block.statements {
        statements.push(typecheck_stmt(stmt, ctx)?);
    }

    let output = match &block.output {
        Some(expr) => Some(typecheck_expr(expr, ctx)?),

        // parsing alone can't find all the cases where the final statement
        // in a block is a typed expression indicating the output, for example
        // if the block finishes on a call. if we're expecing a return type and
        // parsing didn't find us the output expression, we can move the final
        // stmt into the output if the type matches
        None if *expect_ty != Type::None => {
            let last_stmt_type = statements.last()
                .map(|s: &Statement| &s.annotation().ty);

            if last_stmt_type == Some(expect_ty) {
                Some(statements.pop()
                    .and_then(|s| s.try_into_expr())
                    .unwrap())
            } else {
                None
            }
        }

        None => None,
    };

    if *expect_ty != Type::None {
        let output_ty = output.as_ref()
            .map(|o| &o.annotation.ty);

        if output_ty != Some(expect_ty) {
            return Err(TypecheckError::TypeMismatch {
                expected: expect_ty.clone(),
                actual: output_ty.cloned().unwrap_or(Type::None),
                span: match &output {
                    Some(expr) => expr.annotation.span.clone(),
                    None => block.end.clone(),
                }
            })
        }
    }

    let span = block.annotation.span().clone();
    let annotation = match &output {
        Some(out_expr) => {
            let out_ty = out_expr.annotation.ty.clone();
            assert_ne!(Type::None, out_ty);
            TypeAnnotation::typed_value(out_ty, ValueKind::Temporary, span)
        },
        None => TypeAnnotation::untyped(span),
    };

    Ok(Block {
        annotation,
        output,
        statements,

        begin: block.begin.clone(),
        end: block.end.clone(),
    })
}