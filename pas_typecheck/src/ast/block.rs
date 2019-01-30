use {
    crate::ast::prelude::*,
};

pub type Block = ast::Block<TypeAnnotation>;

pub fn typecheck_block(block: &ast::Block<Span>,
        expect_ty: &Type,
        ctx: &mut Context) -> TypecheckResult<Block> {
    let mut statements = Vec::new();
    for stmt in &block.statements {
        statements.push(typecheck_stmt(stmt, ctx)?);
    }

    let output = match &block.output {
        Some(expr) => {
            let out_expr = typecheck_expr(expr, expect_ty, ctx)?;
            if out_expr.annotation.ty == Type::Nothing {
                // the block contains a final expression which returns no value,
                // so it should just be treated like a statement
                let last_stmt = Statement::try_from_expr(out_expr)
                    .map_err(|invalid| TypecheckError::InvalidBlockOutput(Box::new(invalid)))?;
                statements.push(last_stmt);
                None
            } else {
                Some(out_expr)
            }
        },

        // parsing alone can't find all the cases where the final statement
        // in a block is a typed expression indicating the output, for example
        // if the block finishes on a call. if we're expecing a return type and
        // parsing didn't find us the output expression, we can move the final
        // stmt into the output if the type matches
        None if *expect_ty != Type::Nothing => {
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

    if *expect_ty != Type::Nothing {
        let output_ty = output.as_ref()
            .map(|o| &o.annotation.ty);

        if output_ty != Some(expect_ty) {
            return Err(TypecheckError::TypeMismatch {
                expected: expect_ty.clone(),
                actual: output_ty.cloned().unwrap_or(Type::Nothing),
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
            if out_expr.annotation.ty == Type::Nothing {
                TypeAnnotation::untyped(span)
            } else {
                let out_ty = out_expr.annotation.ty.clone();
                TypeAnnotation::typed_value(out_ty, ValueKind::Temporary, span)
            }
        },
        None => TypeAnnotation::untyped(span),
    };

    let block = Block {
        annotation,
        output,
        statements,

        begin: block.begin.clone(),
        end: block.end.clone(),
    };

    assert_eq!(block.annotation.ty, {
        let out_ty = block.output.as_ref().map(|o| o.annotation.ty.clone());
        out_ty.unwrap_or(Type::Nothing)
    });

    Ok(block)
}