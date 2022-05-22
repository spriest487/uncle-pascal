use pas_common::span::{Span, Spanned};
use pas_syn::ast;
use crate::{ast::{
    cast::implicit_conversion,
    expect_expr_initialized,
    expect_stmt_initialized,
    typecheck_expr,
    typecheck_stmt,
}, Context, Environment, Type, TypeAnnotation, TypecheckError, TypecheckResult, TypedValueAnnotation, ValueKind};

pub type Block = ast::Block<TypeAnnotation>;

pub fn typecheck_block(
    block: &ast::Block<Span>,
    expect_ty: &Type,
    ctx: &mut Context,
) -> TypecheckResult<Block> {
    let block_scope = ctx.push_scope(Environment::Block {
        allow_unsafe: block.unsafe_kw.is_some(),
    });

    let mut statements = Vec::new();
    let mut output = None;

    let expect_output = *expect_ty != Type::Nothing;

    for (i, stmt) in block.stmts.iter().enumerate() {
        let is_last_stmt = i == block.stmts.len() - 1;

        if is_last_stmt && expect_output && block.output.is_none() {
            // this is the final stmt in the block, and during parsing this block didn't
            // get an output expr. we expect this block to have an output, so try to convert
            // the final stmt here into an expr
            match stmt.to_expr() {
                Some(src_output_stmt_expr) => {
                    let mut output_stmt_expr = typecheck_expr(&src_output_stmt_expr, expect_ty, ctx)?;
                    if *expect_ty != Type::Nothing {
                        output_stmt_expr = implicit_conversion(output_stmt_expr, expect_ty, ctx)?;
                    }
                    output = Some(output_stmt_expr);
                },

                None => {
                    // typecheck the actual stmt which isn't a valid expr so we can use it
                    // for a better error message
                    let bad_stmt = typecheck_stmt(&stmt, expect_ty, ctx)?;
                    return Err(TypecheckError::BlockOutputIsNotExpression {
                        stmt: Box::new(bad_stmt),
                        expected_expr_ty: expect_ty.clone(),
                    });
                },
            }

            continue;
        }

        let stmt = typecheck_stmt(stmt, &Type::Nothing, ctx)?;
        expect_stmt_initialized(&stmt, ctx)?;
        statements.push(stmt);
    }

    // process the parsed output expr (this is mutually exclusive with converting the final
    // stmt into an output)
    // the block's body statements can alter the context by declaring vars, initializing decls,
    // etc, so this has to be checked *after* we've processed the rest of the statements
    if let Some(src_output_expr) = &block.output {
        // we should not have tried to interpret any statements as output expressions
        assert_eq!(None, output);

        let mut out_expr = typecheck_expr(src_output_expr, expect_ty, ctx)?;
        if *expect_ty != Type::Nothing {
            out_expr = implicit_conversion(out_expr, expect_ty, ctx)?;
        }
        output = Some(out_expr);
    }

    if let Some(output_expr) = &output {
        expect_expr_initialized(output_expr, ctx)?;
    }

    let span = block.annotation.span().clone();
    let annotation = match &output {
        Some(out_expr) => {
            if *out_expr.annotation().ty() == Type::Nothing {
                TypeAnnotation::Untyped(span)
            } else {
                let out_ty = out_expr.annotation().ty().into_owned();
                TypedValueAnnotation {
                    ty: out_ty,
                    value_kind: ValueKind::Temporary,
                    span,
                    decl: None,
                }
                .into()
            }
        },
        None => TypeAnnotation::Untyped(span),
    };

    let block = Block {
        annotation,
        output,
        stmts: statements,

        begin: block.begin.clone(),
        end: block.end.clone(),

        unsafe_kw: block.unsafe_kw.clone(),
    };

    assert_eq!(*block.annotation.ty(), {
        let out_ty = block
            .output
            .as_ref()
            .map(|o| o.annotation().ty().into_owned());
        out_ty.unwrap_or(Type::Nothing)
    });

    ctx.pop_scope(block_scope);

    Ok(block)
}
