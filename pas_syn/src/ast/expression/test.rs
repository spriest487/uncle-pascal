use super::*;
use crate::{
    token_tree::TokenTree,
};
use pas_common::BuildOptions;

fn tokenize(src: &str) -> TokenStream {
    let tokens = TokenTree::tokenize("test", src, &BuildOptions::default())
        .unwrap();

    TokenStream::new(tokens, Span::zero("test"))
}

fn try_parse_expr(src: &str) -> ParseResult<Expression<Span>> {
    let mut tokens = tokenize(src);
    Expression::parse(&mut tokens)
        .and_then(|expr| {
            tokens.finish()?;
            Ok(expr)
        })
}

fn parse_expr(src: &str) -> Expression<Span> {
    try_parse_expr(src).unwrap()
}

/// for an expression like x := Y() we should read it as x := (Y()) not (x := Y)()
#[test]
fn parse_assignment_then_call_in_order() {
    match parse_expr("x := Y()") {
        Expression::BinOp(bin_op) => {
            let lhs_ident = bin_op.lhs.as_ident()
                .unwrap_or_else(|| panic!("parsed expr should have ident x on the lhs, found {}", bin_op.lhs));
            assert_eq!("x", lhs_ident.name);
        }

        invalid => panic!("expected binary op, got `{:#?}`", invalid),
    }
}

#[test]
fn parse_assignment_then_call_in_order_explicit() {
    match parse_expr("x := (Y())") {
        Expression::BinOp(bin_op) => {
            let lhs_ident = bin_op.lhs.as_ident()
                .unwrap_or_else(|| panic!("parsed expr should have ident x on the lhs, found {}", bin_op.lhs));
            assert_eq!("x", lhs_ident.name);
        }

        invalid => panic!("expected binary op, got `{:#?}`", invalid),
    }
}