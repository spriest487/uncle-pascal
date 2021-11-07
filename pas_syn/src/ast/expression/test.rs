use super::*;
use crate::token_tree::TokenTree;
use pas_common::BuildOptions;

fn tokenize(src: &str) -> TokenStream {
    let tokens = TokenTree::tokenize("test", src, &BuildOptions::default()).unwrap();

    TokenStream::new(tokens, Span::zero("test"))
}

fn try_parse_expr(src: &str) -> ParseResult<Expression<Span>> {
    let mut tokens = tokenize(src);
    Expression::parse(&mut tokens).and_then(|expr| {
        tokens.finish()?;
        Ok(expr)
    })
}

pub fn parse_expr(src: &str) -> Expression<Span> {
    try_parse_expr(src).unwrap()
}

/// for an expression like x := Y() we should read it as x := (Y()) not (x := Y)()
#[test]
fn parse_assignment_then_call_in_order() {
    match parse_expr("x := Y()") {
        Expression::BinOp(bin_op) => {
            let lhs_ident = bin_op.lhs.as_ident().unwrap_or_else(|| {
                panic!(
                    "parsed expr should have ident x on the lhs, found {}",
                    bin_op.lhs
                )
            });
            assert_eq!("x", lhs_ident.name);
        }

        invalid => panic!("expected binary op, got `{:#?}`", invalid),
    }
}

#[test]
fn parse_assignment_then_call_in_order_explicit() {
    match parse_expr("x := (Y())") {
        Expression::BinOp(bin_op) => {
            let lhs_ident = bin_op.lhs.as_ident().unwrap_or_else(|| {
                panic!(
                    "parsed expr should have ident x on the lhs, found {}",
                    bin_op.lhs
                )
            });
            assert_eq!("x", lhs_ident.name);
        }

        invalid => panic!("expected binary op, got `{:#?}`", invalid),
    }
}

#[test]
fn invocation_with_empty_type_args_is_error() {
    let input = "A[]()";
    match try_parse_expr(input) {
        Ok(invalid) => panic!("expected error, got expression {:#?}", invalid),
        Err(TracedError { err: ParseError::EmptyTypeArgList(..), .. }) => {},
        Err(invalid) => panic!("unexpected error: {}", invalid.err),
    }
}

#[test]
fn parse_invocation_with_type_args() {
    match parse_expr("A[B, C]()") {
        Expression::Call(call) => match call.as_ref() {
            Call::Function(func_call) => {
                let target_ident = func_call.target.as_ident().unwrap_or_else(|| {
                    panic!("parsed func call should have ident as the call target");
                });
                assert_eq!(target_ident.name.as_str(), "A");

                let type_args = func_call.type_args.as_ref().unwrap_or_else(|| {
                    panic!("parsed func call should have type args")
                });

                let expect_args = [ "B", "C" ];
                assert_eq!(expect_args.len(), type_args.items.len());

                for (&expected, actual) in expect_args.iter().zip(type_args.items.iter()) {
                    match actual {
                        TypeName::Ident { ident, .. } => {
                            assert_eq!(None, ident.parent());
                            assert_eq!(expected, ident.last().name.as_str());
                        }

                        invalid => panic!("expected all args to be idents, got {:#?}", invalid)
                    }
                }
            },
            invalid => panic!("expected function call, got {:#?}", invalid)
        },
        invalid => panic!("expected call, got {:#?}", invalid)
    }
}

#[test]
fn member_indexer_parse_order() {
    let expr = parse_expr("x.y[1]");

    match expr {
        Expression::Indexer(indexer) => {
            match indexer.base {
                Expression::BinOp(bin_op) => {
                    let lhs_ident = bin_op.lhs.as_ident().expect("lhs should be an ident");
                    let rhs_ident = bin_op.rhs.as_ident().expect("rhs should be an ident");

                    assert_eq!(lhs_ident.name, "x");
                    assert_eq!(rhs_ident.name, "y");
                }
                invalid => panic!("expected indexer base expression, got {:#?}", invalid),
            }

            match indexer.index {
                Expression::Literal(Literal::Integer(int_const), ..) => {
                    assert_eq!(1, int_const.as_i32().unwrap());
                }
                invalid => panic!("expected indexer index expression, got {:#?}", invalid),
            }
        }

        invalid => panic!("expected indexer expression, got {:#?}", invalid)
    }
}