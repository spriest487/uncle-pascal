pub mod statement;
pub mod unit;

pub use self::{
    statement::*,
    unit::*,
};

use {
    crate::{
        Ident,
        IntConstant,
        Span,
        Matcher,
        TokenTree,
        TokenStream,
    },
    pas_common::TracedError,
};

#[derive(Debug)]
pub enum ParseError {
    UnexpectedToken(TokenTree, Option<Matcher>),
    UnexpectedEOF(Matcher, TokenTree),
}

pub type ParseResult<T> = Result<T, TracedError<ParseError>>;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Type {
    Ident(Ident),
}

impl Type {
    pub fn parse(tokens: &mut TokenStream) -> ParseResult<Self> {
        match tokens.match_one(Matcher::AnyIdent)? {
            TokenTree::Ident(ident) => Ok(Type::Ident(ident)),
            unexpected => {
                let expected = Matcher::AnyIdent;
                Err(ParseError::UnexpectedToken(unexpected, Some(expected)).into())
            },
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Expression {
    LiteralInt { value: IntConstant, span: Span },
}

impl Expression {
    pub fn parse(tokens: &mut TokenStream) -> ParseResult<Self> {
        match tokens.next() {
            Some(TokenTree::IntNumber { value, span }) => {
                let expr = Expression::LiteralInt { value, span };
                Ok(expr)
            }

            _ => unimplemented!()
        }
    }

    pub fn span(&self) -> &Span {
        match self {
            Expression::LiteralInt { span, .. } => span,
        }
    }
}