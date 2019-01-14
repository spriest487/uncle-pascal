mod token_stream;
mod matcher;

use {
    std::{
        fmt,
    },
    pas_common::TracedError,
    crate::{
        token_tree::*,
        ast::*,
        span::*,
    },
};

pub use self::{
    matcher::*,
    token_stream::*,
};

#[derive(Debug)]
pub enum ParseError {
    UnexpectedToken(TokenTree, Option<Matcher>),
    UnexpectedEOF(Matcher, Span),
    EmptyOperand { operator: TokenTree, before: bool },
    InvalidStatement(ExpressionNode<Span>),
}

pub type ParseResult<T> = Result<T, TracedError<ParseError>>;

impl Spanned for ParseError {
    fn span(&self) -> &Span {
        match self {
            ParseError::UnexpectedToken(tt, _) => tt.span(),
            ParseError::UnexpectedEOF(_, tt) => tt.span(),
            ParseError::EmptyOperand { operator, .. } => operator.span(),
            ParseError::InvalidStatement(expr) => &expr.annotation,
        }
    }
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ParseError::UnexpectedToken(tt, Some(expected)) =>
                write!(f, "expected {}, found {}", expected, tt),

            ParseError::UnexpectedToken(tt, None) =>
                write!(f, "unexpected {}", tt),

            ParseError::UnexpectedEOF(expected, tt) =>
                write!(f, "expected {} after {} but reached end of sequence", expected, tt),

            ParseError::EmptyOperand { operator, before } => {
                let pos_name = if *before { "before" } else { "after" };
                write!(f, "expected operand {} {}", pos_name, operator)
            }

            ParseError::InvalidStatement(expr) => {
                write!(f, "the expression {} is not valid as a statement", expr)
            }
        }
    }
}