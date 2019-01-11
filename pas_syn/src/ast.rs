pub mod statement;
pub mod unit;
pub mod expression;
pub mod call;
pub mod function;
pub mod block;

pub use self::{
    statement::*,
    expression::*,
    unit::*,
    call::*,
    block::*,
    function::*,
};

use {
    std::{
        fmt,
        hash::Hash,
    },
    crate::{
        Ident,
        Span,
        Spanned,
        Matcher,
        TokenTree,
        TokenStream,
    },
    pas_common::TracedError,
};

pub trait Annotation : Spanned + Clone + PartialEq + Eq + Hash {
    type Type: fmt::Debug + fmt::Display + Clone + PartialEq + Eq + Hash;
}

impl Annotation for Span {
    type Type = TypeName;
}

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

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum TypeName {
    Ident(Ident),
}

impl Spanned for TypeName {
    fn span(&self) -> &Span {
        match self {
            TypeName::Ident(ident) => ident.span(),
        }
    }
}

impl TypeName {
    pub fn parse(tokens: &mut TokenStream) -> ParseResult<Self> {
        match tokens.match_one(Matcher::AnyIdent)? {
            TokenTree::Ident(ident) => Ok(TypeName::Ident(ident)),
            unexpected => {
                let expected = Matcher::AnyIdent;
                Err(TracedError::trace(ParseError::UnexpectedToken(unexpected, Some(expected))))
            },
        }
    }
}

impl fmt::Display for TypeName {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TypeName::Ident(ident) => write!(f, "{}", ident)
        }
    }
}