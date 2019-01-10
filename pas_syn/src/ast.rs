pub mod statement;
pub mod unit;
pub mod expression;

pub use self::{
    statement::*,
    expression::*,
    unit::*,
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
    UnexpectedEOF(Matcher, TokenTree),
}

pub type ParseResult<T> = Result<T, TracedError<ParseError>>;

impl Spanned for ParseError {
    fn span(&self) -> &Span {
        match self {
            ParseError::UnexpectedToken(tt, _) => tt.span(),
            ParseError::UnexpectedEOF(_, tt) => tt.span(),
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
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum TypeName {
    Ident(Ident),
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