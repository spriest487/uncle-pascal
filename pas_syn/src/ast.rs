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
        ident::Ident,
        token_tree::*,
        parse::*,
    },
    pas_common::{
        TracedError,
        span::*,
    },
};

pub trait Annotation : Spanned + Clone + PartialEq + Eq + Hash {
    type Type: fmt::Debug + fmt::Display + Clone + PartialEq + Eq + Hash;
}

impl Annotation for Span {
    type Type = TypeName;
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