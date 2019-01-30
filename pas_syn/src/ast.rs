pub mod statement;
pub mod unit;
pub mod expression;
pub mod call;
pub mod function;
pub mod block;
pub mod typedecl;
pub mod ctor;
pub mod cond;
pub mod iter;

pub use self::{
    statement::*,
    expression::*,
    unit::*,
    call::*,
    block::*,
    function::*,
    typedecl::*,
    ctor::*,
    cond::*,
    iter::*,
};

use {
    std::{
        fmt,
        hash::Hash,
    },
    crate::{
        ident::Ident,
        token_tree::*,
        parse::prelude::*,
    },
    pas_common::{
        TracedError,
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
    Ident { ident: Ident, indirection: usize },
}

impl Spanned for TypeName {
    fn span(&self) -> &Span {
        match self {
            TypeName::Ident { ident, .. } => ident.span(),
        }
    }
}

impl TypeName {
    pub fn parse(tokens: &mut TokenStream) -> ParseResult<Self> {
        let mut indirection = 0;
        while tokens.look_ahead().match_one(Operator::Deref).is_some() {
            indirection += 1;
        }

        match tokens.match_one(Matcher::AnyIdent)? {
            TokenTree::Ident(ident) => Ok(TypeName::Ident { ident, indirection }),
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
            TypeName::Ident { ident, indirection } => {
                for _ in 0..*indirection {
                    write!(f, "^")?;
                }
                write!(f, "{}", ident)
            }
        }
    }
}