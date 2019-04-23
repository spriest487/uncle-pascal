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
pub mod op;

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
    op::*,
};

use {
    crate::{
        ident::Ident,
        token_tree::*,
        parse::prelude::*,
    },
    std::{
        fmt,
        hash::Hash,
    },
    pas_common::{
        TracedError,
    },
};

pub trait Typed: fmt::Debug + fmt::Display + Clone + PartialEq + Eq + Hash {
    fn is_known(&self) -> bool;
}

pub trait Annotation : Spanned + Clone + PartialEq + Eq + Hash {
    type Type: Typed;
}

impl Annotation for Span {
    type Type = TypeName;
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum TypeName {
    /// type is unknown or unnamed at parse time
    Unknown(Span),
    Ident { ident: Ident, indirection: usize },
}

impl Spanned for TypeName {
    fn span(&self) -> &Span {
        match self {
            TypeName::Ident { ident, .. } => ident.span(),
            TypeName::Unknown(span) => span,
        }
    }
}

impl Typed for TypeName {
    fn is_known(&self) -> bool {
        match self {
            TypeName::Unknown(_) => false,
            _ => true,
        }
    }
}

impl TypeName {
    pub fn parse(tokens: &mut TokenStream) -> ParseResult<Self> {
        let mut indirection = 0;
        while tokens.look_ahead().match_one(Operator::Deref).is_some() {
            indirection += 1;
            tokens.advance(1);
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

            TypeName::Unknown(_) => {
                write!(f, "<unknown type>")
            }
        }
    }
}