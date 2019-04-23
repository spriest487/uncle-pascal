pub mod block;
pub mod call;
pub mod cond;
pub mod ctor;
pub mod expression;
pub mod function;
pub mod iter;
pub mod op;
pub mod statement;
pub mod typedecl;
pub mod unit;

pub use self::{
    block::*,
    call::*,
    cond::*,
    ctor::*,
    expression::*,
    function::*,
    iter::*,
    op::*,
    statement::*,
    typedecl::*,
    unit::*,
};

use crate::{
    parse::prelude::*,
    token_tree::*,
};
use pas_common::TracedError;
use std::{
    fmt,
    hash::Hash,
};

pub trait Typed: fmt::Debug + fmt::Display + Clone + PartialEq + Eq + Hash {
    fn is_known(&self) -> bool;
}

pub trait Annotation: Spanned + Clone + PartialEq + Eq + Hash {
    type Type: Typed;
}

impl Annotation for Span {
    type Type = TypeName;
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum TypeName {
    /// type is unknown or unnamed at parse time
    Unknown(Span),
    Ident {
        ident: IdentPath,
        indirection: usize,
    },
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
            TokenTree::Ident(ident) => {
                // todo: multi-part paths in typenames
                let ident = Path::from(ident);
                Ok(TypeName::Ident { ident, indirection })
            },
            unexpected => {
                let expected = Matcher::AnyIdent;
                Err(TracedError::trace(ParseError::UnexpectedToken(
                    unexpected,
                    Some(expected),
                )))
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
            },

            TypeName::Unknown(_) => write!(f, "<unknown type>"),
        }
    }
}
