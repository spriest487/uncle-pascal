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
    Array {
        element: Box<TypeName>,
        dim: usize,
        span: Span,
    }
}

impl Spanned for TypeName {
    fn span(&self) -> &Span {
        match self {
            TypeName::Ident { ident, .. } => ident.span(),
            TypeName::Array { span, .. } => span,
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

        if let Some(array_kw) = tokens.look_ahead().match_one(Keyword::Array) {
            tokens.advance(1);
            let dim = match tokens.match_one(Matcher::Delimited(DelimiterPair::SquareBracket))? {
                TokenTree::Delimited { inner, open, .. } => {
                    let mut dim_tokens = TokenStream::new(inner, open);
                    let dim = dim_tokens.match_one(Matcher::AnyLiteralInteger)?
                        .as_literal_int()
                        .and_then(|i| i.as_usize())
                        .unwrap();
                    dim_tokens.finish()?;

                    dim
                }
                _ => unreachable!("match failed"),
            };

            tokens.match_one(Keyword::Of)?;

            let element = Self::parse(tokens)?;

            Ok(TypeName::Array {
                dim,
                span: array_kw.span().to(element.span()),
                element: Box::new(element),
            })
        } else {
            let path = tokens.match_repeating(|i, tokens| {
                if i > 0 {
                    if tokens.look_ahead().match_one(Operator::Member).is_none() {
                        return Ok(Generate::Break);
                    } else {
                        tokens.advance(1);
                    }
                }

                let ident_tt = tokens.match_one(Matcher::AnyIdent)?;
                let ident = ident_tt.into_ident().unwrap();
                Ok(Generate::Yield(ident))
            })?;
            assert!(!path.is_empty(), "parsed type path must always have 1+ parts");

            Ok(TypeName::Ident { ident: Path::from_parts(path), indirection })
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

            TypeName::Array { element, dim, .. } => {
                write!(f, "array[{}] of {}", dim, element)
            }

            TypeName::Unknown(_) => write!(f, "<unknown type>"),
        }
    }
}
