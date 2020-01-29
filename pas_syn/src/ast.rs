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
pub mod type_constraint;

pub use self::{
    block::*, call::*, cond::*, ctor::*, expression::*, function::*, iter::*, op::*, statement::*,
    typedecl::*, unit::*, type_constraint::*,
};

use crate::parse::prelude::*;
use pas_common::TracedError;
use std::{fmt, hash::Hash};

pub trait Typed: fmt::Debug + fmt::Display + Clone + PartialEq + Eq + Hash {
    fn is_known(&self) -> bool;
}

pub trait DeclNamed: fmt::Debug + fmt::Display + Clone + PartialEq + Eq + Hash {
    fn as_local(&self) -> &TypeDeclName;
    fn decl_ty_params(&self) -> &[Ident];
}

pub trait Annotation: Spanned + Clone + PartialEq + Eq + Hash {
    type Type: Typed;
    type DeclName: DeclNamed;
    type Pattern: fmt::Debug + fmt::Display + Clone + PartialEq + Eq + Hash;
}

impl Annotation for Span {
    type Type = TypeName;
    type DeclName = TypeDeclName;
    type Pattern = TypeNamePattern;
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum TypeName {
    /// type is unknown or unnamed at parse time
    Unknown(Span),
    Ident {
        ident: IdentPath,
        type_args: Vec<TypeName>,
        indirection: usize,
        span: Span,
    },
    Array {
        element: Box<TypeName>,
        dim: Option<usize>,
        span: Span,
    },
}

impl Spanned for TypeName {
    fn span(&self) -> &Span {
        match self {
            TypeName::Ident { span, .. } => span,
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
    fn match_next() -> Matcher {
        Keyword::Array.or(Matcher::AnyIdent)
    }

    pub fn parse(tokens: &mut TokenStream) -> ParseResult<Self> {
        let mut indirection = 0;
        let mut indirection_span = None;

        while let Some(deref_tt) = tokens.match_one_maybe(Operator::Deref) {
            if indirection_span.is_none() {
                indirection_span = Some(deref_tt.span().clone());
            }
            indirection += 1;
        }

        tokens.look_ahead().expect_one(Self::match_next())?;

        if let Some(array_kw) = tokens.match_one_maybe(Keyword::Array) {
            // `array of` means the array is dynamic (no dimension)
            let dim = match tokens.look_ahead().match_one(Keyword::Of) {
                Some(_) => None,

                None => match tokens.match_one(Matcher::Delimited(DelimiterPair::SquareBracket))? {
                    TokenTree::Delimited { inner, open, .. } => {
                        let mut dim_tokens = TokenStream::new(inner, open);
                        let dim = dim_tokens
                            .match_one(Matcher::AnyLiteralInteger)?
                            .as_literal_int()
                            .and_then(IntConstant::as_usize)
                            .unwrap();
                        dim_tokens.finish()?;

                        Some(dim)
                    }
                    _ => unreachable!("match failed"),
                },
            };

            tokens.match_one(Keyword::Of)?;

            let element = Self::parse(tokens)?;

            let array_span = array_kw.span().to(element.span());
            let span = match indirection_span {
                Some(indir_span) => indir_span.to(&array_span),
                None => array_span,
            };

            Ok(TypeName::Array {
                dim,
                span,
                element: Box::new(element),
            })
        } else {
            let ident = IdentPath::parse(tokens)?;

            let of_clause = OfClause::parse(tokens, TypeName::parse, Self::match_next())?;

            let (type_args, name_span) = match of_clause {
                None => (Vec::new(), Spanned::span(&ident).clone()),
                Some(of) => (of.items, ident.span().to(&of.span)),
            };

            let span = match indirection_span {
                Some(indir_span) => indir_span.to(&name_span),
                None => name_span,
            };

            Ok(TypeName::Ident {
                ident,
                indirection,
                type_args,
                span,
            })
        }
    }
}

impl fmt::Display for TypeName {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TypeName::Ident {
                ident,
                indirection,
                type_args,
                ..
            } => {
                for _ in 0..*indirection {
                    write!(f, "^")?;
                }
                write!(f, "{}", ident)?;

                if !type_args.is_empty() {
                    write!(f, "<")?;
                    for (i, arg) in type_args.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        write!(f, "{}", arg)?;
                    }
                    write!(f, ">")?;
                }

                Ok(())
            }

            TypeName::Array {
                element,
                dim: Some(dim),
                ..
            } => write!(f, "array[{}] of {}", dim, element),

            TypeName::Array {
                element, dim: None, ..
            } => write!(f, "array of {}", element),

            TypeName::Unknown(_) => write!(f, "<unknown type>"),
        }
    }
}

pub struct OfClause<Item> {
    pub items: Vec<Item>,
    pub span: Span,
}

impl<Item: Spanned> OfClause<Item> {
    fn parse<ItemParser, ItemMatcher>(
        tokens: &mut TokenStream,
        mut item_parser: ItemParser,
        item_next_matcher: ItemMatcher,
    ) -> ParseResult<Option<Self>>
    where
        ItemParser: FnMut(&mut TokenStream) -> ParseResult<Item>,
        ItemMatcher: Into<Matcher>,
    {
        let item_next_matcher = item_next_matcher.into();

        match tokens.match_one_maybe(Keyword::Of) {
            Some(of_kw) => {
                let items = tokens.match_separated(Separator::Comma, |i, tokens| {
                    // expect at least one item after `of`
                    if i > 0
                        && tokens
                            .look_ahead()
                            .match_one(item_next_matcher.clone())
                            .is_none()
                    {
                        Ok(Generate::Break)
                    } else {
                        let item = item_parser(tokens)?;
                        Ok(Generate::Yield(item))
                    }
                })?;

                let span = items
                    .last()
                    .map(|item| of_kw.span().to(item.span()))
                    .expect("must have at least one item following the `of` keyword");

                Ok(Some(OfClause { items, span }))
            }

            None => Ok(None),
        }
    }
}

#[derive( Clone, Debug, Eq, PartialEq, Hash)]
pub enum TypeNamePatternKind {
    Is,
    IsWithBinding(Ident),
    IsNot,
}

impl TypeNamePatternKind {
    pub fn binding(&self) -> Option<&Ident> {
        match self {
            TypeNamePatternKind::IsWithBinding(binding) => Some(binding),
            _ => None,
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum TypeNamePattern {
    TypeName {
        name: IdentPath,
        kind: TypeNamePatternKind,
        span: Span,
    },
    ExactType {
        name: TypeName,
        kind: TypeNamePatternKind,
        span: Span,
    },
}

impl TypeNamePattern {
    pub fn kind(&self) -> &TypeNamePatternKind {
        match self {
            TypeNamePattern::ExactType { kind, .. } => kind,
            TypeNamePattern::TypeName { kind, .. } => kind,
        }
    }

    pub fn parse(tokens: &mut TokenStream) -> ParseResult<Self> {
        let not_kw = tokens.match_one_maybe(Operator::Not);
        let name = TypeName::parse(tokens)?;

        let pattern_path = match &name {
            TypeName::Ident { ident, type_args, indirection, .. } => {
                if ident.as_slice().len() >= 2 && type_args.is_empty() && *indirection == 0 {
                    Some(ident)
                } else {
                    None
                }
            },
            _ => None,
        };

        let binding = if not_kw.is_none() {
            tokens.match_one_maybe(Matcher::AnyIdent).and_then(TokenTree::into_ident)
        } else {
            None
        };

        let span = match (&not_kw, &binding) {
            (Some(not_kw), None) => not_kw.span().to(name.span()),
            (None, Some(binding)) => name.span().to(binding.span()),
            _ => name.span().clone(),
        };

        let kind = match binding {
            Some(binding) => {
                assert!(not_kw.is_none());
                TypeNamePatternKind::IsWithBinding(binding)
            },
            None if not_kw.is_some() => TypeNamePatternKind::IsNot,
            None => TypeNamePatternKind::Is,
        };

        match pattern_path {
            Some(pattern_path) => {
                Ok(TypeNamePattern::TypeName { name: pattern_path.clone(), span, kind })
            }

            None => {
                Ok(TypeNamePattern::ExactType { name, span, kind })
            }
        }
    }
}

impl fmt::Display for TypeNamePattern {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TypeNamePattern::TypeName { name, kind, .. } => {
                if let TypeNamePatternKind::IsNot = kind {
                    write!(f, "not ")?;
                }
                write!(f, "{}", name)?;
                if let TypeNamePatternKind::IsWithBinding(binding) = kind {
                    write!(f, " {}", binding)?;
                }
                Ok(())
            }

            TypeNamePattern::ExactType { name, kind, .. } => {
                if let TypeNamePatternKind::IsNot = kind {
                    write!(f, "not ")?;
                }
                write!(f, "{}", name)?;
                if let TypeNamePatternKind::IsWithBinding(binding) = kind {
                    write!(f, " {}", binding)?;
                }
                Ok(())
            }
        }
    }
}

impl Spanned for TypeNamePattern {
    fn span(&self) -> &Span {
        match self {
            TypeNamePattern::TypeName { span, .. } => span,
            TypeNamePattern::ExactType { span, .. } => span,
        }
    }
}
