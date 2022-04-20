use crate::{
    ast::{match_operand_start, Annotation, Expression},
    parse::{
        LookAheadTokenStream, Matcher, ParseResult, ParseSeq,
        TokenStream,
    },
    DelimiterPair, Ident, IdentPath, TokenTree, Separator,
};
use pas_common::span::{Span, Spanned};
use std::fmt;

#[derive(Eq, PartialEq, Clone, Hash, Debug)]
pub struct ObjectCtorMember<A: Annotation> {
    pub ident: Ident,
    pub value: Expression<A>,
    pub span: Span,
}

impl<A: Annotation> fmt::Display for ObjectCtorMember<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}: {}", self.ident, self.value)
    }
}

impl<A: Annotation> Spanned for ObjectCtorMember<A> {
    fn span(&self) -> &Span {
        &self.span
    }
}

impl ParseSeq for ObjectCtorMember<Span> {
    fn parse_group(prev: &[Self], tokens: &mut TokenStream) -> ParseResult<Self> {
        if !prev.is_empty() {
            tokens.match_one(Separator::Semicolon)?;
        }

        let ident = Ident::parse(tokens)?;
        tokens.match_one(Separator::Colon)?;

        let value = Expression::parse(tokens)?;

        Ok(ObjectCtorMember {
            span: ident.span().to(value.span()),
            ident,
            value,
        })
    }

    fn has_more(prev: &[Self], tokens: &mut LookAheadTokenStream) -> bool {
        if tokens.match_one(Separator::Semicolon).is_none() {
            return false;
        }

        tokens.match_one(Matcher::AnyIdent).is_some()
    }
}

#[derive(Eq, PartialEq, Clone, Hash, Debug)]
pub struct ObjectCtorArgs<A: Annotation> {
    pub open: Span,
    pub members: Vec<ObjectCtorMember<A>>,
    pub close: Span,
}

impl ObjectCtorArgs<Span> {
    pub fn parse(tokens: &mut TokenStream) -> ParseResult<Self> {
        let args_group = tokens.match_one(DelimiterPair::Bracket)?;
        let (open, inner, close) = match args_group {
            TokenTree::Delimited {
                open, inner, close, ..
            } => (open, inner, close),
            _ => unreachable!(),
        };

        let mut members_tokens = TokenStream::new(inner, open.clone());
        let members = ObjectCtorMember::parse_seq(&mut members_tokens)?;

        if members.len() > 0 {
            members_tokens.match_one_maybe(Separator::Semicolon);
        }

        members_tokens.finish()?;

        Ok(ObjectCtorArgs {
            open,
            members,
            close,
        })
    }
}

impl<A: Annotation> ObjectCtorArgs<A> {
    pub fn iter(&self) -> impl Iterator<Item = &ObjectCtorMember<A>> {
        self.members.iter()
    }
}

impl<A: Annotation> fmt::Display for ObjectCtorArgs<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "(")?;
        for (i, member) in self.members.iter().enumerate() {
            if i > 0 {
                write!(f, "; ")?;
            }
            write!(f, "{}", member)?;
        }
        write!(f, ")")
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct CollectionCtorElement<A: Annotation> {
    pub value: Expression<A>,
}

impl<A: Annotation> fmt::Display for CollectionCtorElement<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

impl<A: Annotation> Spanned for CollectionCtorElement<A> {
    fn span(&self) -> &Span {
        self.value.span()
    }
}

impl ParseSeq for CollectionCtorElement<Span> {
    fn parse_group(prev: &[Self], tokens: &mut TokenStream) -> ParseResult<Self> {
        if !prev.is_empty() {
            tokens.match_one(Separator::Comma)?;
        }

        let value = Expression::parse(tokens)?;
        Ok(CollectionCtorElement { value })
    }

    fn has_more(prev: &[Self], tokens: &mut LookAheadTokenStream) -> bool {
        if tokens.match_one(Separator::Comma).is_none() {
            return false;
        }

        tokens.match_one(match_operand_start()).is_some()
    }
}

#[derive(Eq, PartialEq, Clone, Hash, Debug)]
pub struct ObjectCtor<A: Annotation> {
    pub ident: IdentPath,
    pub args: ObjectCtorArgs<A>,
    pub annotation: A,
}

impl<A: Annotation> fmt::Display for ObjectCtor<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}{}", self.ident, self.args)
    }
}

#[derive(Eq, PartialEq, Clone, Hash, Debug)]
pub struct CollectionCtor<A: Annotation> {
    pub elements: Vec<CollectionCtorElement<A>>,
    pub annotation: A,
}

impl<A: Annotation> fmt::Display for CollectionCtor<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[")?;
        for (i, element) in self.elements.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", element)?;
        }
        write!(f, "]")
    }
}

impl CollectionCtor<Span> {
    pub fn parse(tokens: &mut TokenStream) -> ParseResult<Self> {
        let (span, mut elems_tokens) =
            match tokens.match_one(Matcher::Delimited(DelimiterPair::SquareBracket))? {
                TokenTree::Delimited {
                    span, inner, open, ..
                } => (span, TokenStream::new(inner, open)),

                _ => unreachable!(),
            };

        let elements = CollectionCtorElement::parse_seq(&mut elems_tokens)?;
        if elements.len() > 0 {
            elems_tokens.match_one_maybe(Separator::Colon);
        }

        elems_tokens.finish()?;

        Ok(Self {
            elements,
            annotation: span,
        })
    }
}
