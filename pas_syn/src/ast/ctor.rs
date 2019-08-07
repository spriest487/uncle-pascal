use crate::{ast::Expression, parse::prelude::*};

#[derive(Eq, PartialEq, Clone, Hash, Debug)]
pub struct ObjectCtorMember<A: Annotation> {
    pub ident: Ident,
    pub value: Expression<A>,
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
        let members = members_tokens.match_separated(Separator::Semicolon, |_, tokens| {
            let ident = tokens.match_one(Matcher::AnyIdent)?.into_ident().unwrap();
            tokens.match_one(Separator::Colon)?;
            let value = Expression::parse(tokens)?;

            Ok(Generate::Yield(ObjectCtorMember { ident, value }))
        })?;
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
            write!(f, "{}: {}", member.ident, member.value)?;
        }
        write!(f, ")")
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
    pub elements: Vec<Expression<A>>,
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

        let elements = elems_tokens.match_separated(Separator::Comma, |_, elem_tokens| {
            let elem_expr = Expression::parse(elem_tokens)?;
            Ok(Generate::Yield(elem_expr))
        })?;

        Ok(Self {
            elements,
            annotation: span,
        })
    }
}
