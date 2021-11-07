use std::fmt;
use std::hash::{Hash, Hasher};
use crate::ast::prelude::*;

#[derive(Eq, Clone, Debug)]
pub struct SizeOf<A: Annotation> {
    pub ty: A::Type,
    pub annotation: A,
}

impl<A: Annotation> PartialEq for SizeOf<A> {
    fn eq(&self, other: &Self) -> bool {
        self.ty.eq(&other.ty)
    }
}

impl<A: Annotation> Hash for SizeOf<A> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.ty.hash(state)
    }
}

impl<A: Annotation> Spanned for SizeOf<A> {
    fn span(&self) -> &Span {
        &self.annotation.span()
    }
}

impl<A: Annotation> fmt::Display for SizeOf<A> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}({})", Keyword::SizeOf, self.ty)
    }
}

impl SizeOf<Span> {
    pub fn parse(tokens: &mut TokenStream) -> ParseResult<Self> {
        let kw = tokens.match_one(Keyword::SizeOf)?;

        let (ty_tokens, close_bracket) = match tokens.match_one(DelimiterPair::Bracket)? {
            TokenTree::Delimited { inner, close, .. } =>  (inner, close),
            _ => unreachable!(),
        };

        let mut ty_token_stream = TokenStream::new(ty_tokens, kw.span().clone());
        let ty = TypeName::parse(&mut ty_token_stream)?;
        ty_token_stream.finish()?;

        let span = kw.span().to(close_bracket.span());

        Ok(Self {
            annotation: span,
            ty
        })
    }
}