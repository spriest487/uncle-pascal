use crate::parse::ParseResult;
use crate::{
    ast::{Annotation, TypeDeclName, TypeName},
    parse::TokenStream,
};
use derivative::Derivative;
use pas_common::span::{Span, Spanned};
use std::fmt;

#[derive(Clone, Eq, Derivative)]
#[derivative(PartialEq, Hash, Debug)]
pub struct AliasDecl<A>
where
    A: Annotation,
{
    pub name: A::Name,
    pub ty: Box<A::Type>,

    #[derivative(Debug = "ignore")]
    #[derivative(PartialEq = "ignore")]
    #[derivative(Hash = "ignore")]
    pub span: Span,
}

impl AliasDecl<Span> {
    pub fn parse(tokens: &mut TokenStream, name: TypeDeclName) -> ParseResult<Self> {
        let ty = TypeName::parse(tokens)?;
        let span = name.span().to(ty.span());

        Ok(Self {
            name,
            span,
            ty: Box::new(ty),
        })
    }
}

impl<A> Spanned for AliasDecl<A>
where
    A: Annotation,
{
    fn span(&self) -> &Span {
        &self.span
    }
}

impl<A> fmt::Display for AliasDecl<A> where A: Annotation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.ty)
    }
}
