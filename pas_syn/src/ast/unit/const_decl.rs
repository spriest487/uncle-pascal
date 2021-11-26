use std::fmt;
use std::hash::{Hash, Hasher};
use pas_common::span::{Span, Spanned};
use crate::ast::{Annotation, Expression, TypeName};
use crate::{Ident, Keyword, Operator, Separator};
use crate::parse::{ParseResult, TokenStream};

#[derive(Clone, Debug, Eq)]
pub struct ConstDecl<A: Annotation> {
    pub ident: Ident,
    pub ty: A::Type,

    pub val: Box<Expression<A>>,

    pub span: Span,
}

impl<A: Annotation> Spanned for ConstDecl<A> {
    fn span(&self) -> &Span {
        &self.span
    }
}

impl<A: Annotation> PartialEq for ConstDecl<A> {
    fn eq(&self, other: &Self) -> bool {
        self.ident == other.ident && self.ty == other.ty
    }
}

impl<A: Annotation> Hash for ConstDecl<A> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.ident.hash(state);
        self.ty.hash(state);
    }
}

impl<A: Annotation> fmt::Display for ConstDecl<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "const {}: {} = {}", self.ident, self.ty, self.val)
    }
}

impl ConstDecl<Span> {
    pub fn parse(tokens: &mut TokenStream) -> ParseResult<Self> {
        let kw_token = tokens.match_one(Keyword::Const)?;
        let ident = Ident::parse(tokens)?;
        tokens.match_one(Separator::Colon)?;
        let ty_name = TypeName::parse(tokens)?;
        tokens.match_one(Operator::Equals)?;

        let val = Expression::parse(tokens)?;

        Ok(ConstDecl {
            span: kw_token.span().to(val.span()),
            ty: ty_name,
            val: Box::new(val),
            ident,
        })
    }
}