use std::{
    fmt,
};
use pas_common::span::{Span, Spanned};
use crate::{
    ast::{Annotation, Expression, TypeName},
    Ident,
    Keyword,
    Operator,
    Separator,
    parse::{
        ParseResult,
        TokenStream,
        Parse,
        LookAheadTokenStream,
        Matcher,
        ParseSeq
    }
};
use derivative::*;
use pas_common::TracedError;
use crate::parse::ParseError;

#[derive(Clone, Eq, Derivative)]
#[derivative(PartialEq, Hash, Debug)]
pub struct ConstDecl<A: Annotation> {
    pub items: Vec<ConstDeclItem<A>>,

    #[derivative(Debug = "ignore")]
    #[derivative(Hash = "ignore")]
    #[derivative(PartialEq = "ignore")]
    pub span: Span,
}

impl<A: Annotation> Spanned for ConstDecl<A> {
    fn span(&self) -> &Span {
        &self.span
    }
}

impl Parse for ConstDecl<Span> {
    fn parse(tokens: &mut TokenStream) -> ParseResult<Self> {
        let kw_token = tokens.match_one(Keyword::Const)?;

        let items = ConstDeclItem::parse_seq(tokens)?;
        let last_item = items.last().ok_or_else(|| {
            TracedError::trace(ParseError::EmptyConstDecl {
                span: kw_token.clone().into_span(),
            })
        })?;

        let span = kw_token.span().to(last_item.span());

        Ok(ConstDecl {
            span,
            items,
        })
    }
}

impl<A: Annotation> fmt::Display for ConstDecl<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "const")?;
        for (i, item) in self.items.iter().enumerate() {
            if i > 0 {
                writeln!(f, ";")?;
            }
            write!(f, "\t{}", item)?;
        }

        Ok(())
    }
}

#[derive(Clone, Eq, Derivative)]
#[derivative(PartialEq, Hash, Debug)]
pub struct ConstDeclItem<A: Annotation> {
    pub ident: Ident,
    pub ty: Option<A::Type>,

    pub val: Box<Expression<A>>,

    #[derivative(Debug = "ignore")]
    #[derivative(Hash = "ignore")]
    #[derivative(PartialEq = "ignore")]
    pub span: Span,
}

impl<A: Annotation> Spanned for ConstDeclItem<A> {
    fn span(&self) -> &Span {
        &self.span
    }
}

impl<A: Annotation> fmt::Display for ConstDeclItem<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.ident)?;
        if let Some(ty) = &self.ty {
            write!(f, ": {}", ty)?;
        }

        write!(f, " = {}", self.val)
    }
}

impl ParseSeq for ConstDeclItem<Span> {
    fn parse_group(prev: &[Self], tokens: &mut TokenStream) -> ParseResult<Self> {
        if !prev.is_empty() {
            tokens.match_one(Separator::Semicolon)?;
        }

        let ident = Ident::parse(tokens)?;

        let explicit_ty = match tokens.match_one_maybe(Separator::Colon) {
            Some(..) => {
                let ty_name = TypeName::parse(tokens)?;
                Some(ty_name)
            }
            None => None,
        };

        tokens.match_one(Operator::Equals)?;

        let val = Expression::parse(tokens)?;

        Ok(ConstDeclItem {
            span: ident.span().to(val.span()),
            ty: explicit_ty,
            val: Box::new(val),
            ident,
        })
    }

    fn has_more(prev: &[Self], tokens: &mut LookAheadTokenStream) -> bool {
        if !prev.is_empty() && tokens.match_one(Separator::Semicolon).is_none() {
            return false;
        }

        tokens.match_one(Matcher::AnyIdent).is_some()
    }
}