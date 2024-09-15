use crate::{
    ast::{Annotation, TypeDeclName},
    parse::Parse,
    parse::{LookAheadTokenStream, Matcher, ParseResult, ParseSeq, TokenStream},
    Ident, Keyword, Separator,
};
use derivative::*;
use pas_common::span::{Span, Spanned};
use std::fmt;
use crate::ast::type_name::TypeName;

#[derive(Clone, Eq, Derivative)]
#[derivative(Debug, PartialEq, Hash)]
pub struct VariantDef<A: Annotation> {
    pub name: A::Name,
    pub cases: Vec<VariantCase<A>>,

    #[derivative(Debug = "ignore")]
    #[derivative(PartialEq = "ignore")]
    #[derivative(Hash = "ignore")]
    pub span: Span,
}

#[derive(Clone, Eq, Derivative)]
#[derivative(Debug, PartialEq, Hash)]
pub struct VariantCase<A: Annotation> {
    pub ident: Ident,
    pub data_ty: Option<A::Type>,

    #[derivative(Debug = "ignore")]
    #[derivative(PartialEq = "ignore")]
    #[derivative(Hash = "ignore")]
    pub span: Span,
}

impl ParseSeq for VariantCase<Span> {
    fn parse_group(prev: &[Self], tokens: &mut TokenStream) -> ParseResult<Self> {
        if !prev.is_empty() {
            tokens.match_one(Separator::Semicolon)?;
        }

        let ident = Ident::parse(tokens)?;

        let case = match tokens.match_one_maybe(Separator::Colon) {
            Some(..) => {
                let ty = TypeName::parse(tokens)?;
                let span = ident.span().to(ty.span());

                VariantCase {
                    span,
                    ident,
                    data_ty: Some(ty),
                }
            },

            None => VariantCase {
                span: ident.span.clone(),
                ident,
                data_ty: None,
            },
        };

        Ok(case)
    }

    fn has_more(prev: &[Self], tokens: &mut LookAheadTokenStream) -> bool {
        if !prev.is_empty() && tokens.match_one(Separator::Semicolon).is_none() {
            return false;
        }

        tokens.match_one(Matcher::AnyIdent).is_some()
    }
}

impl<A: Annotation> VariantDef<A> {
    pub fn case_position(&self, case_ident: &Ident) -> Option<usize> {
        self.cases.iter().position(|c| c.ident == *case_ident)
    }
}

impl VariantDef<Span> {
    pub fn parse(tokens: &mut TokenStream, name: TypeDeclName) -> ParseResult<Self> {
        let kw = tokens.match_one(Keyword::Variant)?;

        let cases = VariantCase::parse_seq(tokens)?;
        tokens.match_one_maybe(Separator::Semicolon);

        let end_kw = tokens.match_one(Keyword::End)?;

        let span = kw.span().to(end_kw.span());

        Ok(VariantDef { name, cases, span })
    }
}

impl<A: Annotation> fmt::Display for VariantDef<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "variant {}", self.name)?;
        for case in &self.cases {
            write!(f, "  {}", case.ident)?;
            if let Some(data_ty) = &case.data_ty {
                write!(f, ": {}", data_ty)?;
            }
            writeln!(f, ";")?;
        }
        write!(f, "end")
    }
}

impl<A: Annotation> Spanned for VariantDef<A> {
    fn span(&self) -> &Span {
        &self.span
    }
}
