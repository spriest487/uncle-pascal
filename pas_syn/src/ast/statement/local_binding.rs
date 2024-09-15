use std::fmt;
use pas_common::span::{Span, Spanned};
use crate::{
    ast::Annotation,
    ast::{
        Expr, TypeAnnotation,
    },
    parse::{ParseResult, TokenStream},
    Ident,
    Keyword,
    Operator,
    Separator
};
use crate::ast::type_name::TypeName;
use crate::parse::Parse;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct LocalBinding<A: Annotation> {
    pub name: Ident,
    pub ty: A::Type,
    pub val: Option<Expr<A>>,
    pub annotation: A,
}

impl LocalBinding<Span> {
    pub fn parse(tokens: &mut TokenStream) -> ParseResult<Self> {
        let var_kw_tt = tokens.match_one(Keyword::Var)?;
        let name = Ident::parse(tokens)?;

        let ty = match tokens.match_one_maybe(Separator::Colon) {
            Some(_) => TypeName::parse(tokens)?,
            None => TypeName::Unknown(name.span().clone()),
        };

        let (val, span) = match tokens.match_one_maybe(Operator::Assignment) {
            Some(_) => {
                let val = Expr::parse(tokens)?;
                let span = var_kw_tt.span().to(val.annotation());
                (Some(val), span)
            }
            None => (None, var_kw_tt.span().to(ty.span())),
        };

        Ok(LocalBinding {
            name,
            ty,
            val,
            annotation: span,
        })
    }
}

impl<A: Annotation> fmt::Display for LocalBinding<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "var {}", self.name)?;
        if self.ty.is_known() {
            write!(f, ": {}", self.ty)?;
        }
        if let Some(val) = &self.val {
            write!(f, " := {}", val)?;
        }
        Ok(())
    }
}

impl<A: Annotation> Spanned for LocalBinding<A> {
    fn span(&self) -> &Span {
        self.annotation.span()
    }
}
