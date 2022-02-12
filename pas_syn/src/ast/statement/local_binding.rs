use crate::{
    ast::{
        Expression, Typed,
    },
    parse::prelude::*,
};

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct LocalBinding<A: Annotation> {
    pub name: Ident,
    pub val_ty: A::Type,
    pub val: Option<Expression<A>>,
    pub mutable: bool,
    pub annotation: A,
}

impl LocalBinding<Span> {
    pub fn parse(tokens: &mut TokenStream, allow_mutable: bool) -> ParseResult<Self> {
        let kw_matcher = if allow_mutable {
            Keyword::Let.or(Keyword::Var)
        } else {
            Keyword::Let.into()
        };

        let let_kw = tokens.match_one(kw_matcher)?;
        let mutable = match let_kw {
            TokenTree::Keyword {
                kw: Keyword::Var, ..
            } => true,
            _ => false,
        };

        let name_token = tokens.match_one(Matcher::AnyIdent)?;

        let val_ty = match tokens.match_one_maybe(Separator::Colon) {
            Some(_) => TypeName::parse(tokens)?,
            None => TypeName::Unknown(name_token.span().clone()),
        };

        let (val, span) = match tokens.match_one_maybe(Operator::Assignment) {
            Some(_) => {
                let val = Expression::parse(tokens)?;
                let span = let_kw.span().to(val.annotation());
                (Some(val), span)
            }
            None => (None, let_kw.span().to(val_ty.span())),
        };

        Ok(LocalBinding {
            name: name_token.as_ident().cloned().unwrap(),
            val_ty,
            mutable,
            val,
            annotation: span,
        })
    }
}

impl<A: Annotation> fmt::Display for LocalBinding<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let kw = if self.mutable { "var" } else { "let" };

        write!(f, "{} {}", kw, self.name)?;
        if self.val_ty.is_known() {
            write!(f, ": {}", self.val_ty)?;
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
