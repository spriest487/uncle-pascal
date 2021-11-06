use std::fmt;
use crate::{parse::prelude::*, ast::{Typed}};

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct TypeConstraint<T: Typed> {
    pub param_ident: Ident,
    pub is_ty: T,
    pub span: Span,
}

impl<T: Typed> fmt::Display for TypeConstraint<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} is {}", self.param_ident, self.is_ty)
    }
}

impl<T: Typed> Spanned for TypeConstraint<T> {
    fn span(&self) -> &Span {
        &self.span
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct WhereClause<T: Typed> {
    pub constraints: Vec<TypeConstraint<T>>,
    pub span: Span,
}

impl<T: Typed> fmt::Display for WhereClause<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "where ")?;
        for (i, constraint) in self.constraints.iter().enumerate() {
            if i > 0 {
                write!(f, "; ")?;
            }
            write!(f, "{}", constraint)?;
        }
        Ok(())
    }
}

impl<T: Typed> Spanned for WhereClause<T> {
    fn span(&self) -> &Span {
        &self.span
    }
}

impl WhereClause<TypeName> {
    pub fn parse(tokens: &mut TokenStream) -> ParseResult<Self> {
        let where_kw = tokens.match_one(Keyword::Where)?;

        let constraints = tokens.match_separated(Separator::Semicolon, |_, tokens| {
            if !tokens.look_ahead().match_one(Matcher::AnyIdent).is_some() {
                return Ok(Generate::Break);
            }

            let param = tokens.match_one(Matcher::AnyIdent)?.into_ident().unwrap();
            tokens.match_one(Keyword::Is)?;
            let is_ty_path = IdentPath::parse(tokens)?;

            let is_ty = TypeName::Ident {
                span: is_ty_path.span().clone(),
                type_args: None,
                ident: is_ty_path,
                indirection: 0,
            };

            Ok(Generate::Yield(TypeConstraint {
                span: param.span().to(is_ty.span()),
                is_ty,
                param_ident: param,
            }))
        })?;

        let span = constraints.iter()
            .fold(where_kw.span().clone(), |span, constraint| {
                span.to(constraint.span())
            });

        let clause = Self {
            constraints,
            span,
        };

        if clause.constraints.is_empty() {
            return Err(TracedError::trace(ParseError::EmptyWhereClause(clause)))
        } else {
            Ok(clause)
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct TypeParam<T: Typed> {
    pub ident: Ident,
    pub constraint: Option<TypeConstraint<T>>,
}