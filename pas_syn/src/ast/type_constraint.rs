use crate::{
    ast::TypeAnnotation,
    parse::{LookAheadTokenStream, Matcher, Parse, ParseError, ParseResult, ParseSeq, TokenStream},
    Ident, IdentPath, Keyword, Separator,
};
use pas_common::{
    span::{Span, Spanned},
    TracedError,
};
use std::fmt;
use crate::ast::type_name::{IdentTypeName, TypeName};

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct TypeConstraint<T: TypeAnnotation> {
    pub param_ident: Ident,
    pub is_ty: T,
    pub span: Span,
}

impl<T: TypeAnnotation> fmt::Display for TypeConstraint<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} is {}", self.param_ident, self.is_ty)
    }
}

impl<T: TypeAnnotation> Spanned for TypeConstraint<T> {
    fn span(&self) -> &Span {
        &self.span
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct WhereClause<T: TypeAnnotation> {
    pub constraints: Vec<TypeConstraint<T>>,
    pub span: Span,
}

impl<T: TypeAnnotation> fmt::Display for WhereClause<T> {
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

impl<T: TypeAnnotation> Spanned for WhereClause<T> {
    fn span(&self) -> &Span {
        &self.span
    }
}

impl WhereClause<TypeName> {
    pub fn parse(tokens: &mut TokenStream) -> ParseResult<Self> {
        let where_kw = tokens.match_one(Keyword::Where)?;

        let constraints: Vec<_> = WhereClauseItem::parse_seq(tokens)?
            .into_iter()
            .map(|i| i.0)
            .collect();

        let span = constraints
            .iter()
            .fold(where_kw.span().clone(), |span, constraint| {
                span.to(constraint.span())
            });

        let clause = Self { constraints, span };

        if clause.constraints.is_empty() {
            return Err(TracedError::trace(ParseError::EmptyWhereClause(clause)));
        } else {
            Ok(clause)
        }
    }
}

struct WhereClauseItem(TypeConstraint<TypeName>);

impl ParseSeq for WhereClauseItem {
    fn parse_group(prev: &[Self], tokens: &mut TokenStream) -> ParseResult<Self> {
        if prev.len() > 0 {
            tokens.match_one(Separator::Semicolon)?;
        }

        let param_ident = Ident::parse(tokens)?;
        tokens.match_one(Keyword::Is)?;

        let is_ty_path = IdentPath::parse(tokens)?;

        let is_ty = TypeName::Ident(IdentTypeName {
            span: is_ty_path.span().clone(),
            type_args: None,
            ident: is_ty_path,
            indirection: 0,
        });

        Ok(WhereClauseItem(TypeConstraint {
            span: param_ident.span().to(is_ty.span()),
            param_ident,
            is_ty,
        }))
    }

    fn has_more(prev: &[Self], tokens: &mut LookAheadTokenStream) -> bool {
        if !prev.is_empty() && tokens.match_one(Separator::Semicolon).is_none() {
            return false;
        }

        tokens.match_one(Matcher::AnyIdent).is_some()
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct TypeParam<T: TypeAnnotation> {
    pub name: Ident,
    pub constraint: Option<TypeConstraint<T>>,
}

impl<T: TypeAnnotation> fmt::Display for TypeParam<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.name)?;
        if let Some(constraint) = &self.constraint {
            write!(f, ": {}", constraint.is_ty)?;
        }

        Ok(())
    }
}
