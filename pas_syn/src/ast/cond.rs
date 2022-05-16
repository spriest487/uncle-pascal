#[cfg(test)]
mod test;

use crate::{
    ast::{Expr, Stmt},
    parse::prelude::*,
};

#[derive(Eq, PartialEq, Hash, Debug, Clone)]
pub struct IfCond<A, B>
where
    A: Annotation,
{
    pub cond: Expr<A>,

    pub is_pattern: Option<A::Pattern>,

    pub then_branch: B,
    pub else_branch: Option<B>,
    pub annotation: A,
}

impl<B> IfCond<Span, B>
where
    B: Parse + Spanned,
{
    pub fn parse(tokens: &mut TokenStream) -> ParseResult<Self> {
        let if_token = tokens.match_one(Keyword::If)?;
        let cond = Expr::parse(tokens)?;

        let is_pattern = match tokens.match_one_maybe(Keyword::Is) {
            Some(_is_kw) => {
                let pattern = TypeNamePattern::parse(tokens)?;
                Some(pattern)
            },

            None => None,
        };

        tokens.match_one(Keyword::Then)?;
        let then_branch = B::parse(tokens)?;

        let (else_branch, span) = match tokens.match_one_maybe(Keyword::Else) {
            Some(_else_token) => {
                let else_branch = B::parse(tokens)?;
                let span = if_token.span().to(else_branch.span());

                (Some(else_branch), span)
            },

            None => (None, if_token.span().to(then_branch.span())),
        };

        Ok(IfCond {
            cond,
            is_pattern,
            then_branch,
            else_branch,
            annotation: span,
        })
    }
}

impl<A, B> fmt::Display for IfCond<A, B>
where
    A: Annotation,
    B: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "if {} ", self.cond)?;

        if let Some(is_pattern) = &self.is_pattern {
            write!(f, "is {}", is_pattern)?;
        }

        write!(f, " then {}", self.then_branch)?;

        if let Some(else_branch) = &self.else_branch {
            write!(f, " else {}", else_branch)
        } else {
            Ok(())
        }
    }
}

impl<A, B> Spanned for IfCond<A, B>
where
    A: Annotation,
    B: Parse,
{
    fn span(&self) -> &Span {
        self.annotation.span()
    }
}

impl IfCond<Span, Stmt<Span>> {
    pub fn to_expr(&self) -> Option<IfCond<Span, Expr<Span>>> {
        let then_branch = self.then_branch.to_expr()?;
        let else_branch = match &self.else_branch {
            Some(else_stmt) => Some(else_stmt.to_expr()?),
            None => None,
        };

        Some(IfCond {
            cond: self.cond.clone(),
            annotation: self.annotation.clone(),
            is_pattern: self.is_pattern.clone(),
            then_branch,
            else_branch,
        })
    }
}
