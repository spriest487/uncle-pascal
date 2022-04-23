#[cfg(test)]
mod test;

use crate::ast::{Annotation, Expression, Statement};
use crate::parse::{MatchOneOf, Parse, ParseResult, TokenStream};
use crate::{DelimiterPair, Keyword, Separator, TokenTree};
use pas_common::span::{Span, Spanned};
use std::fmt;
use std::hash::{Hash, Hasher};

pub type CaseStatement<A> = CaseBlock<A, Statement<A>>;
pub type CaseExpr<A> = CaseBlock<A, Expression<A>>;

#[derive(Debug, Clone, Eq)]
pub struct CaseBlock<A: Annotation, B> {
    pub cond_expr: Box<Expression<A>>,
    pub branches: Vec<CaseBranch<A, B>>,
    pub else_branch: Option<Box<B>>,

    pub annotation: A,
}

impl<A, B> PartialEq for CaseBlock<A, B>
where
    A: Annotation,
    B: PartialEq
{
    fn eq(&self, other: &Self) -> bool {
        self.branches == other.branches
            && self.cond_expr == other.cond_expr
            && self.else_branch == other.else_branch
    }
}

impl<A, B> Hash for CaseBlock<A, B>
where
    A: Annotation,
    B: Hash
{
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.branches.hash(state);
        self.cond_expr.hash(state);
        self.else_branch.hash(state);
    }
}

impl<A, B> Spanned for CaseBlock<A, B>
where
    A: Annotation,
{
    fn span(&self) -> &Span {
        self.annotation.span()
    }
}

impl<A, B> fmt::Display for CaseBlock<A, B>
where
    A: Annotation,
    B: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "case {} of", self.cond_expr)?;

        for branch in &self.branches {
            writeln!(f, "{};", branch)?;
        }

        if let Some(else_branch) = &self.else_branch {
            writeln!(f, "else {}", else_branch)?;
        }

        writeln!(f, "end")
    }
}

impl<B> CaseBlock<Span, B>
where
    B: Parse + Spanned
{
    pub fn parse(tokens: &mut TokenStream) -> ParseResult<Self> {
        let group = match tokens.match_one(DelimiterPair::CaseEnd)? {
            TokenTree::Delimited(group) => group,
            _ => unreachable!("matcher failed"),
        };

        let case_kw = group.open.clone();
        let end_kw = group.close.clone();
        let mut group_tokens = group.to_inner_tokens();

        let result = Self::parse_group(&mut group_tokens, case_kw, end_kw)?;

        group_tokens.finish()?;

        Ok(result)
    }

    fn parse_group(tokens: &mut TokenStream, case_kw: Span, end_kw: Span) -> ParseResult<Self> {
        let cond_expr = Expression::parse(tokens)?;

        tokens.match_one(Keyword::Of)?;

        let mut branches = Vec::new();

        // is there a valid separator between the last and current statements?
        let mut prev_sep = true;

        let else_branch = loop {
            if tokens.look_ahead().next().is_none() {
                break None;
            }

            if branches.len() > 0 {
                if let Some(..) = tokens.match_one_maybe(Keyword::Else) {
                    let else_item = B::parse(tokens)?;

                    // allow a semicolon separator between the "else" statement and the end keyword
                    tokens.match_one_maybe(Separator::Semicolon);

                    break Some(else_item);
                }
            } else if !prev_sep {
                // this match will fail - there was no separator after the last branch,
                // so we expected the end
                if branches.len() > 0 {
                    tokens.match_one(Keyword::End.or(Keyword::Else))?;
                } else {
                    tokens.match_one(Keyword::End)?;
                };

                unreachable!("previous match will always fail");
            }

            let case_branch = CaseBranch::parse(tokens)?;
            branches.push(case_branch);

            // a semicolon is required to separate branches, but not before the final "end"
            // or "else" keywords. if a statement isn't followed by the separator, it must be the
            // last one, and if not we'll get a parse error
            prev_sep = tokens.match_one_maybe(Separator::Semicolon).is_some();
        };

        let span = case_kw.to(&end_kw);

        Ok(CaseBlock {
            cond_expr: Box::new(cond_expr),
            annotation: span,
            branches,
            else_branch: else_branch.map(Box::new),
        })
    }
}

#[derive(Debug, Clone, Eq)]
pub struct CaseBranch<A: Annotation, Item> {
    pub value: Box<Expression<A>>,
    pub item: Box<Item>,
    pub span: Span,
}

impl<A, Item> PartialEq for CaseBranch<A, Item>
where
    A: Annotation,
    Item: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value && self.item == other.item
    }
}

impl<A, Item> Hash for CaseBranch<A, Item>
where
    A: Annotation,
    Item: Hash,
{
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.value.hash(state);
        self.item.hash(state);
    }
}

impl<A, Item> Spanned for CaseBranch<A, Item>
where
    A: Annotation,
{
    fn span(&self) -> &Span {
        &self.span
    }
}

impl<A, Item> fmt::Display for CaseBranch<A, Item>
where
    A: Annotation,
    Item: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}: {}", self.value, self.item)
    }
}

impl<Item> CaseBranch<Span, Item>
where
    Item: Parse + Spanned,
{
    fn parse(tokens: &mut TokenStream) -> ParseResult<Self> {
        let value = Expression::parse(tokens)?;
        tokens.match_one(Separator::Colon)?;
        let item = Item::parse(tokens)?;

        let span = value.span().to(item.span());

        Ok(CaseBranch {
            value: Box::new(value),
            item: Box::new(item),
            span,
        })
    }
}

impl CaseStatement<Span> {
    pub fn to_expr(&self) -> Option<CaseExpr<Span>> {
        // must have an else branch that is a valid expression
        let else_branch = self.else_branch.as_ref()?.clone().to_expr()?;

        let mut branches = Vec::with_capacity(self.branches.len());
        for branch in &self.branches {
            let item = (*branch.item).clone().to_expr()?;
            branches.push(CaseBranch {
                value: branch.value.clone(),
                span: branch.span.clone(),
                item: Box::new(item),
            })
        }

        Some(CaseExpr {
            cond_expr: self.cond_expr.clone(),
            branches,
            else_branch: Some(Box::new(else_branch)),
            annotation: self.annotation.clone(),
        })
    }
}
