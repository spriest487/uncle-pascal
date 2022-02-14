use std::fmt;
use pas_common::span::{Span, Spanned};
use crate::ast::{Annotation, Expression, Statement, TypeNamePattern};
use crate::{DelimiterPair, Keyword, Separator, TokenTree};
use crate::parse::{Generate, Parse, ParseResult, TokenStream};

#[derive(Clone, Debug, Eq, Hash)]
pub struct MatchBlock<A, B>
where
    A: Annotation
{
    pub cond_expr: Expression<A>,

    pub branches: Vec<MatchBlockBranch<A, B>>,
    pub else_branch: Option<B>,

    pub annotation: A,
}

pub type MatchExpr<A> = MatchBlock<A, Expression<A>>;
pub type MatchStmt<A> = MatchBlock<A, Statement<A>>;

#[derive(Clone, Debug, Eq, Hash)]
pub struct MatchBlockBranch<A, B>
where
    A: Annotation
{
    pub pattern: A::Pattern,
    pub item: B,
    pub span: Span,
}

impl<A, B> PartialEq for MatchBlock<A, B>
where
    A: Annotation,
    B: PartialEq
{
    fn eq(&self, other: &Self) -> bool {
        self.branches == other.branches
            && self.else_branch == other.else_branch
            && self.cond_expr == other.cond_expr
    }
}

impl<A, B> PartialEq for MatchBlockBranch<A, B>
where
    A: Annotation,
    B: PartialEq
{
    fn eq(&self, other: &Self) -> bool {
        self.item == other.item && self.pattern == other.pattern
    }
}

impl<B> Parse for MatchBlock<Span, B>
where
    B: Parse + Spanned
{
    fn parse(tokens: &mut TokenStream) -> ParseResult<Self> {
        let (inner_tts, kw_tt, end_tt) = match tokens.match_one(DelimiterPair::MatchEnd)? {
            TokenTree::Delimited { inner, open, close, .. } => (inner, open, close),
            _ => unreachable!(),
        };

        let mut inner_tokens = TokenStream::new(inner_tts, kw_tt.span().clone());

        let cond_expr = Expression::parse(&mut inner_tokens)?;
        inner_tokens.match_one(Keyword::Of)?;

        let branches = inner_tokens.match_separated(Separator::Semicolon, |_i, branch_tokens| {
            if branch_tokens.look_ahead().match_one(Keyword::Else).is_some() {
                return Ok(Generate::Break);
            }

            let pattern = TypeNamePattern::parse(branch_tokens)?;

            branch_tokens.match_one(Separator::Colon)?;

            let item = B::parse(branch_tokens)?;

            Ok(Generate::Yield(MatchBlockBranch {
                span: pattern.span().to(item.span()),
                pattern,
                item,
            }))
        })?;

        let else_branch = match inner_tokens.match_one_maybe(Keyword::Else) {
            Some(_else_tt) => Some(B::parse(&mut inner_tokens)?),
            None => None,
        };

        inner_tokens.finish()?;

        Ok(MatchBlock {
            cond_expr,
            branches,
            else_branch,
            annotation: kw_tt.to(&end_tt),
        })
    }
}

impl<A, B> fmt::Display for MatchBlock<A, B>
where
    A: Annotation,
    B: fmt::Display
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "match {} of", self.cond_expr)?;

        for branch in &self.branches {
            writeln!(f, "\t{}: {};", branch.pattern, branch.item)?;
        }

        if let Some(else_branch) = &self.else_branch {
            writeln!(f, "\telse {};", else_branch)?;
        }

        write!(f, "end")
    }
}

impl MatchStmt<Span> {
    pub fn to_expr(&self) -> Option<MatchExpr<Span>> {
        let mut branches = Vec::new();
        for branch in &self.branches {
            let item = branch.item.to_expr()?;
            branches.push(MatchBlockBranch {
                item,
                pattern: branch.pattern.clone(),
                span: branch.span.clone(),
            })
        }

        let else_branch = match self.else_branch.as_ref() {
            Some(else_stmt) => Some(else_stmt.to_expr()?),
            None => None,
        };

        Some(MatchExpr {
            cond_expr: self.cond_expr.clone(),
            else_branch,
            branches,
            annotation: self.annotation.clone(),
        })
    }
}

impl<A, B> Spanned for MatchBlock<A, B>
where
    A: Annotation
{
    fn span(&self) -> &Span {
        self.annotation.span()
    }
}

impl<A, B> Spanned for MatchBlockBranch<A, B>
where
    A: Annotation
{
    fn span(&self) -> &Span {
        &self.span
    }
}