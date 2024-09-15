use std::fmt;
use pas_common::span::{Span, Spanned};
use crate::{
    DelimiterPair,
    Keyword,
    Separator,
    TokenTree,
    ast::{Annotation, Expr, Stmt, TypeNamePattern},
    parse::{LookAheadTokenStream, Parse, ParseResult, ParseSeq, TokenStream}
};

#[derive(Clone, Debug, Eq, Hash)]
pub struct MatchBlock<A, B>
where
    A: Annotation
{
    pub cond_expr: Expr<A>,

    pub branches: Vec<MatchBlockBranch<A, B>>,
    pub else_branch: Option<B>,

    pub annotation: A,
}

pub type MatchExpr<A> = MatchBlock<A, Expr<A>>;
pub type MatchStmt<A> = MatchBlock<A, Stmt<A>>;

#[derive(Clone, Debug, Eq, Hash)]
pub struct MatchBlockBranch<A, B>
where
    A: Annotation
{
    pub pattern: A::Pattern,
    pub item: B,
    pub span: Span,
}

impl<B> ParseSeq for MatchBlockBranch<Span, B>
where
    B: Parse + Spanned
{
    fn parse_group(prev: &[Self], tokens: &mut TokenStream) -> ParseResult<Self> {
        if !prev.is_empty() {
            tokens.match_one(Separator::Semicolon)?;
        }

        let pattern = TypeNamePattern::parse(tokens)?;

        tokens.match_one(Separator::Colon)?;

        let item = B::parse(tokens)?;

        Ok(MatchBlockBranch {
            span: pattern.span().to(item.span()),
            pattern,
            item,
        })
    }

    fn has_more(prev: &[Self], tokens: &mut LookAheadTokenStream) -> bool {
        if !prev.is_empty() && tokens.match_one(Separator::Semicolon).is_none() {
            return false;
        }

        match tokens.next() {
            Some(tt) if !tt.is_keyword(Keyword::Else) => true,
            _ => false,
        }
    }
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
        let block_group = match tokens.match_one(DelimiterPair::MatchEnd)? {
            TokenTree::Delimited(group) => group,
            _ => unreachable!(),
        };

        let kw_span = block_group.open.clone();
        let end_span = block_group.close.clone();

        let mut inner_tokens = block_group.to_inner_tokens();

        let cond_expr = Expr::parse(&mut inner_tokens)?;
        inner_tokens.match_one(Keyword::Of)?;

        let branches = MatchBlockBranch::parse_seq(&mut inner_tokens)?;
        if !branches.is_empty() {
            inner_tokens.match_one_maybe(Separator::Semicolon);
        }

        let else_branch = match inner_tokens.match_one_maybe(Keyword::Else) {
            Some(_else_tt) => Some(B::parse(&mut inner_tokens)?),
            None => None,
        };

        if else_branch.is_some() {
            inner_tokens.match_one_maybe(Separator::Semicolon);
        }
        inner_tokens.finish()?;

        Ok(MatchBlock {
            cond_expr,
            branches,
            else_branch,
            annotation: kw_span.to(&end_span),
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