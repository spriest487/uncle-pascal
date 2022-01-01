use std::fmt;
use std::hash::{Hash, Hasher};
use pas_common::span::{Span, Spanned};
use crate::ast::{Annotation, Expression, Statement};
use crate::parse::{MatchOneOf, ParseResult, TokenStream};
use crate::{Keyword, Separator, TokenTree};

#[derive(Debug, Clone, Eq)]
pub struct Case<A: Annotation> {
    pub cond_expr: Box<Expression<A>>,
    pub branches: Vec<CaseBranch<A>>,
    pub else_branch: Option<Box<Statement<A>>>,

    pub annotation: A,
}

impl<A: Annotation> PartialEq for Case<A> {
    fn eq(&self, other: &Self) -> bool {
        self.branches == other.branches
            && self.cond_expr == other.cond_expr
            && self.else_branch == other.else_branch
    }
}

impl<A: Annotation> Hash for Case<A> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.branches.hash(state);
        self.cond_expr.hash(state);
        self.else_branch.hash(state);
    }
}

impl<A: Annotation> Spanned for Case<A> {
    fn span(&self) -> &Span {
        self.annotation.span()
    }
}

impl<A: Annotation> fmt::Display for Case<A> {
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

impl Case<Span> {
    pub fn parse(tokens: &mut TokenStream) -> ParseResult<Self> {
        let case_kw = tokens.match_one(Keyword::Case)?;

        let cond_expr = Expression::parse(tokens)?;

        tokens.match_one(Keyword::Of)?;

        let sep_matcher = Separator::Semicolon
            .or(Keyword::End)
            .or(Keyword::Else);

        let mut branches = Vec::new();

        let (end_kw, else_branch) = loop {
            let case_branch = CaseBranch::parse(tokens)?;
            branches.push(case_branch);

            match tokens.match_one(sep_matcher.clone())? {
                end_kw @ TokenTree::Keyword { kw: Keyword::End, .. } => {
                    break (end_kw, None);
                }

                TokenTree::Keyword { kw: Keyword::Else, .. } => {
                    let else_stmt = Statement::parse(tokens)?;

                    let end_kw = tokens.match_one(Keyword::End)?;
                    break (end_kw, Some(else_stmt));
                }

                TokenTree::Separator { sep: Separator::Semicolon, .. } => {
                    // continue reading case branches
                }

                _ => unreachable!(),
            }
        };

        let span = case_kw.span().to(end_kw.span());

        Ok(Case {
            cond_expr: Box::new(cond_expr),
            annotation: span,
            branches,
            else_branch: else_branch.map(Box::new),
        })
    }
}

#[derive(Debug, Clone, Eq)]
pub struct CaseBranch<A: Annotation> {
    pub value: Box<Expression<A>>,
    pub stmt: Box<Statement<A>>,
    pub span: Span,
}

impl<A: Annotation> PartialEq for CaseBranch<A> {
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value && self.stmt == other.stmt
    }
}

impl<A: Annotation> Hash for CaseBranch<A> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.value.hash(state);
        self.stmt.hash(state);
    }
}

impl<A: Annotation> Spanned for CaseBranch<A> {
    fn span(&self) -> &Span {
        &self.span
    }
}

impl<A: Annotation> fmt::Display for CaseBranch<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}: {}", self.value, self.stmt)
    }
}

impl CaseBranch<Span> {
    pub fn parse(tokens: &mut TokenStream) -> ParseResult<Self> {
        let value = Expression::parse(tokens)?;
        tokens.match_one(Separator::Colon)?;
        let stmt = Statement::parse(tokens)?;

        let span = value.span().to(stmt.annotation());

        Ok(CaseBranch {
            value: Box::new(value),
            stmt: Box::new(stmt),
            span,
        })
    }
}