use crate::ast::{Annotation, Expression, Statement};
use crate::parse::{MatchOneOf, ParseResult, TokenStream};
use crate::{Keyword, Separator};
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
    B: CaseItemParse
{
    pub fn parse(tokens: &mut TokenStream) -> ParseResult<Self> {
        let case_kw = tokens.match_one(Keyword::Case)?;

        let cond_expr = Expression::parse(tokens)?;

        tokens.match_one(Keyword::Of)?;

        let mut branches = Vec::new();

        // is there a valid separator between the last and current statements?
        let mut prev_sep = true;

        let (end_tt, else_branch) = loop {
            if let Some(end_tt) = tokens.match_one_maybe(Keyword::End) {
                break (end_tt, None);
            } else if branches.len() > 0 {
                if let Some(..) = tokens.match_one_maybe(Keyword::Else) {
                    let else_stmt = B::parse(tokens)?;

                    // allow a semicolon separator between the "else" statement and the end keyword
                    tokens.match_one_maybe(Separator::Semicolon);

                    let end_tt = tokens.match_one(Keyword::End)?;
                    break (end_tt, Some(else_stmt));
                }
            } else if !prev_sep {
                // just let this match fail - there was no separator after the last branch,
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

        let span = case_kw.span().to(end_tt.span());

        Ok(CaseBlock {
            cond_expr: Box::new(cond_expr),
            annotation: span,
            branches,
            else_branch: else_branch.map(Box::new),
        })
    }
}

pub trait CaseItemParse: Sized + Spanned {
    fn parse(tokens: &mut TokenStream) -> ParseResult<Self>;
}

impl CaseItemParse for Expression<Span> {
    fn parse(tokens: &mut TokenStream) -> ParseResult<Self> {
        Expression::parse(tokens)
    }
}

impl CaseItemParse for Statement<Span> {
    fn parse(tokens: &mut TokenStream) -> ParseResult<Self> {
        Statement::parse(tokens)
    }
}

#[derive(Debug, Clone, Eq)]
pub struct CaseBranch<A: Annotation, B> {
    pub value: Box<Expression<A>>,
    pub item: Box<B>,
    pub span: Span,
}

impl<A, B> PartialEq for CaseBranch<A, B>
where
    A: Annotation,
    B: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value && self.item == other.item
    }
}

impl<A, B> Hash for CaseBranch<A, B>
where
    A: Annotation,
    B: Hash,
{
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.value.hash(state);
        self.item.hash(state);
    }
}

impl<A, B> Spanned for CaseBranch<A, B>
where
    A: Annotation,
{
    fn span(&self) -> &Span {
        &self.span
    }
}

impl<A, B> fmt::Display for CaseBranch<A, B>
where
    A: Annotation,
    B: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}: {}", self.value, self.item)
    }
}

impl<Item> CaseBranch<Span, Item>
where
    Item: CaseItemParse,
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

#[cfg(test)]
mod test {
    use super::*;
    use crate::TokenTree;
    use pas_common::BuildOptions;

    fn parse_case<Item>(s: &str) -> CaseBlock<Span, Item>
        where
            Item: CaseItemParse
    {
        let src_tokens = TokenTree::tokenize("test", s, &BuildOptions::default()).unwrap();
        let mut token_stream = TokenStream::new(src_tokens, Span::zero("test"));

        match Statement::parse(&mut token_stream).unwrap() {
            Statement::Case(case) => {
                token_stream.finish().unwrap();
                *case
            }
            _ => panic!("test source is not a case statement"),
        }
    }

    #[test]
    fn empty_case_parses() {
        let case = parse_case("case 1 of end");

        assert_eq!(0, case.branches.len());
        assert_eq!(None, case.else_branch);
    }

    #[test]
    #[should_panic]
    fn case_with_garbage_is_err() {
        parse_case("case 1 of cat dog horse end");
    }

    #[test]
    #[should_panic]
    fn case_unterminated_is_err() {
        parse_case("case 1 of 1: a()");
    }

    #[test]
    fn case_with_single_branch() {
        let case = parse_case("case 1 of 1: a() end");
        assert_eq!(1, case.branches.len());
        assert_eq!(None, case.else_branch);
    }

    #[test]
    fn case_with_single_branch_and_separator() {
        let case = parse_case("case 1 of 1: a(); end");
        assert_eq!(1, case.branches.len());
        assert_eq!(None, case.else_branch);
    }

    #[test]
    fn case_with_separated_branches() {
        let case = parse_case("case 1 of 1: a(); 2: b(); 3: c() end");
        assert_eq!(3, case.branches.len());
        assert_eq!(None, case.else_branch);
    }

    #[test]
    fn case_with_unseparated_else() {
        let case = parse_case("case 1 of 1: a() else b() end");
        assert_eq!(1, case.branches.len());
        assert!(case.else_branch.is_some());
    }

    #[test]
    fn case_with_else_and_separator_after_final_branch() {
        let case = parse_case("case 1 of 1: a(); 2: b(); else c() end");
        assert_eq!(2, case.branches.len());
        assert!(case.else_branch.is_some());
    }

    #[test]
    fn case_with_separator_after_final_branch_and_else_stmt() {
        let case = parse_case("case 1 of 1: a(); 2: b(); else c(); end");
        assert_eq!(2, case.branches.len());
        assert!(case.else_branch.is_some());
    }
}
