use std::fmt;
use std::hash::{Hash, Hasher};
use pas_common::span::{Span, Spanned};
use crate::ast::{Annotation, Expression, Statement};
use crate::parse::{MatchOneOf, ParseResult, TokenStream};
use crate::{Keyword, Separator};

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

        let mut branches = Vec::new();

        // is there a valid separator between the last and current statements?
        let mut prev_sep = true;

        let (end_tt, else_branch) = loop {
            if let Some(end_tt) = tokens.match_one_maybe(Keyword::End) {
                break (end_tt, None);
            } else if branches.len() > 0 {
                if let Some(..) = tokens.match_one_maybe(Keyword::Else) {
                    let else_stmt = Statement::parse(tokens)?;

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

#[cfg(test)]
mod test {
    use pas_common::BuildOptions;
    use crate::TokenTree;
    use super::*;

    fn parse_case(s: &str) -> Case<Span> {
        let src_tokens = TokenTree::tokenize("test", s, &BuildOptions::default()).unwrap();
        let mut token_stream = TokenStream::new(src_tokens, Span::zero("test"));

        match Statement::parse(&mut token_stream).unwrap() {
            Statement::Case(case) => {
                token_stream.finish().unwrap();
                *case
            },
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