use super::*;
use crate::{
    TokenTree,
    parse::Parse
};
use pas_common::BuildOptions;

fn try_parse_case<Item>(s: &str) -> Result<CaseBlock<Span, Item>, String>
where
    Item: Parse + Spanned
{
    let test_unit = pas_pp::Preprocessor::new("test", BuildOptions::default())
        .preprocess(s)
        .map_err(|err| err.to_string())?;

    let src_tokens = TokenTree::tokenize(test_unit)
        .map_err(|err| err.to_string())?;

    let mut token_stream = TokenStream::new(src_tokens, Span::zero("test"));

    CaseBlock::parse(&mut token_stream)
        .map_err(|err| err.to_string())
}

fn parse_case<Item>(s: &str) -> CaseBlock<Span, Item>
    where
        Item: Parse + Spanned
{
    try_parse_case(s).unwrap()
}

fn parse_case_stmt(s: &str) -> CaseStmt<Span> {
    parse_case::<Stmt<Span>>(s)
}

#[test]
fn empty_case_parses() {
    let case = parse_case_stmt("case 1 of end");

    assert_eq!(0, case.branches.len());
    assert_eq!(None, case.else_branch);
}

#[test]
fn case_with_garbage_is_err() {
    assert!(try_parse_case::<Stmt<Span>>("case 1 of cat dog horse end").is_err());
}

#[test]
fn case_unterminated_is_err() {
    assert!(try_parse_case::<Stmt<Span>>("case 1 of 1: a()").is_err())
}

#[test]
fn case_with_single_branch() {
    let case = parse_case_stmt("case 1 of 1: a() end");
    assert_eq!(1, case.branches.len());
    assert_eq!(None, case.else_branch);
}

#[test]
fn case_with_single_branch_and_separator() {
    let case = parse_case_stmt("case 1 of 1: a(); end");
    assert_eq!(1, case.branches.len());
    assert_eq!(None, case.else_branch);
}

#[test]
fn case_with_separated_branches() {
    let case = parse_case_stmt("case 1 of 1: a(); 2: b(); 3: c() end");
    assert_eq!(3, case.branches.len());
    assert_eq!(None, case.else_branch);
}

#[test]
fn case_with_unseparated_else() {
    let case = parse_case_stmt("case 1 of 1: a() else b() end");
    assert_eq!(1, case.branches.len());
    assert!(case.else_branch.is_some());
}

#[test]
fn case_with_else_and_separator_after_final_branch() {
    let case = parse_case_stmt("case 1 of 1: a(); 2: b(); else c() end");
    assert_eq!(2, case.branches.len());
    assert!(case.else_branch.is_some());
}

#[test]
fn case_with_separator_after_final_branch_and_else_stmt() {
    let case = parse_case_stmt("case 1 of 1: a(); 2: b(); else c(); end");
    assert_eq!(2, case.branches.len());
    assert!(case.else_branch.is_some());
}
