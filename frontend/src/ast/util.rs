use crate::ast::Unit;
use crate::parse::ParseResult;
use crate::parse::TokenStream;
use crate::ast::Ident;
use crate::ast::IdentPath;
use crate::TokenTree;
use common::span::Span;
use common::BuildOptions;
use crate::pp::Preprocessor;

pub fn try_unit_from_string(unit_name: &str, src: &str) -> ParseResult<Unit<Span>> {
    let pp = Preprocessor::new(format!("{}.pas", unit_name), BuildOptions::default());
    let pp_unit = pp.preprocess(src).unwrap();

    let tokens = TokenTree::tokenize(pp_unit).unwrap();
    let mut stream = TokenStream::new(tokens, Span::zero(unit_name));

    let unit_ident = Ident::new(unit_name, Span::zero(unit_name));

    let unit = Unit::parse(&mut stream, IdentPath::from_parts(vec![unit_ident]))?;
    stream.finish()?;

    Ok(unit)
}

pub fn unit_from_string(unit_name: &str, src: &str) -> Unit<Span> {
    try_unit_from_string(unit_name, src).unwrap()
}
