use node::{
    self,
};
use syntax::{
    ParsedContext,
    ParsedSymbol,
    Parse,
    TokenStream,
    ParseResult,
    Matcher,
    Expression,
};
use keywords;
use tokens::{
    self,
    AsToken,
};
use operators;

pub type ConstDecl = node::ConstDecl<ParsedSymbol, ParsedContext>;
pub type ConstDecls = node::ConstDecls<ParsedSymbol, ParsedContext>;

impl Parse for ConstDecls {
    fn parse(tokens: &mut TokenStream) -> ParseResult<Self> {
        tokens.match_one(keywords::Const)?;

        let mut decls = Vec::new();
        loop {
            if tokens.match_sequence_peek(Matcher::AnyIdentifier
                .and_then(operators::Equals))?
                .is_none() {
                break;
            }

            let decl: ConstDecl = tokens.parse()?;
            tokens.match_or_endl(tokens::Semicolon)?;

            decls.push(decl);
        }

        Ok(ConstDecls {
            decls
        })
    }
}

impl Parse for ConstDecl {
    fn parse(tokens: &mut TokenStream) -> ParseResult<Self> {
        let name_tokens = tokens.match_sequence(Matcher::AnyIdentifier
            .and_then(operators::Equals))?;

        let name = name_tokens[0].unwrap_identifier().to_string();

        let value_token = tokens.match_one(Matcher::AnyLiteralInteger
            .or(Matcher::AnyLiteralString))?;

        let value_context = ParsedContext::from(value_token.clone());

        let value = match value_token.as_token() {
            tokens::LiteralInteger(i) => Expression::literal_int(*i, value_context),
            tokens::LiteralString(s) => Expression::literal_string(s, value_context),
            _ => unreachable!(),
        };

        Ok(ConstDecl {
            name,
            value,
            context: name_tokens[0].clone().into(),
        })
    }
}