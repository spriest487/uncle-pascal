use node::{
    self,
    TypeName,
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
            if tokens.peeked().match_one(Matcher::AnyIdentifier).is_none() {
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
            .and_then(Matcher::Operator(operators::Equals)
                .or(tokens::Colon)))?;

        let name = name_tokens[0].unwrap_identifier().to_string();

        let decl_type = match name_tokens[1].as_token() {
            /* `const x: Int32 = 1` - explicit type */
            tokens::Colon => {
                let decl_type: TypeName = tokens.parse()?;
                tokens.match_one(operators::Equals)?;
                Some(decl_type)
            },

            /* `const x = 1` - implicit type from argument */
            tokens::Operator(operators::Equals) => {
                None
            }
            _ => unreachable!(),
        };

        let value_token = tokens.match_one(Matcher::AnyLiteralInteger
            .or(Matcher::AnyLiteralString)
            .or(Matcher::AnyLiteralBoolean))?;

        let value_context = ParsedContext::from(value_token.clone());

        let value = match value_token.as_token() {
            tokens::LiteralInteger(i) => Expression::literal_int(*i, value_context),
            tokens::LiteralString(s) => Expression::literal_string(s, value_context),
            tokens::Keyword(keywords::True) => Expression::literal_bool(true, value_context),
            tokens::Keyword(keywords::False) => Expression::literal_bool(false, value_context),
            _ => unreachable!(),
        };

        Ok(ConstDecl {
            name,
            value,
            decl_type,
            context: name_tokens[0].clone().into(),
        })
    }
}