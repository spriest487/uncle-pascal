use types;
use tokens;
use keywords;
use syntax::*;

#[derive(Clone, Debug)]
pub struct VarDecl {
    name: String,
    decl_type: types::Identifier,
}

impl VarDecl {
    pub fn parse<TIter, TToken>(in_tokens: TIter) -> ParseResult<Vec<VarDecl>, TToken>
        where TIter: IntoIterator<Item=TToken> + 'static,
              TToken: tokens::AsToken + 'static
    {
        let match_kw = TokenMatcher::Keyword(keywords::Var);
        let (_, after_kw) = match_kw.match_one(in_tokens.into_iter())?.unwrap();

        let match_terminator = TokenMatcher::Exact(tokens::Semicolon);
        let (until_terminator, remaining) = match_terminator.split(after_kw)?.unwrap();

        let match_separator = TokenMatcher::Exact(tokens::Semicolon);

        let decls = until_terminator.before.split(|token| {
            match_separator.match_token(token)
        });

        let parse_decls = decls
            .map(|decl_tokens| -> Result<VarDecl, ParseError<TToken>> {
                let match_decl = TokenMatcher::AnyIdentifier
                    .and_then(TokenMatcher::Exact(tokens::Colon))
                    .and_then(TokenMatcher::AnyIdentifier);

                let decl = match_decl.match_tokens(decl_tokens
                    .into_iter()
                    .cloned())?
                    .finish()?;

                let name = decl[0].as_token().unwrap_identifier().to_owned();
                let decl_type = types::Identifier::parse(decl[2].as_token().unwrap_identifier());

                Ok(VarDecl { name, decl_type })
            })
            .collect::<Result<Vec<_>, ParseError<TToken>>>();

        Ok(ParseOutput::new(parse_decls?, remaining))
    }
}