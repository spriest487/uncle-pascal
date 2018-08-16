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
        let match_kw = Matcher::Keyword(keywords::Var);
        let (_, after_kw) = match_kw.match_one(in_tokens.into_iter())?.unwrap();

        let match_terminator = Matcher::Exact(tokens::Semicolon);
        let (until_terminator, remaining) = match_terminator.split_at_match(after_kw)?
            .unwrap();

        let match_separator = Matcher::Exact(tokens::Semicolon);

        let decls = until_terminator.before_split.split(|token| {
            match_separator.is_match(token)
        });

        let parse_decls = decls
            .map(|decl_tokens| -> Result<VarDecl, ParseError<TToken>> {
                let match_decl = Matcher::AnyIdentifier
                    .and_then(Matcher::Exact(tokens::Colon))
                    .and_then(Matcher::AnyIdentifier);

                let decl = match_decl.match_sequence(decl_tokens
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