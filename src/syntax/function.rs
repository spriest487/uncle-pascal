use syntax::*;
use keywords;
use tokens;
use types;

#[derive(Debug, Clone)]
pub struct Function {
    name: String,
    return_type: types::Identifier,

    //TODO
    body: Vec<tokens::Token>,
}

impl Function {
    pub fn parse<TIter, TToken>(in_tokens: TIter) -> ParseResult<Self, TToken>
        where TIter: IntoIterator<Item=TToken> + 'static,
              TToken: tokens::AsToken + 'static
    {
        //match the sig
        let sig_match = TokenMatcher::Keyword(keywords::Function)
            .and_then(TokenMatcher::AnyIdentifier)
            .and_then(TokenMatcher::Exact(tokens::Colon))
            .and_then(TokenMatcher::AnyIdentifier)
            .and_then(TokenMatcher::Exact(tokens::Semicolon));

        let (sig, after_sig) = sig_match.match_tokens(in_tokens.into_iter())?.unwrap();

        let fn_name = &sig[1];
        let fn_return_type = &sig[3];

        //TODO: just stop at next END for now
        let match_end = TokenMatcher::Keyword(keywords::End);

        let (fn_tokens, mut after_end) = match_end.until_match(after_sig)?
            .unwrap();

        match after_end.next() {
            Some(ref token) if *token.as_token() == tokens::Semicolon => {
                let function = Function {
                    name: fn_name.as_token().unwrap_identifier().to_owned(),
                    return_type: types::Identifier::parse(fn_return_type.as_token().unwrap_identifier()),

                    body: fn_tokens.into_iter()
                        .map(|t| t.as_token().clone())
                        .collect(),
                };

                Ok(ParseOutput::new(function, after_end))
            },

            Some(unexpected) => {
                Err(ParseError::UnexpectedToken(
                    unexpected,
                    Some(TokenMatcher::Exact(tokens::Semicolon)),
                ))
            },

            None => {
                Err(ParseError::UnexpectedEOF)
            },
        }
    }
}
