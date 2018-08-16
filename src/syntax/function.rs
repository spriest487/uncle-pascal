use syntax::*;
use keywords;
use tokens;
use types;

#[derive(Debug, Clone)]
pub struct Function {
    name: String,
    return_type: types::Identifier,

    local_vars: Vec<var_decl::VarDecl>,
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

        let (first_after_sig, after_sig) = TokenMatcher::Keyword(keywords::Var)
            .or(TokenMatcher::Keyword(keywords::Begin))
            .match_peek(after_sig)?
            .unwrap();

        let (local_vars, after_local_vars) = if first_after_sig.as_token()
            .is_keyword(keywords::Var) {
            var_decl::VarDecl::parse(after_sig)?.unwrap()
        } else {
            (Vec::new(), after_sig)
        };

        let (body_match, after_body) = TokenMatcher::Keyword(keywords::Begin)
            .closed_with(TokenMatcher::Keyword(keywords::End))
            .match_pair(after_local_vars)?
            .unwrap();

        let match_semicolon = TokenMatcher::Exact(tokens::Semicolon);
        let (_, remaining) = match_semicolon.match_one(after_body)?.unwrap();

        let function = Function {
            name: fn_name.as_token().unwrap_identifier().to_owned(),
            return_type: types::Identifier::parse(fn_return_type.as_token().unwrap_identifier()),

            local_vars,

            body: body_match.between.into_iter()
                .map(|t| t.as_token().clone())
                .collect(),
        };

        Ok(ParseOutput::new(function, remaining))
    }
}
