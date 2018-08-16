use syntax::*;
use syntax::var_decl::*;
use keywords;
use tokens;
use types;
use ToSource;

#[derive(Debug, Clone)]
pub struct Function {
    name: String,
    return_type: types::Identifier,

    local_vars: Vars,
    //TODO
    body: Vec<tokens::Token>,
}

impl Function {
    pub fn parse<TIter, TToken>(in_tokens: TIter) -> ParseResult<Self, TToken>
        where TIter: IntoIterator<Item=TToken> + 'static,
              TToken: tokens::AsToken + 'static
    {
        //match the sig
        let sig_match = Matcher::Keyword(keywords::Function)
            .and_then(Matcher::AnyIdentifier)
            .and_then(Matcher::Exact(tokens::Colon))
            .and_then(Matcher::AnyIdentifier)
            .and_then(Matcher::Exact(tokens::Semicolon));

        let (sig, after_sig) = sig_match.match_sequence(in_tokens.into_iter())?.unwrap();

        let fn_name = &sig[1];
        let fn_return_type = &sig[3];

        let (first_after_sig, after_sig) = Matcher::Keyword(keywords::Var)
            .or(Matcher::Keyword(keywords::Begin))
            .match_peek(after_sig)?
            .unwrap();

        let (local_vars, after_local_vars) = if first_after_sig.as_token()
            .is_keyword(keywords::Var) {
            Vars::parse(after_sig)?.unwrap()
        } else {
            (Vars::default(), after_sig)
        };

        let (body_match, after_body) = Matcher::Keyword(keywords::Begin)
            .paired_with(Matcher::Keyword(keywords::End))
            .match_pair(after_local_vars)?
            .unwrap();

        let match_semicolon = Matcher::Exact(tokens::Semicolon);
        let (_, remaining) = match_semicolon.match_one(after_body)?.unwrap();

        let function = Function {
            name: fn_name.as_token().unwrap_identifier().to_owned(),
            return_type: types::Identifier::parse(fn_return_type.as_token().unwrap_identifier()),

            local_vars,

            body: body_match.inner.into_iter()
                .map(|t| t.as_token().clone())
                .collect(),
        };

        Ok(ParseOutput::new(function, remaining))
    }
}

impl ToSource for Function {
    fn to_source(&self) -> String {
        let mut lines = Vec::new();
        lines.push(format!("function {};", self.name));

        if self.local_vars.decls.len() > 0 {
            lines.push(self.local_vars.to_source());
        }

        lines.push("begin".to_owned());
        lines.push(self.body.iter().map(|t| t.to_source()).collect::<Vec<_>>().join(" "));
        lines.push("end;".to_owned());

        lines.join("\n")
    }
}
