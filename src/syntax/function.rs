use syntax::*;
use syntax::var_decl::*;
use keywords;
use tokens;
use tokens::AsToken;
use node;
use ToSource;

pub type Function = node::Function<node::Identifier>;

impl Function {
    pub fn parse<TIter>(in_tokens: TIter, context: &TIter::Item) -> ParseResult<Self, TIter::Item>
        where TIter: IntoIterator + 'static,
              TIter::Item: tokens::AsToken + 'static
    {
        //match the sig
        let sig_match = Matcher::Keyword(keywords::Function)
            .and_then(Matcher::AnyIdentifier)
            .and_then(Matcher::Exact(tokens::Colon))
            .and_then(Matcher::AnyIdentifier)
            .and_then(Matcher::Exact(tokens::Semicolon));

        let sig = sig_match.match_sequence(in_tokens.into_iter(), context)?;

        let fn_name = &sig.value[1];
        let fn_return_type = &sig.value[3];

        let peek_after_sig = Matcher::Keyword(keywords::Var)
            .or(Matcher::Keyword(keywords::Begin))
            .match_peek(sig.next_tokens, &sig.last_token)?;

        let local_vars = match peek_after_sig.value {
            Some(ref var_kw) if var_kw.as_token().is_keyword(keywords::Var) => {
                Vars::parse(peek_after_sig.next_tokens, &peek_after_sig.last_token)?
            }
            _ => {
                ParseOutput::new(Vars::default(),
                                 peek_after_sig.last_token,
                                 peek_after_sig.next_tokens)
            }
        };

        let body_block = Block::parse(local_vars.next_tokens, &local_vars.last_token)?;

        let match_semicolon = Matcher::Exact(tokens::Semicolon);
        let last_semicolon = match_semicolon
            .match_one(body_block.next_tokens, &body_block.last_token)?;

        let function = Function {
            name: fn_name.as_token().unwrap_identifier().to_owned(),
            return_type: node::Identifier::parse(fn_return_type.as_token().unwrap_identifier()),

            local_vars: local_vars.value,
            args: Vars{ decls: Vec::new() }, //TODO: steal vars parsing and make a match_list for ; vs , arg list

            body: body_block.value,
        };

        Ok(ParseOutput::new(function,
                            last_semicolon.last_token,
                            last_semicolon.next_tokens))
    }
}

impl ToSource for Function {
    fn to_source(&self) -> String {
        let mut lines = Vec::new();
        lines.push(format!("function {};", self.name));

        if self.local_vars.decls.len() > 0 {
            lines.push(self.local_vars.to_source());
        }

        lines.push(self.body.to_source() + ";");

        lines.join("\n")
    }
}
