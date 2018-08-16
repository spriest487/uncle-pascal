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

    body: Block,
}

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

        let (sig, sig_last, after_sig) = sig_match.match_sequence(in_tokens.into_iter(), context)?.unwrap();

        let fn_name = &sig[1];
        let fn_return_type = &sig[3];

        let (first_after_sig, _, after_sig) = Matcher::Keyword(keywords::Var)
            .or(Matcher::Keyword(keywords::Begin))
            .match_peek(after_sig, &sig_last)?
            .unwrap();

        let local_vars = match first_after_sig {
            Some(ref var_kw) if var_kw.as_token().is_keyword(keywords::Var) => {
                Vars::parse(after_sig, &sig_last)?
            }
            _ => ParseOutput::new(Vars::default(), sig_last, after_sig)
        };

        let body_block = Block::parse(local_vars.next, &local_vars.last_parsed)?;

        let match_semicolon = Matcher::Exact(tokens::Semicolon);
        let (_, after_last_semicolon, remaining) = match_semicolon
            .match_one(body_block.next, &body_block.value.end)?
            .unwrap();

        let function = Function {
            name: fn_name.as_token().unwrap_identifier().to_owned(),
            return_type: types::Identifier::parse(fn_return_type.as_token().unwrap_identifier()),

            local_vars: local_vars.value,

            body: body_block.value.block,
        };

        Ok(ParseOutput::new(function,
                            after_last_semicolon,
                            remaining))
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
