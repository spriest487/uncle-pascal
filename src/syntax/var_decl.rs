use tokens;
use tokens::AsToken;
use keywords;
use node;
use syntax::*;
use ToSource;

pub type VarDecl = node::VarDecl<node::Identifier>;
pub type Vars = node::Vars<node::Identifier>;

impl ToSource for Vars {
    fn to_source(&self) -> String {
        let decl_lines = self.decls.iter()
            .map(|decl| {
                format!("\t{}: {};", decl.name, decl.decl_type)
            })
            .collect::<Vec<_>>()
            .join("\n");

        format!("var\n{}", decl_lines)
    }
}

impl Vars {
    pub fn parse<TIter>(in_tokens: TIter, context: &TIter::Item) -> ParseResult<Vars, TIter::Item>
        where TIter: IntoIterator + 'static,
              TIter::Item: tokens::AsToken + 'static
    {
        let var_kw = keywords::Var.match_one(in_tokens, context)?;

        let mut next_tokens = WrapIter::new(var_kw.next_tokens);
        let mut last_context = var_kw.last_token;

        let mut decls = Vec::new();
        loop {
            let mut peekable_tokens = next_tokens.peekable();
            match peekable_tokens.peek().cloned() {
                Some(ref id) if id.as_token().is_any_identifier() => {
                    let match_decl = Matcher::AnyIdentifier
                        .and_then(tokens::Colon)
                        .and_then(Matcher::AnyIdentifier)
                        .and_then(tokens::Semicolon);

                    let decl = match_decl.match_sequence(peekable_tokens, &last_context)?;

                    let name = decl.value[0].as_token().unwrap_identifier().to_owned();
                    let decl_type = node::Identifier::parse(decl.value[2].as_token().unwrap_identifier());
                    decls.push(VarDecl { name, decl_type });

                    next_tokens = WrapIter::new(decl.next_tokens);
                    last_context = decl.last_token;
                }
                Some(ref _unexpected) => {
                    next_tokens = WrapIter::new(peekable_tokens);
                    break;
                }
                None => {
                    return Err(ParseError::UnexpectedEOF(Matcher::AnyIdentifier, last_context));
                }
            }
        }

        Ok(ParseOutput::new(Vars { decls }, last_context, next_tokens))
    }
}
