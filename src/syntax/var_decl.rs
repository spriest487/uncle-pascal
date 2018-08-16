use types;
use tokens;
use keywords;
use syntax::*;
use ToSource;

#[derive(Clone, Debug)]
pub struct VarDecl {
    name: String,
    decl_type: types::Identifier,
}

#[derive(Clone, Debug, Default)]
pub struct Vars {
    pub decls: Vec<VarDecl>,
}

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
        let match_kw = Matcher::Keyword(keywords::Var);
        let (_, _, after_kw) = match_kw.match_one(in_tokens.into_iter(), context)?.unwrap();

        let mut next_tokens = after_kw;
        let mut last_context = context.clone();

        let mut decls = Vec::new();
        loop {
            let mut peekable_tokens = next_tokens.peekable();
            match peekable_tokens.peek().cloned() {
                Some(ref id) if id.as_token().is_any_identifier() => {
                    let match_decl = Matcher::AnyIdentifier
                        .and_then(Matcher::Exact(tokens::Colon))
                        .and_then(Matcher::AnyIdentifier)
                        .and_then(Matcher::Exact(tokens::Semicolon));

                    let (decl, _, after_decl) = match_decl.match_sequence(peekable_tokens, &last_context)?
                        .unwrap();

                    let name = decl[0].as_token().unwrap_identifier().to_owned();
                    let decl_type = types::Identifier::parse(decl[2].as_token().unwrap_identifier());
                    decls.push(VarDecl { name, decl_type });

                    next_tokens = after_decl;
                    last_context = decl[3].clone();
                }
                Some(ref _unexpected) => {
                    next_tokens = WrapIter::new(peekable_tokens);
                    break;
                }
                None => {
                    return Err(ParseError::UnexpectedEOF(Matcher::AnyIdentifier, last_context));
                },
            }
        }

        Ok(ParseOutput::new(Vars{ decls}, last_context, next_tokens.into_iter()))
    }
}
