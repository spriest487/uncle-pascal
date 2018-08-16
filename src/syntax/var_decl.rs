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

impl VarDecl {
    pub fn parse<TIter>(in_tokens: TIter, context: &TIter::Item) -> ParseResult<VarDecl, TIter::Item>
        where TIter: IntoIterator + 'static,
              TIter::Item: tokens::AsToken + 'static
    {
        let decl = Matcher::AnyIdentifier
            .and_then(tokens::Colon)
            .and_then(Matcher::AnyIdentifier)
            .match_sequence(in_tokens, context)?;

        let name = decl.value[0].as_token().unwrap_identifier().to_owned();
        let decl_type = node::Identifier::parse(decl.value[2].as_token().unwrap_identifier());

        Ok(ParseOutput::new(VarDecl { name, decl_type },
                            decl.last_token,
                            decl.next_tokens))
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
                    let decl = VarDecl::parse(peekable_tokens, &last_context)?;
                    let semicolon = tokens::Semicolon.match_one(decl.next_tokens,
                                                                &decl.last_token)?;
                    decls.push(decl.value);

                    next_tokens = WrapIter::new(semicolon.next_tokens);
                    last_context = semicolon.last_token;
                }
                _ => {
                    next_tokens = WrapIter::new(peekable_tokens);
                    break;
                }
            }
        }

        Ok(ParseOutput::new(Vars { decls }, last_context, next_tokens))
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use tokenizer::*;

    static NO_CONTEXT : SourceToken = SourceToken {
        token: tokens::Keyword(keywords::Program),
        line: 0,
        col: 0
    };

    fn parse_vars(src: &str) -> Vars {
        let vars = Vars::parse(tokenize(src).unwrap(), &NO_CONTEXT);

        assert!(vars.is_ok(), "test source `{}` must parse correctly", src);

        vars.unwrap().value
    }

    #[test]
    fn parses_empty_vars() {
        let vars = parse_vars("var;");

        assert_eq!(0, vars.decls.len());
    }

    #[test]
    fn parses_var() {
        let vars = parse_vars("var x: Integer;");

        assert_eq!(1, vars.decls.len());
        assert_eq!("x", vars.decls[0].name);
        assert_eq!(node::Identifier::parse("Integer"), vars.decls[0].decl_type);
    }

    #[test]
    fn parses_var_list() {
        let vars = parse_vars("var x: Integer; y: String;");

        assert_eq!(2, vars.decls.len());

        assert_eq!("x", vars.decls[0].name);
        assert_eq!(node::Identifier::parse("Integer"), vars.decls[0].decl_type);

        assert_eq!("y", vars.decls[1].name);
        assert_eq!(node::Identifier::parse("String"), vars.decls[1].decl_type);
    }
}
