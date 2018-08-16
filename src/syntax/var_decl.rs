use tokens;
use tokens::AsToken;
use keywords;
use node;
use syntax::*;
use source;

pub type VarDecl = node::VarDecl<node::Identifier>;
pub type VarDecls = node::VarDecls<node::Identifier>;

impl node::ToSource for VarDecls {
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
    pub fn parse<TIter>(in_tokens: TIter, context: &source::Token) -> ParseResult<VarDecl>
        where TIter: IntoIterator<Item=source::Token> + 'static
    {
        let decl_tokens = Matcher::AnyIdentifier
            .and_then(tokens::Colon)
            .and_then(Matcher::AnyIdentifier)
            .match_sequence(in_tokens, context)?;

        let name_token = decl_tokens.value[0].clone();

        let name = name_token.as_token().unwrap_identifier().to_owned();
        let decl_type = node::Identifier::parse(decl_tokens.value[2].as_token().unwrap_identifier());

        let decl = VarDecl {
            name,
            decl_type,
            context: name_token.clone(),
        };

        Ok(ParseOutput::new(decl, decl_tokens.last_token, decl_tokens.next_tokens))
    }
}

impl VarDecls {
    pub fn parse<TIter>(in_tokens: TIter, context: &source::Token) -> ParseResult<VarDecls>
        where TIter: IntoIterator<Item=source::Token> + 'static
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
                    let semicolon = tokens::Semicolon.match_or_endl(decl.next_tokens,
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

        Ok(ParseOutput::new(VarDecls { decls }, last_context, next_tokens))
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use tokenizer::*;
    use source;

    fn parse_vars(src: &str) -> VarDecls {
        let vars = VarDecls::parse(tokenize("test", src).unwrap(),
                                   &source::test::empty_context());

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
