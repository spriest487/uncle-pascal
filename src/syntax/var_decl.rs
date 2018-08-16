use std::collections::HashSet;

use tokens;
use tokens::AsToken;
use keywords;
use operators;
use node::{self, Identifier};
use syntax::*;
use source;

pub type VarDecl = node::VarDecl<Identifier>;
pub type VarDecls = node::VarDecls<Identifier>;

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
        /* var names can't be fully-qualified, so we only need to match a
        single name token here */
        let id_tokens = Matcher::AnyIdentifier
            .and_then(tokens::Colon)
            .match_sequence(in_tokens, &context)?;

        let name_token = id_tokens.value[0].clone();
        let name = Identifier::from(name_token.as_token().unwrap_identifier());

        /* match any number of unique modifiers */
        let mut modifier_tokens = WrapIter::new(id_tokens.next_tokens);
        let mut modifier_context = id_tokens.last_token;
        let mut modifiers = HashSet::new();
        let match_any_modifier = Matcher::from(operators::Deref);

        loop {
            let match_modifier = match_any_modifier.match_peek(modifier_tokens,
                                                               &modifier_context)?;

            modifier_tokens = WrapIter::new(match_modifier.next_tokens
                .skip(if match_modifier.value.is_some() {
                    1
                } else {
                    0
                }));

            modifier_context = match_modifier.last_token;

            let mut add_or_err = |modifier: node::VarModifier| {
                if modifiers.contains(&modifier) {
                    let expected = match_any_modifier
                        .clone()
                        .or(Matcher::AnyIdentifier);

                    Err(ParseError::UnexpectedToken(modifier_context.clone(),
                                                    Some(expected)))
                } else {
                    modifiers.insert(modifier);
                    Ok(())
                }
            };

            match match_modifier.value {
                Some(ref t) if t.is_operator(operators::Deref) => {
                    add_or_err(node::VarModifier::Pointer)?;
                }

                _ => break,
            }
        }

        let type_id = Identifier::parse(modifier_tokens,
        &modifier_context)?;

        let decl = VarDecl {
            name,
            decl_type: type_id.value,
            context: name_token.clone(),
            modifiers,
        };

        Ok(ParseOutput::new(decl, type_id.last_token, type_id.next_tokens))
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
        assert_eq!(Identifier::from("x"), vars.decls[0].name);
        assert_eq!(Identifier::from("Integer"), vars.decls[0].decl_type);
    }

    #[test]
    fn parses_var_list() {
        let vars = parse_vars("var x: System.Integer; y: System.String;");

        assert_eq!(2, vars.decls.len());

        assert_eq!(Identifier::from("x"), vars.decls[0].name);
        assert_eq!(Identifier::from("System.Integer"), vars.decls[0].decl_type);

        assert_eq!(Identifier::from("y"), vars.decls[1].name);
        assert_eq!(Identifier::from("System.String"), vars.decls[1].decl_type);
    }

    #[test]
    fn parses_var_with_modifier() {
        let vars = parse_vars("var x: ^T.B");

        assert_eq!(1, vars.decls.len());
        assert_eq!(Identifier::from("x"), vars.decls[0].name);
        assert_eq!(Identifier::from("T.B"), vars.decls[0].decl_type);
        assert!(vars.decls[0].modifiers.contains(&node::VarModifier::Pointer))
    }
}
