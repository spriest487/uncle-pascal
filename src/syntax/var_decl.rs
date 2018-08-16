use tokens;
use tokens::AsToken;
use keywords;
use node::{self, Identifier, TypeName};
use syntax::*;

pub type VarDecl = node::VarDecl<ParsedSymbol>;
pub type VarDecls = node::VarDecls<ParsedSymbol>;

impl Parse for VarDecl {
    fn parse(tokens: &mut TokenStream) -> ParseResult<VarDecl> {
        /* var names can't be fully-qualified, so we only need to match a
        single name token here */
        let id_tokens = tokens.match_sequence(Matcher::AnyIdentifier
            .and_then(tokens::Colon))?;

        let name_token = id_tokens[0].clone();
        let name = Identifier::from(name_token.as_token().unwrap_identifier());

        let decl_type: TypeName = tokens.parse()?;

        Ok(VarDecl {
            name,
            decl_type,
            context: name_token,
        })
    }
}

impl Parse for VarDecls {
    fn parse(tokens: &mut TokenStream) -> ParseResult<VarDecls> {
        tokens.match_one(keywords::Var)?;

        let mut decls = Vec::new();
        loop {
            match tokens.peek() {
                Some(ref id) if id.as_token().is_any_identifier() => {
                    let decl: VarDecl = tokens.parse()?;
                    tokens.match_or_endl(tokens::Semicolon)?;

                    decls.push(decl);
                }

                _ => break,
            }
        }

        // it's legal for a var section to be nothing but a terminator
        if decls.len() == 0 {
            tokens.match_or_endl(tokens::Semicolon)?;
        }

        Ok(VarDecls {
            decls
        })
    }
}

#[cfg(test)]
mod test {
    use super::*;

    fn parse_vars(src: &str) -> VarDecls {
        TokenStream::tokenize("test", src)
            .unwrap()
            .parse_to_end()
            .unwrap()
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
        assert_eq!(TypeName::with_name("Integer"), vars.decls[0].decl_type);
    }

    #[test]
    fn parses_var_list() {
        let vars = parse_vars("var x: System.Integer; y: System.String;");

        assert_eq!(2, vars.decls.len());

        assert_eq!(Identifier::from("x"), vars.decls[0].name);
        assert_eq!(TypeName::with_name("System.Integer"), vars.decls[0].decl_type);

        assert_eq!(Identifier::from("y"), vars.decls[1].name);
        assert_eq!(TypeName::with_name("System.String"), vars.decls[1].decl_type);
    }

    #[test]
    fn parses_var_with_indirection() {
        let vars = parse_vars("var x: ^T.B");

        assert_eq!(1, vars.decls.len());
        assert_eq!(Identifier::from("x"), vars.decls[0].name);
        assert_eq!(TypeName::with_name("T.B").pointer(), vars.decls[0].decl_type);
    }
}
