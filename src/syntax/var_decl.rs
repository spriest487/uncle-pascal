use tokens;
use tokens::AsToken;
use keywords;
use node::{self, TypeName};
use syntax::*;

pub type VarDecl = node::VarDecl<ParsedSymbol, ParsedContext>;
pub type VarDecls = node::VarDecls<ParsedSymbol, ParsedContext>;

impl Parse for Vec<VarDecl> {
    fn parse(tokens: &mut TokenStream) -> ParseResult<Self> {
        let mut decls = Vec::new();

        loop {
            /* keep matching until the next thing in the token stream
            doesn't look like a valid var decl */
            match tokens.peeked().next().map(|t| t.as_token().clone()) {
                Some(tokens::Identifier(_)) => {}
                _ => break,
            }

            /* parse names until the next colon */
            let mut names = Vec::new();
            loop {
                let name_tokens = tokens.match_sequence(Matcher::AnyIdentifier
                    .and_then(tokens::Colon.or(tokens::Comma)))?;

                let name = name_tokens[0].unwrap_identifier().to_string();
                let context = ParsedContext::from(name_tokens[0].clone());

                names.push((name, context));

                if *name_tokens[1].as_token() == tokens::Colon {
                    break;
                }
            }

            let decl_type: TypeName = tokens.parse()?;
            tokens.match_or_endl(tokens::Semicolon)?;

            for (name, context) in names {
                decls.push(VarDecl {
                    name,
                    context,
                    decl_type: decl_type.clone(),
                })
            }
        }

        Ok(decls)
    }
}

impl Parse for VarDecls {
    fn parse(tokens: &mut TokenStream) -> ParseResult<VarDecls> {
        tokens.match_one(keywords::Var)?;

        let decls: Vec<VarDecl> = tokens.parse()?;

        /* it's legal for a var section to be nothing but a terminator */
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
        assert_eq!("x", vars.decls[0].name);
        assert_eq!(TypeName::with_name("Integer"), vars.decls[0].decl_type);
    }

    #[test]
    fn parses_var_list() {
        let vars = parse_vars("var x: System.Integer; y: System.String;");

        assert_eq!(2, vars.decls.len());

        assert_eq!("x", vars.decls[0].name);
        assert_eq!(TypeName::with_name("System.Integer"), vars.decls[0].decl_type);

        assert_eq!("y", vars.decls[1].name);
        assert_eq!(TypeName::with_name("System.String"), vars.decls[1].decl_type);
    }

    #[test]
    fn parses_var_with_indirection() {
        let vars = parse_vars("var x: ^T.B");

        assert_eq!(1, vars.decls.len());
        assert_eq!("x", vars.decls[0].name);
        assert_eq!(TypeName::with_name("T.B").pointer(), vars.decls[0].decl_type);
    }
}
