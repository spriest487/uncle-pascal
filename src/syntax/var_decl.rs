use tokens;
use tokens::AsToken;
use keywords;
use node::{
    self,
    TypeName,
};

use operators;
use syntax::{
    ParseResult,
    ParsedContext,
    Expression,
    TokenStream,
    Parse,
    Matcher,
    MatchOneOf,
};

pub type VarDecl = node::VarDecl<ParsedContext>;

impl Parse for Vec<VarDecl> {
    fn parse(tokens: &mut TokenStream) -> ParseResult<Self> {
        let mut decls = Vec::new();

        loop {
            /* keep matching until the next thing in the token stream
            doesn't look like a valid var decl */
            match tokens.look_ahead().next().map(|t| t.as_token().clone()) {
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

            if names.len() == 1 {}

            let default_value: Option<Expression> = if names.len() == 1 {
                match tokens.look_ahead().next() {
                    Some(ref t) if t.is_operator(operators::Equals) => {
                        tokens.advance(1);

                        Some(tokens.parse()?)
                    }

                    _ => None,
                }
            } else {
                None
            };

            tokens.match_or_endl(tokens::Semicolon)?;

            if names.len() == 1 {
                let (name, context) = names.into_iter().next().unwrap();
                decls.push(VarDecl {
                    name,
                    context,
                    decl_type,
                    default_value,
                })
            } else {
                for (name, context) in names {
                    decls.push(VarDecl {
                        name,
                        context,
                        decl_type: decl_type.clone(),
                        default_value: None,
                    })
                }
            }
        }

        Ok(decls)
    }
}

impl VarDecl {
    /**
        parse a "var" section, consisting of the "var" keyword followed by
        0+ var declarations, separated by semicolon terminators and/or newlines

        part of the implementation or interface section of
        a unit, the global section of a program, or the decls section of a function
     */
    pub fn parse_var_section(tokens: &mut TokenStream) -> ParseResult<Vec<VarDecl>> {
        tokens.match_one(keywords::Var)?;

        let vars = tokens.parse::<Vec<VarDecl>>()?;

        /* it's legal for a var section to be nothing but a terminator */
        if vars.is_empty() {
            tokens.match_or_endl(tokens::Semicolon)?;
        }

        Ok(vars)
    }
}

#[cfg(test)]
mod test {
    use std::rc::Rc;

    use super::*;
    use source;
    use opts::CompileOptions;
    use node::ScalarTypeName;

    fn parse_vars(src: &str) -> Vec<VarDecl> {
        let mut tokens = TokenStream::tokenize("test", src, &CompileOptions::default())
            .unwrap();

        VarDecl::parse_var_section(&mut tokens).unwrap()
    }

    fn make_type_name(name: &str) -> TypeName {
        TypeName::Scalar(ScalarTypeName::with_name(
            name,
            source::Token::new(
                keywords::Program, source::Location {
                    line: 0,
                    col: 0,
                    file: Rc::new("test".to_string()),
                })
        ))
    }

    #[test]
    fn parses_empty_vars() {
        let vars = parse_vars("var;");

        assert_eq!(0, vars.len());
    }

    #[test]
    fn parses_var() {
        let vars = parse_vars("var x: Integer;");

        assert_eq!(1, vars.len());
        assert_eq!("x", vars[0].name);
        assert_eq!(make_type_name("Integer"), vars[0].decl_type);
    }

    #[test]
    fn parses_var_list() {
        let vars = parse_vars("var x: System.Integer; y: System.String;");

        assert_eq!(2, vars.len());

        assert_eq!("x", vars[0].name);
        assert_eq!(make_type_name("System.Integer"), vars[0].decl_type);

        assert_eq!("y", vars[1].name);
        assert_eq!(make_type_name("System.String"), vars[1].decl_type);
    }

    #[test]
    fn parses_var_with_indirection() {
        let vars = parse_vars("var x: ^T.B");

        assert_eq!(1, vars.len());
        assert_eq!("x", vars[0].name);
        assert_eq!(make_type_name("T.B").pointer(), vars[0].decl_type);
    }
}
