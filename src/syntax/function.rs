use syntax::*;
use syntax::var_decl::*;
use keywords;
use tokens;
use tokens::AsToken;
use node::{
    self,
    TypeName,
    FunctionKind,
    FunctionModifier,
    FunctionArgModifier,
};

pub type FunctionDecl = node::FunctionDecl<ParsedContext>;
pub type FunctionDeclBody = node::FunctionDeclBody<ParsedContext>;
pub type FunctionArg = node::FunctionArg<ParsedContext>;

impl Parse for FunctionDecl {
    fn parse(tokens: &mut TokenStream) -> ParseResult<Self> {
        //match the name
        let name_match = tokens.match_sequence(vec![
            Self::match_any_function_keyword(),
            Matcher::AnyIdentifier
        ])?;

        let kind_kw = name_match[0].unwrap_keyword();
        let kind = match kind_kw {
            keywords::Constructor => FunctionKind::Constructor,
            keywords::Destructor => FunctionKind::Destructor,
            _ => FunctionKind::Function,
        };

        let fn_name = name_match[1].clone();

        let args = Self::parse_argument_list(tokens)?;

        let return_type = match kind_kw {
            // procedures return nothing
            keywords::Procedure |
            keywords::Destructor =>  None,

            _ => {
                //functions and constructors must return something
                tokens.match_one(tokens::Colon)?;
                let type_id: TypeName = tokens.parse()?;

                Some(type_id)
            }
        };

        let modifiers = Self::parse_modifiers(tokens)?;

        //body (if present) appears after separator or newline
        tokens.match_or_endl(tokens::Semicolon)?;

        let peek_after_sig = tokens.look_ahead().match_one(keywords::Var
            .or(keywords::Const)
            .or(keywords::Begin));

        let body = match peek_after_sig {
            // forward decl
            None => None,

            // decl with body
            Some(_) => {
                /* parse consts and vars until we run out of those */
                let mut local_consts = ConstDecls::default();
                let mut local_vars = VarDecls::default();

                loop {
                    match tokens.look_ahead().match_one(keywords::Var.or(keywords::Const)) {
                        Some(t) => if t.is_keyword(keywords::Var) {
                            let vars: VarDecls = tokens.parse()?;
                            local_vars.decls.extend(vars.decls);
                        } else {
                            let consts: ConstDecls = tokens.parse()?;
                            local_consts.decls.extend(consts.decls);
                        }

                        None => break,
                    }
                }

                let block = Block::parse(tokens)?;

                // body is followed by semicolon or newline
                tokens.match_or_endl(tokens::Semicolon)?;

                Some(FunctionDeclBody {
                    block,
                    local_vars,
                    local_consts,
                })
            }
        };

        Ok(FunctionDecl {
            name: fn_name.unwrap_identifier().to_string(),
            context: fn_name.into(),
            return_type,
            modifiers,
            args,
            kind,
            body,
        })
    }
}

impl FunctionDecl {
    pub fn match_any_function_keyword() -> matcher::Matcher {
        keywords::Function
            .or(keywords::Procedure)
            .or(keywords::Constructor)
            .or(keywords::Destructor)
    }

    pub fn parse_modifiers(tokens: &mut TokenStream) -> ParseResult<Vec<FunctionModifier>> {
        let mut modifiers = Vec::new();

        loop {
            let next_mod_tokens = tokens.look_ahead().match_sequence(tokens::Semicolon
                .and_then(Matcher::AnyIdentifier));

            match next_mod_tokens {
                Some(mod_tokens) => {
                    if mod_tokens[1].is_identifier("cdecl") {
                        modifiers.push(node::FunctionModifier::Cdecl);
                        tokens.advance(mod_tokens.len());
                    } else if mod_tokens[1].is_identifier("stdcall") {
                        modifiers.push(node::FunctionModifier::Stdcall);
                        tokens.advance(mod_tokens.len());
                    } else {
                        break Ok(modifiers);
                    }
                }

                None => break Ok(modifiers),
            }
        }
    }

    pub fn parse_argument_list(tokens: &mut TokenStream) -> ParseResult<Vec<FunctionArg>> {
        match tokens.look_ahead().match_one(tokens::BracketLeft) {
            None => {
                // no args list
                return Ok(Vec::new())
            },
            Some(_) => {
                // continue to read args list in brackets
                tokens.advance(1)
            },
        }

        // cover the case of an empty argument list with brackets: `procedure x()`
        if tokens.look_ahead().match_one(tokens::BracketRight).is_some() {
            tokens.advance(1);
            return Ok(Vec::new());
        }

        let mut args = Vec::new();
        loop {
            let context = tokens.look_ahead().next().ok_or_else(|| {
                ParseError::UnexpectedEOF(Matcher::AnyIdentifier, tokens.context().clone())
            })?;

            /* the modifier comes first if there is one, and applies to all args in this group - e.g.
            `const x, y: Integer` means x and y are consts of type Integer */
            let modifier = tokens.look_ahead().match_one(keywords::Var
                .or(keywords::Const)
                .or(keywords::Out))
                .map(|t| match t.unwrap_keyword() {
                    keywords::Var => FunctionArgModifier::Var,
                    keywords::Const => FunctionArgModifier::Const,
                    keywords::Out => FunctionArgModifier::Out,
                    _ => unreachable!()
                });
            if modifier.is_some() {
                tokens.advance(1);
            }

            /* can declare multiple args at once with the same type like a var section -
            the modifier applies to all of them e.g. `out x, y: Integer` -> two Integer out args */
            let mut names = Vec::new();
            loop {
                let name = tokens.match_one(Matcher::AnyIdentifier)?
                    .unwrap_identifier()
                    .to_string();

                // each time a name is followed by a comma, expect another name after it
                names.push(name);
                match tokens.look_ahead().match_one(tokens::Comma) {
                    Some(_) => tokens.advance(1),
                    None => break,
                }
            };

            let decl_type: TypeName = match modifier {
                // "var" might omit the typename to form an untyped reference
                Some(FunctionArgModifier::Var) => {
                    match tokens.look_ahead().match_one(tokens::Colon) {
                        Some(_colon) => {
                            tokens.advance(1);
                            let decl_type = tokens.parse()?;
                            decl_type
                        }

                        None => {
                            TypeName::UntypedRef { context: context.clone().into() }
                        },
                    }
                }

                // no "var", definitely has a colon + typename
                _ => {
                    tokens.match_one(tokens::Colon)?;
                    tokens.parse()?
                }
            };

            args.extend(names.into_iter().map(|name| {
                node::FunctionArg {
                    name,
                    context: context.clone().into(),
                    decl_type: decl_type.clone(),
                    modifier,
                    default_value: None,
                }
            }));

            match tokens.look_ahead().match_one(tokens::BracketRight) {
                Some(_) => {
                    // found end bracket, advance past it and finish
                    tokens.advance(1);
                    break Ok(args);
                }

                None => {
                    // no end bracket, expect a semicolon and keep reading args
                    tokens.match_one(tokens::Semicolon)?;
                }
            }
        }
    }
}

#[cfg(test)]
mod test {
    use std::rc::Rc;
    use tokenizer;
    use opts::CompileOptions;
    use super::*;

    fn parse_func(src: &str) -> FunctionDecl {
        let tokens = tokenizer::tokenize("test", src, &CompileOptions::default())
            .unwrap();

        FunctionDecl::parse(&mut TokenStream::from(tokens))
            .unwrap()
    }

    fn make_type_name(name: &str) -> TypeName {
        TypeName::with_name(name, source::Token::new(keywords::Program, source::Location {
            line: 0,
            col: 0,
            file: Rc::new("test".to_string()),
        }))
    }

    #[test]
    fn parses_sig_with_empty_args() {
        let func = parse_func("function hello(): System.String; begin end;");

        assert_eq!("hello", func.name);
        assert_eq!(Some(make_type_name("System.String")), func.return_type);
        assert_eq!(0, func.args.len());
    }

    #[test]
    fn parses_sig_without_args() {
        let func = parse_func("function hello: String; begin end;");

        assert_eq!("hello", func.name);
        assert_eq!(Some(make_type_name("String")), func.return_type);
        assert_eq!(0, func.args.len());
    }

    #[test]
    fn parses_sig_with_args() {
        let func = parse_func("function hello(x: System.Float; y: Integer): String; begin end;");

        assert_eq!("hello", func.name);
        assert_eq!(Some(make_type_name("String")), func.return_type);
        assert_eq!(2, func.args.len());

        assert_eq!("x", func.args[0].name);
        assert_eq!(make_type_name("System.Float"), func.args[0].decl_type);

        assert_eq!("y", func.args[1].name);
        assert_eq!(make_type_name("Integer"), func.args[1].decl_type);
    }
}
