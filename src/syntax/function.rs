use syntax::*;
use syntax::var_decl::*;
use keywords;
use tokens;
use tokens::AsToken;
use node::{self, Identifier, FunctionKind};

pub type FunctionDecl = node::FunctionDecl<ParsedSymbol>;
pub type FunctionDeclBody = node::FunctionDeclBody<ParsedSymbol>;

impl FunctionDecl {
    pub fn match_any_function_keyword() -> matcher::Matcher {
        keywords::Function
            .or(keywords::Procedure)
            .or(keywords::Constructor)
            .or(keywords::Destructor)
    }

    pub fn parse<TIter>(tokens: &mut TokenStream) -> ParseResult<Self> {
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

        let arg_groups = match tokens.match_peek(tokens::BracketLeft)? {
            Some(_) => {
                tokens.match_groups(tokens::BracketLeft, tokens::BracketRight, tokens::Semicolon)?
                    .groups
            }
            None => Vec::new(),
        };

        let args = arg_groups.into_iter()
            .map(|arg_tokens| {
                let mut arg_tokens = TokenStream::new(arg_tokens.tokens, &arg_tokens.context);

                let var_decl = VarDecl::parse(&mut arg_tokens)?;
                arg_tokens.finish()?;

                Ok(var_decl)
            })
            .collect::<Result<_, _>>()?;

        let return_type = match kind_kw {
            // procedures return nothing
            keywords::Procedure |
            keywords::Destructor =>  None,

            _ => {
                //functions and constructors must return something
                tokens.match_one(tokens::Colon)?;
                let type_id = ParsedType::parse(tokens)?;

                Some(type_id)
            }
        };

        //body (if present) appears after separator or newline
        tokens.match_or_endl(tokens::Semicolon)?;

        let peek_after_sig = tokens.match_peek(keywords::Var.or(keywords::Begin))?;

        let body = match peek_after_sig {
            // forward decl
            None => None,

            // decl with body
            Some(body_kw) => {
                let local_vars = if body_kw.is_keyword(keywords::Var) {
                    VarDecls::parse(tokens)?
                } else {
                    VarDecls::default()
                };

                let block = Block::parse(tokens)?;

                // body is followed by semicolon or newline
                tokens.match_or_endl(tokens::Semicolon)?;

                Some(FunctionDeclBody {
                    block,
                    local_vars,
                })
            }
        };

        Ok(FunctionDecl {
            name: Identifier::from(fn_name.unwrap_identifier()),
            context: fn_name,
            return_type,
            args: VarDecls { decls: args },
            kind,
            body,
        })
    }
}

#[cfg(test)]
mod test {
    use tokenizer;
    use node::Identifier;
    use super::*;

    fn parse_func(src: &str) -> FunctionDecl {
        let tokens = tokenizer::tokenize("test", src).unwrap();

        FunctionDecl::parse(tokens, &source::test::empty_context())
            .unwrap()
            .value
    }

    #[test]
    fn parses_sig_with_empty_args() {
        let func = parse_func("function hello(): System.String; begin end;");

        assert_eq!(Identifier::from("hello"), func.name);
        assert_eq!(Some(ParsedType::with_name("System.String")), func.return_type);
        assert_eq!(0, func.args.decls.len());
    }

    #[test]
    fn parses_sig_without_args() {
        let func = parse_func("function hello: String; begin end;");

        assert_eq!(Identifier::from("hello"), func.name);
        assert_eq!(Some(ParsedType::with_name("String")), func.return_type);
        assert_eq!(0, func.args.decls.len());
    }

    #[test]
    fn parses_sig_with_args() {
        let func = parse_func("function hello(x: System.Float; y: Integer): String; begin end;");

        assert_eq!(Identifier::from("hello"), func.name);
        assert_eq!(Some(ParsedType::with_name("String")), func.return_type);
        assert_eq!(2, func.args.decls.len());

        assert_eq!(Identifier::from("x"), func.args.decls[0].name);
        assert_eq!(ParsedType::with_name("System.Float"), func.args.decls[0].decl_type);

        assert_eq!(Identifier::from("y"), func.args.decls[1].name);
        assert_eq!(ParsedType::with_name("Integer"), func.args.decls[1].decl_type);
    }
}
