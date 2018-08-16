use syntax::*;
use syntax::var_decl::*;
use keywords;
use tokens;
use tokens::AsToken;
use node::{
    self,
    Identifier,
    FunctionKind,
    FunctionModifier
};

pub type FunctionDecl = node::FunctionDecl<ParsedSymbol, ParsedContext>;
pub type FunctionDeclBody = node::FunctionDeclBody<ParsedSymbol, ParsedContext>;

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
            let next_mod_tokens = tokens.match_sequence_peek(tokens::Semicolon
                .and_then(Matcher::AnyIdentifier))?;

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

    pub fn parse_argument_list(tokens: &mut TokenStream) -> ParseResult<VarDecls> {
        match tokens.match_block_peek(tokens::BracketLeft, tokens::BracketRight)? {
            Some(args_block) => {
                let arg_tokens_len = args_block.len();

                let mut args_tokens = TokenStream::new(args_block.inner, &args_block.open);
                let args: Vec<VarDecl> = args_tokens.parse()?;
                
                args_tokens.finish()?;

                tokens.advance(arg_tokens_len);

                Ok(VarDecls { decls: args })
            }

            None => Ok(VarDecls::default()),
        }
    }
}

#[cfg(test)]
mod test {
    use tokenizer;
    use node::Identifier;
    use super::*;

    fn parse_func(src: &str) -> FunctionDecl {
        let tokens = tokenizer::tokenize("test", src).unwrap();

        FunctionDecl::parse(&mut TokenStream::from(tokens))
            .unwrap()
    }

    #[test]
    fn parses_sig_with_empty_args() {
        let func = parse_func("function hello(): System.String; begin end;");

        assert_eq!(Identifier::from("hello"), func.name);
        assert_eq!(Some(TypeName::with_name("System.String")), func.return_type);
        assert_eq!(0, func.args.decls.len());
    }

    #[test]
    fn parses_sig_without_args() {
        let func = parse_func("function hello: String; begin end;");

        assert_eq!(Identifier::from("hello"), func.name);
        assert_eq!(Some(TypeName::with_name("String")), func.return_type);
        assert_eq!(0, func.args.decls.len());
    }

    #[test]
    fn parses_sig_with_args() {
        let func = parse_func("function hello(x: System.Float; y: Integer): String; begin end;");

        assert_eq!(Identifier::from("hello"), func.name);
        assert_eq!(Some(TypeName::with_name("String")), func.return_type);
        assert_eq!(2, func.args.decls.len());

        assert_eq!(Identifier::from("x"), func.args.decls[0].name);
        assert_eq!(TypeName::with_name("System.Float"), func.args.decls[0].decl_type);

        assert_eq!(Identifier::from("y"), func.args.decls[1].name);
        assert_eq!(TypeName::with_name("Integer"), func.args.decls[1].decl_type);
    }
}
