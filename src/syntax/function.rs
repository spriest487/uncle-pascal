use syntax::*;
use syntax::var_decl::*;
use keywords;
use tokens;
use tokens::AsToken;
use node::{self, Identifier, FunctionKind};
use source;

pub type FunctionDecl = node::FunctionDecl<ParsedSymbol>;
pub type FunctionDeclBody = node::FunctionDeclBody<ParsedSymbol>;

impl FunctionDecl {
    pub fn match_any_function_keyword() -> matcher::Matcher {
        keywords::Function
            .or(keywords::Procedure)
            .or(keywords::Constructor)
            .or(keywords::Destructor)
    }

    pub fn parse<TIter>(in_tokens: TIter, context: &TIter::Item) -> ParseResult<Self>
        where TIter: IntoIterator<Item=source::Token> + 'static
    {
        //match the name
        let name_match = Self::match_any_function_keyword()
            .and_then(Matcher::AnyIdentifier)
            .match_sequence(in_tokens, context)?;

        let kind_kw = name_match.value[0].unwrap_keyword();
        let kind = match kind_kw {
            keywords::Constructor => FunctionKind::Constructor,
            keywords::Destructor => FunctionKind::Destructor,
            _ => FunctionKind::Function,
        };

        let fn_name = name_match.value[1].clone();
        let open_args = tokens::BracketLeft.match_peek(name_match.next_tokens,
                                                       &name_match.last_token)?;

        let arg_groups = match open_args.value {
            Some(_) => {
                tokens::BracketLeft.terminated_by(tokens::BracketRight)
                    .match_groups(tokens::Semicolon,
                                  open_args.next_tokens,
                                  &open_args.last_token)?
                    .map(|groups_match| groups_match.groups)
            }
            None => {
                ParseOutput::new(Vec::new(),
                                 open_args.last_token.clone(),
                                 open_args.next_tokens)
            }
        };

        let args = arg_groups.value.into_iter()
            .map(|arg_tokens| {
                VarDecl::parse(arg_tokens.tokens, &arg_tokens.context)?
                    .finish()
            })
            .collect::<Result<_, _>>()?;

        let return_type = match kind_kw {
            keywords::Procedure |
            keywords::Destructor => {
                // procedures return nothing
                ParseOutput::new(None, arg_groups.last_token, arg_groups.next_tokens)
            }
            _ => {
                //functions and constructors must return something
                let colon = tokens::Colon.match_one(arg_groups.next_tokens,
                                                    &arg_groups.last_token)?;

                let type_id = ParsedType::parse(colon.next_tokens, &colon.last_token)?;

                ParseOutput::new(Some(type_id.value), type_id.last_token, type_id.next_tokens)
            }
        };

        let semicolon_after_sig = tokens::Semicolon.match_or_endl(return_type.next_tokens,
                                                                  &return_type.last_token)?;

        let peek_after_sig = keywords::Var.or(keywords::Begin)
            .match_peek(semicolon_after_sig.next_tokens,
                        &semicolon_after_sig.last_token)?;

        let body = match peek_after_sig.value {
            // forward decl
            None => ParseOutput::new(None,
                                     peek_after_sig.last_token,
                                     peek_after_sig.next_tokens),

            // decl with body
            Some(body_kw) => {
                let local_vars = if body_kw.is_keyword(keywords::Var) {
                    VarDecls::parse(peek_after_sig.next_tokens, &peek_after_sig.last_token)?
                } else {
                    ParseOutput::new(VarDecls::default(),
                                     peek_after_sig.last_token,
                                     peek_after_sig.next_tokens)
                };

                let block = Block::parse(local_vars.next_tokens, &local_vars.last_token)?;

                let body = FunctionDeclBody {
                    block: block.value,
                    local_vars: local_vars.value,
                };

                ParseOutput::new(Some(body), block.last_token, block.next_tokens)
            }
        };

        let last_semicolon = tokens::Semicolon.match_or_endl(body.next_tokens,
                                                             &body.last_token)?;

        let function = FunctionDecl {
            name: Identifier::from(fn_name.unwrap_identifier()),
            context: fn_name,
            return_type: return_type.value,
            args: VarDecls { decls: args },
            kind,
            body: body.value,
        };

        Ok(ParseOutput::new(function,
                            last_semicolon.last_token,
                            last_semicolon.next_tokens))
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
