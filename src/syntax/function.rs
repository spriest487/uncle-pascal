use syntax::*;
use syntax::var_decl::*;
use keywords;
use tokens;
use tokens::AsToken;
use node;
use source;

pub type Function = node::FunctionDecl<node::Identifier>;

impl Function {
    pub fn parse<TIter>(in_tokens: TIter, context: &TIter::Item) -> ParseResult<Self>
        where TIter: IntoIterator<Item=source::Token> + 'static
    {
        //match the name
        let name_match = keywords::Function.or(keywords::Procedure)
            .and_then(Matcher::AnyIdentifier)
            .match_sequence(in_tokens, context)?;

        let func_or_proc = name_match.value[0].clone();
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
                let fn_arg = Matcher::AnyIdentifier
                    .and_then(tokens::Colon)
                    .and_then(Matcher::AnyIdentifier)
                    .match_sequence(arg_tokens.tokens, &arg_tokens.context)?;

                let name = String::from(fn_arg.value[0].unwrap_identifier());
                let decl_type = node::Identifier::parse(fn_arg.value[2].unwrap_identifier());

                Ok(VarDecl {
                    name,
                    context: fn_arg.value[0].clone(),
                    decl_type,
                })
            })
            .collect::<Result<_, _>>()?;

        let return_type = if func_or_proc.is_keyword(keywords::Function) {
            let match_return = tokens::Colon
                .and_then(Matcher::AnyIdentifier)
                .match_sequence(arg_groups.next_tokens, &arg_groups.last_token)?;

            let type_id = node::Identifier::parse(match_return.value[1].unwrap_identifier());

            ParseOutput::new(Some(type_id), match_return.last_token, match_return.next_tokens)
        } else {
            ParseOutput::new(None, arg_groups.last_token, arg_groups.next_tokens)
        };

        let semicolon_after_sig = tokens::Semicolon.match_one(return_type.next_tokens,
                                                              &return_type.last_token)?;

        let peek_after_sig = keywords::Var.or(keywords::Begin)
            .match_peek(semicolon_after_sig.next_tokens,
                        &semicolon_after_sig.last_token)?;

        let local_vars = match peek_after_sig.value {
            Some(ref var_kw) if var_kw.is_keyword(keywords::Var) => {
                VarDecls::parse(peek_after_sig.next_tokens, &peek_after_sig.last_token)?
            }
            _ => {
                ParseOutput::new(VarDecls::default(),
                                 peek_after_sig.last_token,
                                 peek_after_sig.next_tokens)
            }
        };

        let body_block = Block::parse(local_vars.next_tokens,
                                      &local_vars.last_token)?;

        let last_semicolon = tokens::Semicolon.match_one(body_block.next_tokens,
                                                         &body_block.last_token)?;

        let function = Function {
            name: fn_name.unwrap_identifier().to_owned(),
            context: fn_name,
            return_type: return_type.value,

            local_vars: local_vars.value,
            args: VarDecls { decls: args },

            body: body_block.value,
        };

        Ok(ParseOutput::new(function,
                            last_semicolon.last_token,
                            last_semicolon.next_tokens))
    }
}

impl node::ToSource for Function {
    fn to_source(&self) -> String {
        let mut lines = Vec::new();
        lines.push(format!("function {};", self.name));

        if self.local_vars.decls.len() > 0 {
            lines.push(self.local_vars.to_source());
        }

        lines.push(self.body.to_source() + ";");

        lines.join("\n")
    }
}

#[cfg(test)]
mod test {
    use tokenizer;
    use super::*;

    fn parse_func(src: &str) -> Function {
        let tokens = tokenizer::tokenize("test", src).unwrap();

        let result = Function::parse(tokens, &source::test::empty_context());
        assert!(result.is_ok());

        result.unwrap().value
    }

    #[test]
    fn parses_sig_with_empty_args() {
        let func = parse_func("function hello(): String; begin end;");

        assert_eq!("hello", func.name);
        assert_eq!(Some(node::Identifier::parse("String")), func.return_type);
        assert_eq!(0, func.args.decls.len());
    }

    #[test]
    fn parses_sig_without_args() {
        let func = parse_func("function hello: String; begin end;");

        assert_eq!("hello", func.name);
        assert_eq!(Some(node::Identifier::parse("String")), func.return_type);
        assert_eq!(0, func.args.decls.len());
    }

    #[test]
    fn parses_sig_with_args() {
        let func = parse_func("function hello(x: System.Float; y: Integer): String; begin end;");

        assert_eq!("hello", func.name);
        assert_eq!(Some(node::Identifier::parse("String")), func.return_type);
        assert_eq!(2, func.args.decls.len());

        assert_eq!("x", func.args.decls[0].name);
        assert_eq!(node::Identifier::parse("System.Float"), func.args.decls[0].decl_type);

        assert_eq!("y", func.args.decls[1].name);
        assert_eq!(node::Identifier::parse("Integer"), func.args.decls[1].decl_type);
    }
}
