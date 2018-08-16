use syntax::*;
use syntax::var_decl::*;
use keywords;
use tokens;
use tokens::AsToken;
use node;
use ToSource;

pub type Function = node::Function<node::Identifier>;

impl Function {
    pub fn parse<TIter>(in_tokens: TIter, context: &TIter::Item) -> ParseResult<Self, TIter::Item>
        where TIter: IntoIterator + 'static,
              TIter::Item: tokens::AsToken + 'static
    {
        //match the name
        let name_match = Matcher::Keyword(keywords::Function)
            .and_then(Matcher::AnyIdentifier)
            .match_sequence(in_tokens, context)?;

        let fn_name = &name_match.value[1].clone();
        let open_args = Matcher::Exact(tokens::BracketLeft)
            .match_peek(name_match.next_tokens, &name_match.last_token)?;

        let arg_groups = match open_args.value {
            Some(_) => {
                Matcher::Exact(tokens::BracketLeft)
                    .terminated_by(Matcher::Exact(tokens::BracketRight))
                    .match_groups(&Matcher::Exact(tokens::Comma),
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
                    .and_then(Matcher::Exact(tokens::Colon))
                    .and_then(Matcher::AnyIdentifier)
                    .match_sequence(arg_tokens.items, &arg_tokens.context)?;

                let name = String::from(fn_arg.value[0].unwrap_identifier());
                let decl_type = node::Identifier::parse(fn_arg.value[2].unwrap_identifier());

                Ok(VarDecl { name, decl_type })
            })
            .collect::<Result<_, _>>()?;

        let match_return = Matcher::Exact(tokens::Colon)
            .and_then(Matcher::AnyIdentifier)
            .and_then(Matcher::Exact(tokens::Semicolon))
            .match_sequence(arg_groups.next_tokens, &arg_groups.last_token)?;

        let fn_return_type = &match_return.value[1];

        let peek_after_sig = Matcher::Keyword(keywords::Var)
            .or(Matcher::Keyword(keywords::Begin))
            .match_peek(match_return.next_tokens, &match_return.last_token)?;

        let local_vars = match peek_after_sig.value {
            Some(ref var_kw) if var_kw.as_token().is_keyword(keywords::Var) => {
                Vars::parse(peek_after_sig.next_tokens, &peek_after_sig.last_token)?
            }
            _ => {
                ParseOutput::new(Vars::default(),
                                 peek_after_sig.last_token,
                                 peek_after_sig.next_tokens)
            }
        };

        let body_block = Block::parse(local_vars.next_tokens, &local_vars.last_token)?;

        let match_semicolon = Matcher::Exact(tokens::Semicolon);
        let last_semicolon = match_semicolon
            .match_one(body_block.next_tokens, &body_block.last_token)?;

        let function = Function {
            name: fn_name.as_token().unwrap_identifier().to_owned(),
            return_type: node::Identifier::parse(fn_return_type.as_token().unwrap_identifier()),

            local_vars: local_vars.value,
            args: Vars { decls: args }, //TODO: steal vars parsing and make a match_list for ; vs , arg list

            body: body_block.value,
        };

        Ok(ParseOutput::new(function,
                            last_semicolon.last_token,
                            last_semicolon.next_tokens))
    }
}

impl ToSource for Function {
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

    static NO_CONTEXT : tokenizer::SourceToken = tokenizer::SourceToken {
        token: tokens::Keyword(keywords::Program),
        line:0,
        col: 0,
    };

    fn parse_func(src: &str) -> Function {
        let tokens = tokenizer::tokenize(src).unwrap();

        let result = Function::parse(tokens, &NO_CONTEXT);
        assert!(result.is_ok());

        result.unwrap().value
    }

    #[test]
    fn parses_sig_with_empty_args() {
        let func = parse_func("function hello(): String; begin end;");

        assert_eq!("hello", func.name);
        assert_eq!(node::Identifier::parse("String"), func.return_type);
        assert_eq!(0, func.args.decls.len());
    }

    #[test]
    fn parses_sig_without_args() {
        let func = parse_func("function hello: String; begin end;");

        assert_eq!("hello", func.name);
        assert_eq!(node::Identifier::parse("String"), func.return_type);
        assert_eq!(0, func.args.decls.len());
    }

    #[test]
    fn parses_sig_with_args() {
        let func = parse_func("function hello(x: System.Float, y: Integer): String; begin end;");

        assert_eq!("hello", func.name);
        assert_eq!(node::Identifier::parse("String"), func.return_type);
        assert_eq!(2, func.args.decls.len());

        assert_eq!("x", func.args.decls[0].name);
        assert_eq!(node::Identifier::parse("System.Float"), func.args.decls[0].decl_type);

        assert_eq!("y", func.args.decls[1].name);
        assert_eq!(node::Identifier::parse("Integer"), func.args.decls[1].decl_type);
    }
}
