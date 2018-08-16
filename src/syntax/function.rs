use syntax::*;
use syntax::var_decl::*;
use keywords;
use tokens;
use tokens::AsToken;
use node::{
    self,
    Identifier,
    FunctionKind,
    FunctionModifier,
    FunctionArgModifier,
};

pub type FunctionDecl = node::FunctionDecl<ParsedSymbol, ParsedContext>;
pub type FunctionDeclBody = node::FunctionDeclBody<ParsedSymbol, ParsedContext>;
pub type FunctionArg = node::FunctionArg<ParsedSymbol, ParsedContext>;

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

        let peek_after_sig = tokens.match_peek(keywords::Var
            .or(keywords::Const)
            .or(keywords::Begin))?;

        let body = match peek_after_sig {
            // forward decl
            None => None,

            // decl with body
            Some(_) => {
                /* parse consts and vars until we run out of those */
                let mut local_consts = ConstDecls::default();
                let mut local_vars = VarDecls::default();

                loop {
                    match tokens.peeked().match_one(keywords::Var.or(keywords::Const)) {
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

    pub fn parse_argument_list(tokens: &mut TokenStream) -> ParseResult<Vec<FunctionArg>> {
        if tokens.peeked().match_one(tokens::BracketLeft).is_none() {
            return Ok(Vec::new());
        }

        let arg_groups = tokens.match_groups(tokens::BracketLeft,
                                             tokens::BracketRight,
                                             tokens::Semicolon)?;

        let mut args = Vec::new();
        for arg_group in arg_groups.groups {
            let mut arg_group_tokens = TokenStream::new(arg_group.tokens, &arg_group.context);

            /* the modifier comes first if there is one, and applies to all args in this group - e.g.
            `const x, y: Integer` means x and y are consts of type Integer */
            let modifier = arg_group_tokens.peeked().match_one(keywords::Var
                .or(keywords::Const)
                .or(keywords::Out))
                .map(|t| match t.unwrap_keyword() {
                    keywords::Var => FunctionArgModifier::Var,
                    keywords::Const => FunctionArgModifier::Const,
                    keywords::Out => FunctionArgModifier::Out,
                    _ => unreachable!()
                });
            if modifier.is_some() {
                arg_group_tokens.advance(1);
            }

            /* the actual arg group (names, colon, type) works the same as a var decl so we can
            just parse it like that */
            let group_args: Vec<VarDecl> = arg_group_tokens.parse()?;
            for group_arg in group_args {
                args.push(FunctionArg {
                    context: group_arg.context,
                    decl_type: group_arg.decl_type,
                    name: group_arg.name,
                    modifier,
                })
            }

            arg_group_tokens.finish()?;
        }

        Ok(args)
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
