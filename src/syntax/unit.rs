use node;
use syntax::*;
use keywords;
use tokens;
use operators;
use tokens::AsToken;

pub type Unit = node::Unit<ParsedContext>;
pub type UnitDeclaration = node::UnitDeclaration<ParsedContext>;
pub type UnitReference = node::UnitReference<ParsedContext>;

impl Unit {
    pub fn parse(mut tokens: TokenStream) -> ParseResult<Self> {
        let match_name = tokens.match_sequence(keywords::Unit.and_then(Matcher::AnyIdentifier))?;
        tokens.match_or_endl(tokens::Semicolon)?;

        tokens.match_one(keywords::Interface)?;

        let uses: Vec<UnitReference> = tokens.parse()?;

        let interface_decls: Vec<UnitDeclaration> = tokens.parse()?;

        tokens.match_one(keywords::Implementation)?;
        let impl_decls: Vec<UnitDeclaration> = tokens.parse()?;

        tokens.match_sequence(keywords::End.and_then(tokens::Period))?;
        tokens.finish()?;

        Ok(Unit {
            name: match_name[1].unwrap_identifier().to_owned(),
            uses,

            interface: interface_decls,
            implementation: impl_decls,
        })
    }
}

impl Parse for Vec<UnitReference> {
    fn parse(tokens: &mut TokenStream) -> ParseResult<Self> {
        let uses_kw = tokens.look_ahead().match_one(keywords::Uses);

        if uses_kw.is_none() {
            //no uses
            return Ok(Vec::new());
        }
        tokens.advance(1);

        let mut units = Vec::new();
        loop {
            let unit_id = tokens.match_one(Matcher::AnyIdentifier)?;
            let unit_context = tokens.context().clone();

            // might have a . if a non-default uses mode is specified
            // e.g. System.*
            let peek_uses_kind = tokens.look_ahead().match_one(tokens::Period);

            let uses_kind = match peek_uses_kind {
                Some(_) => {
                    let matched_imported_name = tokens.match_sequence(
                        tokens::Period.and_then(operators::Multiply.or(Matcher::AnyIdentifier))
                    )?;

                    match matched_imported_name[1].as_token() {
                        tokens::Operator(operators::Multiply) =>
                            node::UnitReferenceKind::All,

                        tokens::Identifier(name) =>
                            node::UnitReferenceKind::Name(name.to_string()),

                        _ => unreachable!("excluded by token matcher")
                    }
                }

                None => node::UnitReferenceKind::Namespaced
            };

            units.push(node::UnitReference {
                name: node::Identifier::from(unit_id.unwrap_identifier()),
                context: unit_context.into(),
                kind: uses_kind,
            });

            match tokens.look_ahead().next() {
                // end of uses (explicit semicolon)
                Some(ref t) if t.is_token(&tokens::Semicolon) => {
                    tokens.advance(1);
                    break Ok(units);
                }

                // list continues (after an explicit semicolon)
                Some(ref comma) if comma.is_token(&tokens::Comma) => {
                    tokens.advance(1);
                }

                // another name without a comma, but it's on a new line so that's fine
                Some(ref t) if t.is_any_identifier()
                    && t.location.line > tokens.context().location.line => {
                }

                // list ends (EOF or unexpected token but it's on a new line,
                // so we just infer the end of the list)
                None => {
                    break Ok(units)
                },
                Some(ref t) if t.location.line > tokens.context().location.line => {
                    break Ok(units);
                }

                // unexpect tokens on the same line, this is illegal
                Some(unexpected) => {
                    let expected = Some(Matcher::from(tokens::Comma));
                    let err = ParseError::UnexpectedToken(unexpected, expected);

                    break Err(err);
                }
            }
        }
    }
}

impl Parse for Vec<UnitDeclaration> {
    fn parse(tokens: &mut TokenStream) -> ParseResult<Self> {
        let mut decls = Vec::new();

        loop {
            let match_decl_first = FunctionDecl::match_any_function_keyword()
                .or(keywords::Type)
                .or(keywords::Var)
                .or(keywords::Const)
                .or(keywords::Begin);

            let peek_decl = tokens.look_ahead().match_one(match_decl_first);

            match peek_decl {
                Some(ref func_kw) if FunctionDecl::match_any_function_keyword().is_match(func_kw)
                => {
                    let func: FunctionDecl = tokens.parse()?;
                    decls.push(node::UnitDeclaration::Function(func));
                }

                Some(ref type_kw) if type_kw.is_keyword(keywords::Type) => {
                    let type_decls: Vec<TypeDecl> = tokens.parse()?;
                    decls.extend(type_decls.into_iter().map(|type_decl| {
                        node::UnitDeclaration::Type(type_decl)
                    }));
                }

                Some(ref var_kw) if var_kw.is_keyword(keywords::Var) => {
                    let vars = VarDecls::parse(tokens)?;
                    decls.push(node::UnitDeclaration::Vars(vars));
                }

                Some(ref const_kw) if const_kw.is_keyword(keywords::Const) => {
                    let consts = ConstDecls::parse(tokens)?;
                    decls.push(node::UnitDeclaration::Consts(consts));
                }

                _ => break Ok(decls),
            }
        }
    }
}
