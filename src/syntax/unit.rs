use node;
use syntax::*;
use keywords;
use tokens;
use operators;
use tokens::AsToken;

pub type Unit = node::Unit<ParsedContext>;
pub type UnitDecl = node::UnitDecl<ParsedContext>;
pub type Implementation = node::Implementation<ParsedContext>;
pub type UnitReference = node::UnitReference<ParsedContext>;

impl Unit {
    fn parse_optional_block(tokens: &mut TokenStream,
                            start: impl Into<Matcher>,
                            end: impl Into<Matcher>) -> ParseResult<Option<Block>> {
        let start = start.into();
        let end = end.into();

        match tokens.look_ahead().match_one(start.clone()) {
            Some(_) => {
                let context = tokens.match_one(start)?;
                let statements = Block::parse_statements(tokens, end)?;

                Ok(Some(Block {
                    context: context.into(),
                    statements,
                }))
            }

            None => Ok(None),
        }
    }

    pub fn parse(mut tokens: TokenStream) -> ParseResult<Self> {
        let match_name = tokens.match_sequence(keywords::Unit.and_then(Matcher::AnyIdentifier))?;
        tokens.match_or_endl(tokens::Semicolon)?;

        tokens.match_one(keywords::Interface)?;

        let uses: Vec<UnitReference> = tokens.parse()?;

        let interface_decls: Vec<UnitDecl> = tokens.parse()?;

        tokens.match_one(keywords::Implementation)?;
        let impl_decls: Vec<Implementation> = tokens.parse()?;

        let initialization = Self::parse_optional_block(
            &mut tokens, keywords::Initialization, keywords::End.or(keywords::Finalization))?;
        let finalization = Self::parse_optional_block(
            &mut tokens, keywords::Finalization, keywords::End)?;

        tokens.match_sequence(keywords::End.and_then(tokens::Period))?;
        tokens.finish()?;

        Ok(Unit {
            name: match_name[1].unwrap_identifier().to_owned(),
            uses,

            interface: interface_decls,
            implementation: impl_decls,

            initialization,
            finalization,
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
                    && t.location.line > tokens.context().location.line => {}

                // list ends (EOF or unexpected token but it's on a new line,
                // so we just infer the end of the list)
                None => {
                    break Ok(units);
                }
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

fn decl_first_matcher() -> Matcher {
    FunctionDecl::match_any_function_keyword()
        .or(keywords::Type)
        .or(keywords::Var)
        .or(keywords::Const)
}

impl Parse for Vec<Implementation> {
    fn parse(tokens: &mut TokenStream) -> ParseResult<Self> {
        let mut impls = Vec::new();
        loop {
            match tokens.look_ahead().match_one(decl_first_matcher()) {
                Some(ref kw) if FunctionDecl::match_any_function_keyword().is_match(kw) => {
                    let func_definition: Function = tokens.parse()?;

                    impls.push(node::Implementation::Function(func_definition))
                }

                Some(_) => {
                    impls.extend(UnitDecl::parse_one(tokens)?
                        .into_iter()
                        .map(|decl| node::Implementation::Decl(decl)))
                }

                None => break,
            }
        }

        Ok(impls)
    }
}

impl UnitDecl {
    /* parse one decl section. this still returns a Vec of decls because of the way
        the nodes are currently structured i.e. we create one TypeDecl for each type
        in a type... section  */
    fn parse_one(tokens: &mut TokenStream) -> ParseResult<Vec<Self>> {
        let peek_decl = tokens.look_ahead().match_one(decl_first_matcher());

        match peek_decl {
            Some(ref func_kw) if FunctionDecl::match_any_function_keyword().is_match(func_kw)
            => {
                let func: FunctionDecl = tokens.parse()?;
                tokens.match_or_endl(tokens::Semicolon)?;

                Ok(vec![node::UnitDecl::Function(func)])
            }

            Some(ref type_kw) if type_kw.is_keyword(keywords::Type) => {
                let type_decls: Vec<TypeDecl> = tokens.parse()?;

                Ok(type_decls.into_iter()
                    .map(|type_decl| {
                        node::UnitDecl::Type(type_decl)
                    })
                    .collect())
            }

            Some(ref var_kw) if var_kw.is_keyword(keywords::Var) => {
                let vars = VarDecl::parse_var_section(tokens)?;
                Ok(vars.into_iter()
                    .map(|var| node::UnitDecl::Var(var))
                    .collect())
            }

            Some(ref const_kw) if const_kw.is_keyword(keywords::Const) => {
                let consts = ConstDecl::parse_const_section(tokens)?;
                Ok(consts.into_iter()
                    .map(|const_decl| node::UnitDecl::Const(const_decl))
                    .collect())
            }

            Some(unexpected) => {
                let expected = decl_first_matcher();
                Err(ParseError::UnexpectedToken(unexpected, Some(expected)))
            }

            None => {
                Err(ParseError::UnexpectedEOF(decl_first_matcher(), tokens.context().clone()))
            }
        }
    }
}

impl Parse for Vec<UnitDecl> {
    fn parse(tokens: &mut TokenStream) -> ParseResult<Self> {
        let mut decls = Vec::new();

        loop {
            match tokens.look_ahead().match_one(decl_first_matcher()) {
                Some(_) => decls.extend(UnitDecl::parse_one(tokens)?),
                None => break,
            }
        }

        Ok(decls)
    }
}
