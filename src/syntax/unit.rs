use node;
use syntax::*;
use keywords;
use tokens;
use operators;
use tokens::AsToken;

pub type Unit = node::Unit<ParsedSymbol>;
pub type UnitDeclaration = node::UnitDeclaration<ParsedSymbol>;

impl Unit {
    pub fn parse(mut tokens: TokenStream) -> ParseResult<Unit> {
        let match_name = tokens.match_sequence(keywords::Unit.and_then(Matcher::AnyIdentifier))?;
        tokens.match_or_endl(tokens::Semicolon)?;

        let uses = Unit::parse_uses(&mut tokens)?;

        let interface_kw = tokens.match_one(keywords::Interface)?;
        let interface_decls = Unit::parse_decls(&mut tokens)?;

        let impl_kw = tokens.match_one(keywords::Implementation)?;
        let impl_decls = Unit::parse_decls(&mut tokens)?;

        tokens.match_sequence(keywords::End.and_then(tokens::Period))?;
        tokens.finish()?;

        Ok(Unit {
            name: match_name[1].unwrap_identifier().to_owned(),
            uses,

            interface: interface_decls,
            implementation: impl_decls,
        })
    }

    pub fn parse_uses(tokens: &mut TokenStream)-> ParseResult<Vec<node::UnitReference>> {
        let uses_kw = tokens.match_peek(keywords::Uses)?;

        if uses_kw.is_none() {
            //no uses
            return Ok(Vec::new());
        }

        let mut units = Vec::new();
        loop {
            let unit_id = tokens.match_one(Matcher::AnyIdentifier)?;
            let unit_context = tokens.context().clone();

            // might have a . if a non-default uses mode is specified
            // e.g. System.*
            let peek_uses_kind = tokens.match_peek(tokens::Period)?;

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
                context: unit_context,
                kind: uses_kind,
            });

            match tokens.peek() {
                //end of uses (either unexpected token on new line, or explicit semicolon)
                Some(ref t) if (t.is_token(&tokens::Semicolon) ||
                    t.location.line > tokens.context().location.line) => {

                    //skip the semicolon if there was one
                    if t.is_token(&tokens::Semicolon) {
                        tokens.next();
                    }

                    break Ok(units);
                }

                Some(ref comma) if comma.is_token(&tokens::Comma) => {
                    //continue looking for unit names after this comma in next iter
                    tokens.next();
                }

                Some(unexpected) => {
                    let expected = Some(Matcher::from(tokens::Comma));
                    let err = ParseError::UnexpectedToken(unexpected, expected);

                    break Err(err);
                }

                None => {
                    let context = tokens.context().clone();
                    break Err(ParseError::UnexpectedEOF(tokens::Comma.into(), context));
                }
            }
        }
    }

    pub fn parse_decls(tokens: &mut TokenStream) -> ParseResult<Vec<UnitDeclaration>> {
        let mut decls = Vec::new();

        loop {
            let match_decl_first = FunctionDecl::match_any_function_keyword()
                .or(keywords::Type)
                .or(keywords::Var)
                .or(keywords::Begin);

            let peek_decl = tokens.match_peek(match_decl_first)?;

            match peek_decl {
                Some(ref func_kw) if FunctionDecl::match_any_function_keyword().is_match(func_kw)
                => {
                    let func = FunctionDecl::parse(tokens)?;
                    decls.push(node::UnitDeclaration::Function(func));
                }

                Some(ref type_kw) if type_kw.is_keyword(keywords::Type) => {
                    let type_decls = TypeDecl::parse(tokens)?;
                    decls.extend(type_decls.into_iter().map(|type_decl| {
                        node::UnitDeclaration::Type(type_decl)
                    }));
                }

                Some(ref var_kw) if var_kw.is_keyword(keywords::Var) => {
                    let vars = VarDecls::parse(tokens)?;
                    decls.push(node::UnitDeclaration::Vars(vars));
                }

                _ => break Ok(decls),
            }
        }
    }
}
