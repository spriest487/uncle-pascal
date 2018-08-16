use source;
use node;
use syntax::*;
use keywords;
use tokens;
use tokens::AsToken;

pub type Unit = node::Unit<ParsedSymbol>;
pub type UnitDeclaration = node::UnitDeclaration<ParsedSymbol>;

impl Unit {
    pub fn parse<TIter>(tokens: TIter, context: &source::Token) -> Result<Unit, ParseError>
        where TIter: IntoIterator<Item=source::Token> + 'static
    {
        let match_name = keywords::Unit.and_then(Matcher::AnyIdentifier)
            .match_sequence(tokens, context)?;

        let after_name = tokens::Semicolon.match_or_endl(match_name.next_tokens,
                                                         &match_name.last_token)?;

        let uses = Unit::parse_uses(after_name.next_tokens, &after_name.last_token)?;

        let interface_kw = keywords::Interface.match_one(uses.next_tokens,
                                                         &uses.last_token)?;

        let interface_decls = Unit::parse_decls(interface_kw.next_tokens,
                                                &interface_kw.last_token)?;

        let impl_kw = keywords::Implementation.match_one(interface_decls.next_tokens,
                                                         &interface_decls.last_token)?;

        let impl_decls = Unit::parse_decls(impl_kw.next_tokens, &impl_kw.last_token)?;

        let _unit_end = keywords::End.and_then(tokens::Period)
            .match_sequence(impl_decls.next_tokens, &impl_decls.last_token)?;

        Ok(Unit {
            name: match_name.value[1].unwrap_identifier().to_owned(),
            uses: uses.value,

            interface: interface_decls.value,
            implementation: impl_decls.value,
        })
    }

    pub fn parse_uses<TIter>(in_tokens: TIter,
                         context: &source::Token)
                         -> ParseResult<Vec<node::UnitReference>>
        where TIter: IntoIterator<Item=source::Token> + 'static
    {
        let uses_kw = keywords::Uses.match_peek(in_tokens, context)?;

        if uses_kw.value.is_none() {
            //no uses
            return Ok(ParseOutput::new(Vec::new(),
                                       context.clone(),
                                       uses_kw.next_tokens));
        }

        let mut units = Vec::new();
        let mut units_tokens = WrapIter::new(uses_kw.next_tokens.skip(1));
        let mut units_context = uses_kw.last_token;
        loop {
            let unit_id = Matcher::AnyIdentifier.match_one(units_tokens, &units_context)?;

            units.push(node::UnitReference {
                name: node::Identifier::from(unit_id.value.unwrap_identifier()),
                context: unit_id.last_token.clone(),
            });

            units_context = unit_id.last_token;

            let mut peek_after_unit = unit_id.next_tokens.peekable();
            match peek_after_unit.peek().cloned() {
                //end of uses (either unexpected token on new line, or explicit semicolon)
                Some(ref t) if (t.is_token(&tokens::Semicolon) ||
                    t.location.line > units_context.location.line) => {

                    //skip the semicolon if there was one
                    let skip = if t.is_token(&tokens::Semicolon) { 1 } else { 0 };
                    units_tokens = WrapIter::new(peek_after_unit.skip(skip));
                    units_context = t.clone();
                    break;
                }

                Some(ref comma) if comma.is_token(&tokens::Comma) => {
                    //continue looking for unit names after this comma in next iter
                    units_tokens = WrapIter::new(peek_after_unit.skip(1));
                    units_context = comma.clone();
                }

                Some(unexpected) => {
                    let err = ParseError::UnexpectedToken(unexpected,
                                                          Some(Matcher::from(tokens::Comma)));
                    return Err(err);
                }

                None => {
                    return Err(ParseError::UnexpectedEOF(tokens::Comma.into(), units_context));
                }
            }
        }

        Ok(ParseOutput::new(units, units_context, units_tokens))
    }

    pub fn parse_decls<TIter>(in_tokens: TIter,
                              context: &source::Token)
                              -> ParseResult<Vec<UnitDeclaration>>
        where TIter: IntoIterator<Item=source::Token> + 'static
    {
        let mut tokens: Box<Iterator<Item=source::Token>> = Box::from(in_tokens.into_iter());

        let mut decls = Vec::new();

        let mut last_parsed = context.clone();

        loop {
            let match_decl_first = FunctionDecl::match_any_function_keyword()
                .or(keywords::Type)
                .or(keywords::Var)
                .or(keywords::Begin);

            let peek_decl = match_decl_first.match_peek(tokens, &last_parsed)?;
            tokens = Box::from(peek_decl.next_tokens);

            match peek_decl.value {
                Some(ref func_kw) if FunctionDecl::match_any_function_keyword().is_match(func_kw)
                => {
                    let func = FunctionDecl::parse(tokens, &peek_decl.last_token)?;
                    decls.push(node::UnitDeclaration::Function(func.value));

                    tokens = Box::from(func.next_tokens);
                    last_parsed = func.last_token;
                }

                Some(ref type_kw) if type_kw.is_keyword(keywords::Type) => {
                    let record = RecordDecl::parse(tokens, &peek_decl.last_token)?;
                    decls.push(node::UnitDeclaration::Record(record.value));

                    tokens = Box::from(record.next_tokens);
                    last_parsed = record.last_token;
                }

                Some(ref var_kw) if var_kw.is_keyword(keywords::Var) => {
                    let vars = VarDecls::parse(tokens, &peek_decl.last_token)?;
                    decls.push(node::UnitDeclaration::Vars(vars.value));

                    tokens = Box::from(vars.next_tokens);
                    last_parsed = vars.last_token;
                }

                _ => break,
            }
        }

        Ok(ParseOutput::new(decls, last_parsed, tokens))
    }
}
