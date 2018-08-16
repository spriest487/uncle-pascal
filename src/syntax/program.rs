use syntax::*;
use source;
use tokens;
use tokens::AsToken;
use keywords;
use node;

pub type Program = node::Program<node::Identifier>;
pub type UnitDeclaration = node::UnitDeclaration<node::Identifier>;

fn parse_uses<TIter>(in_tokens: TIter,
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
                return Err(ParseError::UnexpectedEOF(tokens::Comma.into(), units_context))
            },
        }
    }

    Ok(ParseOutput::new(units, units_context, units_tokens))
}

fn parse_decls<TIter>(in_tokens: TIter,
                      context: &source::Token)
                      -> ParseResult<Vec<UnitDeclaration>>
    where TIter: IntoIterator<Item=source::Token> + 'static
{
    let mut tokens: Box<Iterator<Item=source::Token>> = Box::from(in_tokens.into_iter());

    let mut decls = Vec::new();

    let mut last_parsed = context.clone();

    loop {
        let match_decl_first = keywords::Function
            .or(keywords::Procedure)
            .or(keywords::Type)
            .or(keywords::Var)
            .or(keywords::Begin);

        let peek_decl = match_decl_first.match_peek(tokens, &last_parsed)?;
        tokens = Box::from(peek_decl.next_tokens);

        match peek_decl.value {
            Some(ref func_kw) if func_kw.is_keyword(keywords::Function) ||
                func_kw.is_keyword(keywords::Procedure) => {
                let func = Function::parse(tokens, &peek_decl.last_token)?;
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

            Some(ref begin_kw) if begin_kw.is_keyword(keywords::Begin) => {
                break;
            }

            Some(ref unexpected) => {
                return Err(ParseError::UnexpectedToken(unexpected.clone(),
                                                       Some(match_decl_first)));
            }

            None => {
                let unexpected = tokens.next().unwrap();
                return Err(ParseError::UnexpectedToken(unexpected.clone(),
                                                       Some(match_decl_first)));
            }
        }
    }

    Ok(ParseOutput::new(decls, last_parsed, tokens))
}

impl Program {
    pub fn parse<TIter>(tokens: TIter,
                        context: &source::Token) -> ParseResult<Self>
        where TIter: Iterator<Item=source::Token> + 'static
    {
        let program_statement = keywords::Program
            .and_then(Matcher::AnyIdentifier)
            .match_sequence(tokens, context)?;

        let name = program_statement.value
            .get(1).unwrap()
            .as_token()
            .unwrap_identifier()
            .to_owned();

        let end_name = tokens::Semicolon
            .match_or_endl(program_statement.next_tokens,
                           &program_statement.last_token)?;

        let uses = parse_uses(end_name.next_tokens,
                              &end_name.last_token)?;

        let decls = parse_decls(uses.next_tokens, &uses.last_token)?;

        let program_block = Block::parse(decls.next_tokens, &decls.last_token)?;

        let last_period = tokens::Period.match_one(program_block.next_tokens,
                                                   &program_block.last_token)?;

        let program = Program {
            name,
            uses: uses.value,

            decls: decls.value,

            program_block: program_block.value,
        };

        Ok(ParseOutput::new(program, last_period.last_token, last_period.next_tokens))
    }
}

impl node::ToSource for Program {
    fn to_source(&self) -> String {
        let mut lines = Vec::new();
        lines.push(format!("program {};", self.name));

        if self.uses.len() > 0 {
            lines.push(format!("uses {};",
                               self.uses.iter().map(|u| format!("{}", u))
                                   .collect::<Vec<_>>()
                                   .join(", ")));
        }

        for decl in self.decls.iter() {
            match decl {
                &node::UnitDeclaration::Record(ref rec_decl) =>
                    lines.push(rec_decl.to_source()),

                &node::UnitDeclaration::Function(ref func_decl) =>
                    lines.push(func_decl.to_source()),

                &node::UnitDeclaration::Vars(ref var_decls) =>
                    lines.push(var_decls.to_source()),
            }
        }

        lines.push(self.program_block.to_source() + ".");

        lines.join("\n\n")
    }
}