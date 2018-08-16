use syntax::*;
use types;
use tokens;
use keywords;
use ToSource;

fn parse_uses<TIter>(in_tokens: TIter, context: &TIter::Item) -> ParseResult<Vec<types::Identifier>, TIter::Item>
    where TIter: IntoIterator + 'static,
          TIter::Item: tokens::AsToken + 'static
{
    let uses_matcher = Matcher::Keyword(keywords::Uses);
    let uses_kw = uses_matcher.match_peek(in_tokens, context)?;

    if uses_kw.value.is_none() {
        //no uses
        return Ok(ParseOutput::new(Vec::new(),
                                   context.clone(),
                                   uses_kw.next_tokens));
    }

    //there is a uses, advance past it
    let mut after_uses = uses_kw.next_tokens;
    after_uses.next();

    let match_semicolon = Matcher::Exact(tokens::Semicolon);
    let uses_tokens = match_semicolon.split_at_match(after_uses, &uses_kw.last_token)?;

    let match_comma = Matcher::Exact(tokens::Comma);
    let uses_identifiers: Result<Vec<_>, ParseError<_>> = uses_tokens.value.before_split
        .split(|source_token| match_comma.is_match(source_token))
        .map(|source_tokens| {
            if source_tokens.len() == 1 &&
                source_tokens[0].as_token().is_any_identifier() {
                Ok(types::Identifier::parse(source_tokens[0].as_token().unwrap_identifier()))
            } else {
                Err(ParseError::UnexpectedToken(source_tokens[0].clone(),
                                                Some(Matcher::AnyIdentifier)))
            }
        })
        .collect();

    Ok(ParseOutput::new(uses_identifiers?,
                        uses_tokens.value.split_at,
                        uses_tokens.next_tokens))
}

struct ProgramDecls {
    functions: Vec<Function>,
    type_decls: Vec<RecordDecl>,
    vars: Vars,
}

fn parse_decls<TIter>(in_tokens: TIter, context: &TIter::Item) -> ParseResult<ProgramDecls, TIter::Item>
    where TIter: IntoIterator + 'static,
          TIter::Item: tokens::AsToken + 'static
{
    let mut tokens: Box<Iterator<Item=TIter::Item>> = Box::from(in_tokens.into_iter());

    let mut program_functions = Vec::new();

    #[allow(unused_mut)]
    let mut program_type_decls = Vec::new();
    let mut program_vars = Vars::default();

    let mut last_parsed = context.clone();

    loop {
        let match_decl_first = Matcher::Keyword(keywords::Function)
            //.or(Matcher::Keyword(keywords::Uses)) TODO?
            .or(Matcher::Keyword(keywords::Var))
            .or(Matcher::Keyword(keywords::Begin));

        let peek_decl = match_decl_first.match_peek(tokens, &last_parsed)?;
        tokens = Box::from(peek_decl.next_tokens);

        match peek_decl.value {
            Some(ref func_kw) if func_kw.as_token().is_keyword(keywords::Function) => {
                let func = Function::parse(tokens, &peek_decl.last_token)?;
                program_functions.push(func.value);

                tokens = Box::from(func.next_tokens);
                last_parsed = func.last_token;
            }

            Some(ref type_kw) if type_kw.as_token().is_keyword(keywords::Type) => {
                unimplemented!()
            }

            Some(ref var_kw) if var_kw.as_token().is_keyword(keywords::Var) => {
                let vars = Vars::parse(tokens, &peek_decl.last_token)?;
                program_vars.decls.extend(vars.value.decls);

                tokens = Box::from(vars.next_tokens);
                last_parsed = vars.last_token;
            }

            Some(ref begin_kw) if begin_kw.as_token().is_keyword(keywords::Begin) => {
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

    let decls = ProgramDecls {
        functions: program_functions,
        type_decls: program_type_decls,
        vars: program_vars,
    };

    Ok(ParseOutput::new(decls, last_parsed, tokens))
}

#[derive(Clone, Debug)]
pub struct Program {
    name: types::Identifier,

    uses: Vec<types::Identifier>,

    functions: Vec<Function>,
    type_decls: Vec<RecordDecl>,
    vars: Vars,

    program_block: Block,
}

impl Program {
    pub fn parse<TIter>(tokens: TIter, context: &TIter::Item) -> ParseResult<Self, TIter::Item>
        where TIter: Iterator + 'static,
              TIter::Item: tokens::AsToken + 'static
    {
        let program_statement = Matcher::Keyword(keywords::Program)
            .and_then(Matcher::AnyIdentifier)
            .and_then(Matcher::Exact(tokens::Semicolon))
            .match_sequence(tokens, context)?;

        let name = types::Identifier::parse(program_statement.value
            .get(1).unwrap()
            .as_token()
            .unwrap_identifier());

        let uses = parse_uses(program_statement.next_tokens,
                              &program_statement.last_token)?;

        let decls = parse_decls(uses.next_tokens, &uses.last_token)?;

        let program_block = Block::parse(decls.next_tokens, &decls.last_token)?;

        let last_period = Matcher::Exact(tokens::Period)
            .match_one(program_block.next_tokens, &program_block.last_token)?;

        let program = Program {
            name,
            uses: uses.value,

            functions: decls.value.functions,
            type_decls: decls.value.type_decls,
            vars: decls.value.vars,

            program_block: program_block.value,
        };

        Ok(ParseOutput::new(program, last_period.last_token, last_period.next_tokens))
    }
}

impl ToSource for Program {
    fn to_source(&self) -> String {
        let mut lines = Vec::new();
        lines.push(format!("program {};", self.name));

        if self.uses.len() > 0 {
            lines.push(format!("uses {};",
                               self.uses.iter().map(|u| format!("{}", u))
                                   .collect::<Vec<_>>()
                                   .join(", ")));
        }

        for func in self.functions.iter() {
            lines.push(func.to_source());
        }

        if self.vars.decls.len() > 0 {
            lines.push(self.vars.to_source());
        }

        lines.push(self.program_block.to_source() + ".");

        lines.join("\n\n")
    }
}