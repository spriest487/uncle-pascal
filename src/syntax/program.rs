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
                                   uses_kw.next));
    }

    //there is a uses, advance past it
    let mut after_uses = uses_kw.next;
    after_uses.next();

    let match_semicolon = Matcher::Exact(tokens::Semicolon);
    let (uses_tokens, _, after_uses) = match_semicolon.split_at_match(after_uses,
                                                                   &uses_kw.last_parsed)?
        .unwrap();

    let match_comma = Matcher::Exact(tokens::Comma);
    let uses_identifiers: Result<Vec<_>, ParseError<_>> = uses_tokens.before_split
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
                        uses_tokens.split_at,
                        after_uses))
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
    let mut program_type_decls = Vec::new();
    let mut program_vars = Vars::default();

    let mut last_parsed = context.clone();

    loop {
        let match_decl_first = Matcher::Keyword(keywords::Function)
            //.or(Matcher::Keyword(keywords::Uses))
            .or(Matcher::Keyword(keywords::Var))
            .or(Matcher::Keyword(keywords::Begin));

        let (decl_first, decl_last, decl_tokens) = match_decl_first
            .match_peek(tokens, &last_parsed)?
            .unwrap();
        tokens = Box::from(decl_tokens);

        match decl_first {
            Some(ref func_kw) if func_kw.as_token().is_keyword(keywords::Function) => {
                let (parsed_fn, fn_last, after_fn) = function::Function::parse(tokens, &decl_last)?
                    .unwrap();

                program_functions.push(parsed_fn);

                tokens = Box::from(after_fn);
                last_parsed = fn_last;
            }

            Some(ref type_kw) if type_kw.as_token().is_keyword(keywords::Type) => {
                unimplemented!()
            }

            Some(ref var_kw) if var_kw.as_token().is_keyword(keywords::Var) => {
                let (vars, vars_last, after_vars) = Vars::parse(tokens, &decl_last)?
                    .unwrap();
                program_vars.decls.extend(vars.decls);

                tokens = Box::from(after_vars);
                last_parsed = vars_last;
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

    //TODO
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

        let uses = parse_uses(program_statement.next,
                              &program_statement.last_parsed)?;

        let (decls, decls_last, after_decls) = parse_decls(uses.next, &uses.last_parsed)?.unwrap();

        let (program_block, block_last, after_block) = Block::parse(after_decls, &decls_last)?
            .unwrap();

        let (last_period, _, trailing) = Matcher::Exact(tokens::Period)
            .match_one(after_block, &block_last)?
            .unwrap();

        let program = Self {
            name,
            uses: uses.value,

            functions: decls.functions,
            type_decls: decls.type_decls,
            vars: decls.vars,

            program_block: program_block.block,
        };

        Ok(ParseOutput::new(program, last_period, trailing))
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