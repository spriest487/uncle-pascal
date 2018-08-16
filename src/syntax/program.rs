use syntax::*;
use types;
use tokens;
use keywords;
use ToSource;

mod type_decl {
    use types;

    #[derive(Clone, Debug)]
    pub struct RecordDecl {
        name: types::Identifier,
        members: Vec<RecordDecl>,
    }
}

fn parse_uses<TIter, TToken>(in_tokens: TIter) -> ParseResult<Vec<types::Identifier>, TToken>
    where TIter: IntoIterator<Item=TToken> + 'static,
          TToken: tokens::AsToken + 'static
{
    let uses_matcher = Matcher::Keyword(keywords::Uses);

    let mut tokens = in_tokens.into_iter();

    let find_keyword = tokens.nth(0)
        .into_iter()
        .filter(|first_token| uses_matcher.is_match(first_token))
        .next();

    match find_keyword {
        Some(_) => {
            let match_semicolon = Matcher::Exact(tokens::Semicolon);
            let (uses_tokens, after_uses) = match_semicolon.split_at_match(tokens)?.unwrap();

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

            Ok(ParseOutput::new(uses_identifiers?, after_uses))
        }
        None => {
            //no uses
            Ok(ParseOutput::new(Vec::new(), tokens))
        }
    }
}

struct ProgramDecls {
    functions: Vec<function::Function>,
    type_decls: Vec<type_decl::RecordDecl>,
}

fn parse_decls<TIter, TToken>(in_tokens: TIter) -> ParseResult<ProgramDecls, TToken>
    where TIter: IntoIterator<Item=TToken> + 'static,
          TToken: tokens::AsToken + 'static
{
    let mut tokens: Box<Iterator<Item=TToken>> = Box::from(in_tokens.into_iter());

    let mut decls = ProgramDecls {
        functions: Vec::new(),
        type_decls: Vec::new(),
    };

    loop {
        let match_decl_first = Matcher::Keyword(keywords::Function)
                //.or(Matcher::Keyword(keywords::Uses))
                .or(Matcher::Keyword(keywords::Var))
                .or(Matcher::Keyword(keywords::Function))
                .or(Matcher::Keyword(keywords::Begin));

        let (decl_first, decl_tokens) = match_decl_first.match_peek(tokens)?
            .unwrap();
        tokens = Box::from(decl_tokens);

        match decl_first.as_token() {
            &tokens::Keyword(keywords::Function) => {
                let (parsed_fn, after_fn) = function::Function::parse(tokens)?.unwrap();

                decls.functions.push(parsed_fn);

                tokens = Box::from(after_fn);
            }

            &tokens::Keyword(keywords::Type) => {
                unimplemented!()
            }

            &tokens::Keyword(keywords::Begin) |
            &tokens::Keyword(keywords::Var) => {
                break;
            }

            _ => {
                let expected = Matcher::OneOf(vec![
                    Box::from(Matcher::Keyword(keywords::Function)),
                    Box::from(Matcher::Keyword(keywords::Type)),
                ]);

                return Err(ParseError::UnexpectedToken(decl_first.clone(), Some(expected)));
            }
        }
    }

    Ok(ParseOutput::new(decls, tokens.into_iter()))
}

#[derive(Clone, Debug)]
pub struct Program {
    name: types::Identifier,

    uses: Vec<types::Identifier>,
    functions: Vec<function::Function>,
    type_decls: Vec<type_decl::RecordDecl>,

    //TODO
    program_block: Vec<tokens::Token>,
}

impl Program {
    pub fn parse<TIter, TToken>(tokens: TIter) -> ParseResult<Self, TToken>
        where TIter: Iterator<Item=TToken>,
              TToken: tokens::AsToken + 'static
    {
        let (program_statement, after_program_statement) = Matcher::Keyword(keywords::Program)
            .and_then(Matcher::AnyIdentifier)
            .and_then(Matcher::Exact(tokens::Semicolon))
            .match_sequence(tokens)?.unwrap();

        let name = types::Identifier::parse(program_statement
            .get(1).unwrap()
            .as_token()
            .unwrap_identifier());

        let (uses, after_uses) = parse_uses(after_program_statement)?.unwrap();
        let (decls, after_decls) = parse_decls(after_uses)?.unwrap();

        let (program_block, remaining) = Matcher::Keyword(keywords::Begin)
            .paired_with(Matcher::Keyword(keywords::End))
            .match_pair(after_decls)?
            .unwrap();

        Ok(ParseOutput::new(Self {
            name,
            uses,

            functions: decls.functions,
            type_decls: decls.type_decls,

            program_block: program_block.inner.iter().map(|t| t.as_token())
                .cloned()
                .collect(),
        }, remaining))
    }
}

impl ToSource for Program {
    fn to_source(&self) -> String {
        vec![
            format!("program {};", self.name),
            format!("uses {};",
                    self.uses.iter().map(|u| format!("{}", u))
                        .collect::<Vec<_>>()
                        .join(", ")),
//            format!("{};", self.vars.to_source()),
            format!("begin\n\nend."),
        ].join("\n\n")
    }
}