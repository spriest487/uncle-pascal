use syntax::*;
use types;
use tokens;
use tokenizer;
use keywords;

mod type_decl {
    use types;

    #[derive(Clone, Debug)]
    pub struct VarDecl {
        name: types::Identifier,
        decl_type: types::Identifier,
    }

    #[derive(Clone, Debug)]
    pub struct RecordDecl {
        name: types::Identifier,
        members: Vec<RecordDecl>,
    }
}

fn parse_uses<I>(in_tokens: I) -> ParseResult<Vec<types::Identifier>>
    where I: IntoIterator<Item=tokenizer::SourceToken> + 'static
{
    let uses_matcher = TokenMatcher::Keyword(keywords::Uses);

    let mut tokens = in_tokens.into_iter();

    let find_keyword = tokens.nth(0)
        .into_iter()
        .filter(|first_token| uses_matcher.match_token(&first_token.token))
        .next();

    match find_keyword {
        Some(_) => {
            let match_semicolon = TokenMatcher::Exact(tokens::Semicolon);
            let uses_tokens = match_semicolon.until_match(tokens)?;

            let match_comma = TokenMatcher::Exact(tokens::Comma);
            let uses_identifiers: Result<Vec<_>, ParseError> = uses_tokens.value
                .split(|source_token| match_comma.match_token(&source_token.token))
                .map(|source_tokens| {
                    if source_tokens.len() == 1 && source_tokens[0].token.is_any_identifier() {
                        Ok(types::Identifier::parse(source_tokens[0].token.unwrap_identifier()))
                    }
                    else {
                        Err(ParseError::UnexpectedToken(source_tokens[0].clone(),
                            Some(TokenMatcher::AnyIdentifier)))
                    }
                })
                .collect();

            Ok(ParseOutput::new(uses_identifiers?, uses_tokens.next))
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

fn parse_decls<I>(in_tokens: I) -> ParseResult<ProgramDecls>
    where I: IntoIterator<Item=tokenizer::SourceToken> + 'static
{
    let mut tokens: Box<Iterator<Item=tokenizer::SourceToken>> = Box::from(in_tokens.into_iter());

    let mut decls = ProgramDecls {
        functions: Vec::new(),
        type_decls:Vec::new(),
    };

    loop {
        let next = tokens.next().ok_or_else(||{
            ParseError::UnexpectedEOF
        })?;

        match next.token {
            tokens::Keyword(keywords::Function) => {
                let (parsed_fn, after_fn) = function::Function::parse(tokens)?.unwrap();

                decls.functions.push(parsed_fn);

                tokens = Box::from(after_fn);
            },
            tokens::Keyword(keywords::Type) => { unimplemented!() },
            _ => {
                let expected = TokenMatcher::OneOf(vec![
                    Box::from(TokenMatcher::Keyword(keywords::Function)),
                    Box::from(TokenMatcher::Keyword(keywords::Type)),
                ]);

                return Err(ParseError::UnexpectedToken(next.clone(), Some(expected)));
            }

            tokens::Keyword(keywords::Begin) => {
                break;
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
}

impl Program {
    pub fn parse<I>(tokens: I) -> ParseResult<Self>
        where I: Iterator<Item=tokenizer::SourceToken>
    {
        let (program_statement, after_program_statment) = TokenMatcher::Keyword(keywords::Program)
            .and_then(TokenMatcher::AnyIdentifier)
            .and_then(TokenMatcher::Exact(tokens::Semicolon))
            .match_tokens(tokens)?.unwrap();

        let name = types::Identifier::parse(program_statement
            .get(1).unwrap()
            .token.unwrap_identifier());

        let (uses, after_uses) = parse_uses(after_program_statment)?.unwrap();
        let (decls, after_decls) = parse_decls(after_uses)?.unwrap();

        Ok(ParseOutput::new(Self {
            name,
            uses,

            functions: decls.functions,
            type_decls: decls.type_decls,
        }, after_decls))
    }
}
