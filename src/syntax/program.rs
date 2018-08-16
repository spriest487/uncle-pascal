use syntax::*;
use types;
use tokens;
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

fn parse_uses<TIter, TToken>(in_tokens: TIter) -> ParseResult<Vec<types::Identifier>, TToken>
    where TIter: IntoIterator<Item=TToken> + 'static,
          TToken: tokens::AsToken + 'static
{
    let uses_matcher = TokenMatcher::Keyword(keywords::Uses);

    let mut tokens = in_tokens.into_iter();

    let find_keyword = tokens.nth(0)
        .into_iter()
        .filter(|first_token| uses_matcher.match_token(first_token))
        .next();

    match find_keyword {
        Some(_) => {
            let match_semicolon = TokenMatcher::Exact(tokens::Semicolon);
            let uses_tokens = match_semicolon.until_match(tokens)?;

            let match_comma = TokenMatcher::Exact(tokens::Comma);
            let uses_identifiers: Result<Vec<_>, ParseError<_>> = uses_tokens.value
                .split(|source_token| match_comma.match_token(source_token.as_token()))
                .map(|source_tokens| {
                    if source_tokens.len() == 1 && source_tokens[0].as_token().is_any_identifier() {
                        Ok(types::Identifier::parse(source_tokens[0].as_token().unwrap_identifier()))
                    } else {
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
        let next = tokens.next().ok_or_else(|| {
            ParseError::UnexpectedEOF
        })?;

        /* we can't use a peekable iter so "wind back" tokens by
        pasting the token we just consumed back onto the beginning */
        tokens = Box::from(vec![next.clone()]
            .into_iter()
            .chain(tokens)
            .into_iter());

        match next.as_token() {
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
                let expected = TokenMatcher::OneOf(vec![
                    Box::from(TokenMatcher::Keyword(keywords::Function)),
                    Box::from(TokenMatcher::Keyword(keywords::Type)),
                ]);

                return Err(ParseError::UnexpectedToken(next.clone(), Some(expected)));
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
    pub fn parse<TIter, TToken>(tokens: TIter) -> ParseResult<Self, TToken>
        where TIter: Iterator<Item=TToken>,
              TToken: tokens::AsToken + 'static
    {
        let (program_statement, after_program_statment) = TokenMatcher::Keyword(keywords::Program)
            .and_then(TokenMatcher::AnyIdentifier)
            .and_then(TokenMatcher::Exact(tokens::Semicolon))
            .match_tokens(tokens)?.unwrap();

        let name = types::Identifier::parse(program_statement
            .get(1).unwrap()
            .as_token()
            .unwrap_identifier());

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
