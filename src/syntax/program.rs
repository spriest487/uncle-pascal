use syntax;
use types;
use tokens;
use tokenizer;
use keywords;

mod function {
    #[derive(Clone, Debug)]
    pub struct Function {}
}

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
        members: Vec<VarDecl>,
    }
}

fn parse_uses<I>(in_tokens: I) -> syntax::ParseResult<Vec<types::Identifier>>
    where I: IntoIterator<Item=tokenizer::SourceToken> + 'static
{
    let uses_matcher = syntax::TokenMatcher::Keyword(keywords::Uses);

    let mut tokens = in_tokens.into_iter();

    let find_keyword = tokens.nth(0)
        .into_iter()
        .filter(|first_token| uses_matcher.match_token(&first_token.token))
        .next();

    match find_keyword {
        Some(_) => {
            let match_semicolon = syntax::TokenMatcher::Exact(tokens::Semicolon);
            let uses_tokens = match_semicolon.until_match(tokens)?;

            let match_comma = syntax::TokenMatcher::Exact(tokens::Comma);
            let uses_identifiers: Result<Vec<_>, syntax::ParseError> = uses_tokens.value
                .split(|source_token| match_comma.match_token(&source_token.token))
                .map(|source_tokens| {
                    if source_tokens.len() == 1 && source_tokens[0].token.is_any_identifier() {
                        Ok(types::Identifier::parse(source_tokens[0].token.unwrap_identifier()))
                    }
                    else {
                        Err(syntax::ParseError::UnexpectedToken(source_tokens[0].clone(),
                            Some(syntax::TokenMatcher::AnyIdentifier)))
                    }
                })
                .collect();

            Ok(syntax::ParseOutput::new(uses_identifiers?, uses_tokens.next))
        }
        None => {
            //no uses
            Ok(syntax::ParseOutput::new(Vec::new(), tokens))
        }
    }
}

#[derive(Clone, Debug)]
pub struct Program {
    name: types::Identifier,

    uses: Vec<types::Identifier>,
    functions: Vec<function::Function>,
    type_decls: Vec<type_decl::RecordDecl>,
}

impl Program {
    pub fn parse<I>(tokens: I) -> syntax::ParseResult<Self>
        where I: Iterator<Item=tokenizer::SourceToken>
    {
        let parsed_start = syntax::TokenMatcher::Keyword(keywords::Program)
            .and_then(syntax::TokenMatcher::AnyIdentifier)
            .and_then(syntax::TokenMatcher::Exact(tokens::Semicolon))
            .match_tokens(tokens)?;

        let name = types::Identifier::parse(parsed_start.value
            .get(1).unwrap()
            .token.unwrap_identifier());

        let parsed_uses = parse_uses(parsed_start.next)?;

        let uses = parsed_uses.value;
        let after_uses = parsed_uses.next;

        Ok(syntax::ParseOutput::new(Self {
            name,
            uses,

            functions: Vec::new(),
            type_decls: Vec::new(),
        }, after_uses))
    }
}
