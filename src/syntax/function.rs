use syntax::*;
use tokenizer;
use keywords;

#[derive(Debug, Clone)]
pub struct Function {}

impl Function {
    pub fn parse<I>(tokens: I) -> ParseResult<Self>
        where I: IntoIterator<Item=tokenizer::SourceToken> + 'static
    {
        //TODO: just stop at next END for now
        let mut tokens = tokens.into_iter();

        let match_end = TokenMatcher::Keyword(keywords::End);
        let end = tokens.find(|source_token| {
            match_end.match_token(&source_token.token)
        });

        match end {
            Some(_) => Ok(ParseOutput::new(Function {}, tokens)),
            None => Err(ParseError::UnexpectedEOF),
        }
    }
}
