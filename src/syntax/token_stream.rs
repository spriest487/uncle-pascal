use std::iter::Peekable;
use source;
use tokens::AsToken;
use syntax::{
    matcher::*,
    ParseError,
    ParseResult,
    BlockGroupsMatch,
    SplitResult,
};

pub struct TokenStream {
    tokens: Peekable<Box<Iterator<Item=source::Token>>>,
    context: source::Token,
}

impl Iterator for TokenStream {
    type Item = source::Token;

    fn next(&mut self) -> Option<<Self as Iterator>::Item> {
        self.context = self.tokens.next();
        self.context.clone()
    }
}

impl TokenStream {
    pub fn new(tokens: impl IntoIterator<Item=source::Token> + 'static,
               context: &source::Token) -> Self {
        TokenStream {
            tokens: Box::new(tokens.into_iter()).peekable(),
            context: context.clone(),
        }
    }

    pub fn context(&self) -> &source::Token {
        &self.context
    }

    pub fn finish(mut self) -> ParseResult<()> {
        match self.next() {
            Some(unexpected) => Err(ParseError::UnexpectedToken(unexpected, None)),
            None => Ok(())
        }
    }

    pub fn peek(&mut self) -> Option<source::Token> {
        self.tokens.peek().cloned()
    }

    pub fn match_one(&mut self, matcher: impl Into<Matcher>) -> ParseResult<source::Token> {
        let matcher = matcher.into();

        match self.next() {
            Some(token) => {
                if !matcher.is_match(token.as_token()) {
                    return Err(ParseError::UnexpectedToken(token, Some(matcher)));
                }

                self.context = token.clone();
                Ok(token)
            }

            None => {
                Err(ParseError::UnexpectedEOF(matcher, self.context.clone()))
            }
        }
    }

    pub fn match_or_endl(&mut self, matcher: impl Into<Matcher>) -> ParseResult<()> {
        match self.peek() {}
    }

    pub fn match_peek(&mut self, matcher: impl Into<Matcher>) -> ParseResult<Option<source::Token>> {
        match self.peek() {
            Some(token) => {
                if !matcher.is_match(token.as_token()) {
                    return Ok(None);
                }

                self.context = token.clone();
                Ok(Some(token))
            }

            None => {
                Err(ParseError::UnexpectedEOF(matcher, self.context.clone()))
            }
        }
    }

    pub fn match_sequence(&mut self,
                          sequence: impl IntoIterator<Item=Matcher>)
                          -> Result<Vec<source::Token>, ParseError> {
        sequence.into().into_iter()
            .map(|matcher| self.match_one(&matcher))
            .collect::<Result<Vec<_>, _>>()
    }

    pub fn match_groups(&mut self,
                        open: impl Into<Matcher>,
                        close: impl Into<Matcher>,
                        separator: impl Into<Matcher>)
                        -> ParseResult<BlockGroupsMatch> {
        unimplemented!()
    }

    pub fn match_block(&mut self,
                       open: impl Into<Matcher>,
                       close: impl Into<Matcher>)
                       -> ParseResult<BlockMatch> {
        unimplemented!()
    }

    pub fn match_block_peek(&mut self,
                            open: impl Into<Matcher>,
                            close: impl Into<Matcher>)
                            -> ParseResult<Option<BlockMatch>> {
        unimplemented!()
    }

    pub fn split_at_match(&mut self, matcher: impl Into<Matcher>)
                          -> ParseResult<SplitResult> {
        unimplemented!()
    }
}