use std::iter::Peekable;
use source;
use tokens::AsToken;
use tokenizer::{
    tokenize,
    TokenizeResult
};
use syntax::{
    matcher::*,
    ParseError,
    ParseResult,
    BlockGroupsMatch,
    SplitResult,
    GroupMatch,
};

pub struct TokenStream {
    tokens: Peekable<Box<Iterator<Item=source::Token>>>,
    context: source::Token,
}

impl Iterator for TokenStream {
    type Item = source::Token;

    fn next(&mut self) -> Option<<Self as Iterator>::Item> {
        self.tokens.next().map(|next_token| {
            self.context = next_token.clone();
            next_token
        })
    }
}

impl From<GroupMatch> for TokenStream {
    fn from(group: GroupMatch) -> Self {
        TokenStream::new(group.tokens, &group.context)
    }
}

impl From<Vec<source::Token>> for TokenStream {
    fn from(tokens: Vec<source::Token>) -> Self {
        if tokens.len() == 0 {
            panic!("length of tokenstream must be at least 1");
        }

        let context = tokens[0].clone();

        let boxed: Box<Iterator<Item=source::Token>> = Box::new(tokens.into_iter());

        TokenStream {
            tokens: boxed.peekable(),
            context,
        }
    }
}

impl TokenStream {
    pub fn tokenize(file_name: &str, source: &str) -> TokenizeResult<Self> {
        let tokens = tokenize(file_name, source)?;

        Ok(TokenStream::from(tokens))
    }

    pub fn new(tokens: impl IntoIterator<Item=source::Token> + 'static,
               context: &source::Token) -> Self {
        let boxed: Box<Iterator<Item=source::Token>> = Box::new(tokens.into_iter());

        TokenStream {
            tokens: boxed.peekable(),
            context: context.clone(),
        }
    }

    pub fn context(&self) -> &source::Token {
        &self.context
    }

    pub fn advance(&mut self, count: usize) {
        for _ in 0..count {
            self.next().expect("must not reach unexpected EOF while advancing token stream");
        }
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
        unimplemented!()
    }

    pub fn match_peek(&mut self, matcher: impl Into<Matcher>) -> ParseResult<Option<source::Token>> {
        let matcher = matcher.into();
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

    pub fn match_until(&mut self, until: impl Into<Matcher>) -> ParseResult<Vec<source::Token>> {
        let until = until.into();
        let mut result = Vec::new();

        loop {
            match self.peek() {
                Some(ref matching) if until.is_match(matching) => {
                    break Ok(result);
                }

                Some(not_matching) => {
                    self.next();
                    result.push(not_matching)
                }

                None => {
                    break Err(ParseError::UnexpectedEOF(until, self.context.clone()));
                }
            }
        }
    }

    pub fn match_sequence(&mut self,
                          sequence: impl IntoIterator<Item=Matcher>)
                          -> Result<Vec<source::Token>, ParseError> {
        sequence.into_iter()
            .map(|matcher| self.match_one(matcher))
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