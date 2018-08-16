use std::{
    collections::VecDeque,
};
use source;
use tokens::AsToken;
use tokenizer::{
    tokenize,
    TokenizeResult,
};
use opts::CompileOptions;
use syntax::{
    matcher::*,
    ParseError,
    ParseResult,
};

pub struct TokenStream {
    tokens: Box<Iterator<Item=source::Token>>,
    context: source::Token,

    lookahead_buffer: VecDeque<source::Token>,
}

impl Iterator for TokenStream {
    type Item = source::Token;

    fn next(&mut self) -> Option<<Self as Iterator>::Item> {
        self.lookahead_buffer.pop_front()
            .or_else(|| self.tokens.next())
            .map(|next_token| {
                self.context = next_token.clone();
                next_token
            })
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
            tokens: boxed,
            context,

            lookahead_buffer: VecDeque::new(),
        }
    }
}

impl TokenStream {
    pub fn tokenize(file_name: &str, source: &str, opts: &CompileOptions) -> TokenizeResult<Self> {
        let tokens = tokenize(file_name, source, opts)?;

        Ok(TokenStream::from(tokens))
    }

    pub fn new(tokens: impl IntoIterator<Item=source::Token> + 'static,
               context: &source::Token) -> Self {
        let boxed: Box<Iterator<Item=source::Token>> = Box::new(tokens.into_iter());

        TokenStream {
            tokens: boxed,
            context: context.clone(),

            lookahead_buffer: VecDeque::new(),
        }
    }

    pub fn context(&self) -> &source::Token {
        &self.context
    }

    pub fn advance(&mut self, count: usize) {
        /* skip new stream elements */
        for _ in 0..count {
            self.next();
        }
    }

    pub fn finish(mut self) -> ParseResult<()> {
        match self.next() {
            Some(unexpected) => {
                Err(ParseError::UnexpectedToken(unexpected, None))
            }
            None => Ok(())
        }
    }

    pub fn match_one(&mut self, matcher: impl Into<Matcher>) -> ParseResult<source::Token> {
        let matcher = matcher.into();

        match self.next() {
            Some(token) => {
                if !matcher.is_match(token.as_token()) {
                    return Err(ParseError::UnexpectedToken(token, Some(matcher)));
                }

                Ok(token)
            }

            None => {
                Err(ParseError::UnexpectedEOF(matcher, self.context.clone()))
            }
        }
    }

    /// match an optional line terminator. a token on the next line will satisfy the
    /// match but will not be consumed, and the end of the input will be treated as a
    /// success
    pub fn match_or_endl(&mut self, matcher: impl Into<Matcher>) -> ParseResult<()> {
        let matcher = matcher.into();

        match self.look_ahead().next() {
            // EOF counts as an endl
            None => Ok(()),

            // endl - next token is on a new line
            Some(ref token) if token.location.line != self.context.location.line => Ok(()),

            // found separator token - next token is the one beyond that
            Some(ref token) if matcher.is_match(token) => {
                self.next();
                Ok(())
            }

            Some(unexpected) => {
                Err(ParseError::UnexpectedToken(unexpected, Some(matcher)))
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

    pub fn look_ahead(&mut self) -> LookAheadTokenStream {
        LookAheadTokenStream {
            tokens: self,
            pos: 0,
            limit: None,
        }
    }

    pub fn parse<TParsed>(&mut self) -> ParseResult<TParsed>
        where TParsed: Parse
    {
        TParsed::parse(self)
    }

    pub fn parse_to_end<TParsed>(mut self) -> ParseResult<TParsed>
        where TParsed: Parse
    {
        let result = TParsed::parse(&mut self)?;
        self.finish()?;
        Ok(result)
    }

    pub fn match_repeating<T>(&mut self,
                              f: impl Fn(usize, &mut Self) -> ParseResult<Option<T>>)
                              -> ParseResult<Vec<T>> {
        let mut results = Vec::new();
        loop {
            match f(results.len(), self)? {
                Some(result) => results.push(result),
                None => break Ok(results),
            }
        }
    }
}

pub trait Parse where Self: Sized {
    fn parse(tokens: &mut TokenStream) -> ParseResult<Self>;
}

pub struct LookAheadTokenStream<'tokens> {
    tokens: &'tokens mut TokenStream,
    pos: usize,
    limit: Option<usize>,
}

impl<'tokens> LookAheadTokenStream<'tokens> {
    pub fn limit(mut self, limit: usize) -> Self {
        self.limit = Some(limit);
        self
    }

    pub fn context(&self) -> &source::Token {
        if self.pos == 0 {
            self.tokens.context()
        } else {
            &self.tokens.lookahead_buffer[self.pos - 1]
        }
    }

    pub fn match_one(&mut self, matcher: impl Into<Matcher>) -> Option<source::Token> {
        let matcher = matcher.into();

        self.next().and_then(|t| {
            match matcher.is_match(t.as_token()) {
                true => Some(t),
                false => None
            }
        })
    }

    pub fn match_sequence(&mut self,
                          sequence: impl Into<SequenceMatcher>)
                          -> Option<Vec<source::Token>> {
        let mut sequence = sequence.into().into_iter();
        let mut matches = Vec::new();

        loop {
            let seq_next_matcher = match sequence.next() {
                Some(matcher) => matcher,
                None => {
                    /* reached end of sequence without incident */
                    break Some(matches);
                }
            };

            match self.next() {
                Some(peeked_token) => {
                    if seq_next_matcher.is_match(&peeked_token) {
                        matches.push(peeked_token);
                    } else {
                        // no match
                        break None;
                    }
                }
                None => {
                    /* there are less tokens in the stream than in the sequence */
                    break None;
                }
            };
        }
    }
}

impl<'tokens> Iterator for LookAheadTokenStream<'tokens> {
    type Item = source::Token;

    fn next(&mut self) -> Option<source::Token> {
        if let Some(limit) = self.limit {
            if self.pos >= limit {
                return None;
            }
        }

        let next = if self.pos < self.tokens.lookahead_buffer.len() {
            self.tokens.lookahead_buffer.get(self.pos).cloned()
        } else {
            let next_token = self.tokens.tokens.next()?;
            self.tokens.lookahead_buffer.push_back(next_token.clone());
            Some(next_token)
        };

        self.pos += 1;
        next
    }
}