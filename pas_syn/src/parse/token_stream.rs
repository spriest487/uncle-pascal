use std::collections::VecDeque;

use pas_common::{span::*, TracedError};

use crate::{parse::*, token_tree::*};

pub struct TokenStream {
    tokens: Box<dyn Iterator<Item = TokenTree>>,
    context: Span,

    current: Option<TokenTree>,

    lookahead_buffer: VecDeque<TokenTree>,
}

impl Iterator for TokenStream {
    type Item = TokenTree;

    fn next(&mut self) -> Option<TokenTree> {
        let tt = self.lookahead_buffer
            .pop_front()
            .or_else(|| self.tokens.next())
            .map(|next_token| {
                self.context = next_token.span().clone();
                next_token
            });

        if tt.is_some() {
            self.current = tt.clone();
        }

        tt
    }
}

impl TokenStream {
    pub fn new(tokens: impl IntoIterator<Item = TokenTree> + 'static, context: Span) -> Self {
        let token_iter = Box::new(tokens.into_iter());

        TokenStream {
            tokens: token_iter,
            context,

            current: None,

            lookahead_buffer: VecDeque::new(),
        }
    }

    pub fn context(&self) -> &Span {
        &self.context
    }

    pub fn advance(&mut self, count: usize) {
        // skip new stream elements
        for _ in 0..count {
            self.next();
        }
    }

    pub fn current(&self) -> Option<&TokenTree> {
        self.current.as_ref()
    }

    pub fn finish(mut self) -> ParseResult<()> {
        match self.next() {
            Some(unexpected) => Err(TracedError::trace(ParseError::UnexpectedToken(
                Box::new(unexpected),
                None,
            ))),
            None => Ok(()),
        }
    }

    pub fn match_one(&mut self, matcher: impl Into<Matcher>) -> ParseResult<TokenTree> {
        let matcher = matcher.into();

        match self.next() {
            Some(token) => {
                if matcher.is_match(&token) {
                    Ok(token)
                } else {
                    Err(TracedError::trace(ParseError::UnexpectedToken(
                        Box::new(token),
                        Some(matcher),
                    )))
                }
            }

            None => Err(TracedError::trace(ParseError::UnexpectedEOF(
                matcher,
                self.context.clone(),
            ))),
        }
    }

    /// Match one token in the stream. If the matcher doesn't match the next token, don't
    /// consume it and return `None`. If the matcher does match the next token, consume it and
    /// return it as `Some`.
    pub fn match_one_maybe(&mut self, matcher: impl Into<Matcher>) -> Option<TokenTree> {
        self.look_ahead().match_one(matcher).map(|tt| {
            self.advance(1);
            tt
        })
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
            Some(ref token) if token.span().start.line != self.context.end.line => Ok(()),

            // found separator token - next token is the one beyond that
            Some(ref token) if matcher.is_match(token) => {
                self.next();
                Ok(())
            }

            Some(unexpected) => Err(TracedError::trace(ParseError::UnexpectedToken(
                Box::new(unexpected),
                Some(matcher),
            ))),
        }
    }

    pub fn match_sequence(
        &mut self,
        sequence: impl IntoIterator<Item = Matcher>,
    ) -> ParseResult<Vec<TokenTree>> {
        sequence
            .into_iter()
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
    where
        TParsed: Parse,
    {
        TParsed::parse(self)
    }

    pub fn parse_to_end<TParsed>(mut self) -> ParseResult<TParsed>
    where
        TParsed: Parse,
    {
        let result = TParsed::parse(&mut self)?;
        self.finish()?;
        Ok(result)
    }

    pub fn match_repeating<T, F>(&mut self, mut f: F) -> ParseResult<Vec<T>>
    where
        F: FnMut(usize, &mut TokenStream) -> ParseResult<Generate<T>>,
    {
        let mut results = Vec::new();
        loop {
            match f(results.len(), self)? {
                Generate::Yield(result) => results.push(result),
                Generate::Break => break Ok(results),
            }
        }
    }
}

pub trait Parse
where
    Self: Sized,
{
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

    pub fn context(&self) -> &Span {
        if self.pos == 0 {
            self.tokens.context()
        } else {
            &self.tokens.lookahead_buffer[self.pos - 1].span()
        }
    }

    pub fn match_one(&mut self, matcher: impl Into<Matcher>) -> Option<TokenTree> {
        let matcher = matcher.into();

        self.next()
            .and_then(|t| if matcher.is_match(&t) { Some(t) } else { None })
    }

    /// check that the next token in the stream matches `matcher` without consuming it, and throw
    /// a ParseError if it doesn't
    pub fn expect_one(&mut self, matcher: impl Into<Matcher>) -> ParseResult<()> {
        let matcher = matcher.into();
        match self.next() {
            Some(ref tt) if matcher.is_match(&tt) => Ok(()),
            Some(unexpected) => Err(TracedError::trace(ParseError::UnexpectedToken(
                Box::new(unexpected),
                Some(matcher),
            ))),
            None => Err(TracedError::trace(ParseError::UnexpectedEOF(
                matcher,
                self.context().clone(),
            ))),
        }
    }

    pub fn match_sequence(
        &mut self,
        sequence: impl Into<SequenceMatcher>,
    ) -> Option<Vec<TokenTree>> {
        let mut sequence = sequence.into().into_iter();
        let mut matches = Vec::new();

        loop {
            let seq_next_matcher = match sequence.next() {
                Some(matcher) => matcher,
                None => {
                    // reached end of sequence without incident
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
                    // there are less tokens in the stream than in the sequence
                    break None;
                }
            };
        }
    }
}

impl<'tokens> Iterator for LookAheadTokenStream<'tokens> {
    type Item = TokenTree;

    fn next(&mut self) -> Option<TokenTree> {
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

pub enum Generate<T> {
    Break,
    Yield(T),
}
