pub mod function;
pub mod program;
pub mod block;

use std::fmt;

use keywords;
use tokens;

#[derive(Clone, Debug)]
pub enum ParseError<TToken> {
    UnexpectedToken(TToken, Option<TokenMatcher>),
    UnbalancedPair(TokenMatcher, TToken),
    UnterminatedBlock,
    UnexpectedEOF,
}

impl<TToken> fmt::Display for ParseError<TToken>
    where TToken: tokens::AsToken + fmt::Display
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &ParseError::UnexpectedToken(ref source_token, ref expected) => {
                write!(f, "unexpected token: {}", source_token)?;

                expected.as_ref()
                    .map(|matcher| write!(f, " (expected: {})", matcher))
                    .unwrap_or(Ok(()))
            },
            &ParseError::UnbalancedPair(ref matcher, ref source_token) =>
                write!(f, "unbalanced pair: expected {} after {}", matcher, source_token),
            &ParseError::UnterminatedBlock =>
                write!(f, "unterminated block"),
            &ParseError::UnexpectedEOF =>
                write!(f, "unexpected end of input"),
        }
    }
}

pub struct ParseOutput<TToken, TValue> {
    pub value: TValue,
    pub next: Box<Iterator<Item=TToken>>,
}

impl<TToken, TValue> ParseOutput<TToken, TValue> {
    pub fn new<TNext>(value: TValue, next: TNext) -> Self
        where TNext: IntoIterator<Item=TToken> + 'static
    {
        Self {
            value,
            next: Box::from(next.into_iter()),
        }
    }

    pub fn finish(mut self) -> Result<TValue, ParseError<TToken>> {
        let unexpected = self.next.next();
        match unexpected {
            Some(token) => Err(ParseError::UnexpectedToken(token, None)),
            None => Ok(self.value)
        }
    }

    pub fn unwrap(self) -> (TValue, WrapIter<TToken>) {
        (self.value, WrapIter {
            wrapped: self.next
        })
    }
}

pub struct WrapIter<TItem> {
    wrapped: Box<Iterator<Item=TItem>>
}

impl<TItem> Iterator for WrapIter<TItem> {
    type Item = TItem;

    fn next(&mut self) -> Option<Self::Item> {
        self.wrapped.next()
    }
}

type ParseResult<TValue, TToken> = Result<ParseOutput<TToken, TValue>, ParseError<TToken>>;

#[derive(Clone, Debug)]
pub enum TokenMatcher {
    Keyword(keywords::Keyword),
    AnyKeyword,
    AnyIdentifier,
    AnyBinaryOperator,
    AnyLiteralInteger,
    AnyLiteralString,
    Exact(tokens::Token),
    OneOf(Vec<Box<TokenMatcher>>),
}

impl fmt::Display for TokenMatcher {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &TokenMatcher::Keyword(kw) => write!(f, "keyword {}", kw),
            &TokenMatcher::AnyKeyword => write!(f, "keyword"),
            &TokenMatcher::AnyIdentifier => write!(f, "identifier"),
            &TokenMatcher::AnyBinaryOperator => write!(f, "binary operator"),
            &TokenMatcher::AnyLiteralInteger => write!(f, "integer literal"),
            &TokenMatcher::AnyLiteralString => write!(f, "string literal"),
            &TokenMatcher::Exact(ref exact_token) => write!(f, "{}", exact_token),
            &TokenMatcher::OneOf(ref matchers) => write!(f, "one of: {}", matchers.iter()
                .map(|matcher| format!("{}", matcher))
                .collect::<Vec<_>>()
                .join(", ")),
        }
    }
}

impl TokenMatcher {
    pub fn match_token<T>(&self, token: &T) -> bool
        where T: tokens::AsToken
    {
        match self {
            &TokenMatcher::Keyword(kw) => token.as_token().is_keyword(kw),
            &TokenMatcher::AnyKeyword => token.as_token().is_any_keyword(),
            &TokenMatcher::AnyIdentifier => token.as_token().is_any_identifier(),
            &TokenMatcher::AnyBinaryOperator => token.as_token().is_any_binary_operator(),
            &TokenMatcher::AnyLiteralInteger => token.as_token().is_any_literal_int(),
            &TokenMatcher::AnyLiteralString => token.as_token().is_any_literal_string(),
            &TokenMatcher::Exact(ref exact_token) => token.as_token() == exact_token,
            &TokenMatcher::OneOf(ref matchers) => matchers.iter()
                .any(|matcher| matcher.match_token(token.as_token())),
        }
    }

    pub fn and_then(self, next_matcher: TokenMatcher) -> SequenceMatcher {
        SequenceMatcher {
            sequence: vec![self, next_matcher]
        }
    }

    pub fn until_match<TIter, TToken>(&self, in_tokens: TIter) -> ParseResult<Vec<TToken>, TToken>
        where TIter: IntoIterator<Item=TToken> + 'static,
              TToken: tokens::AsToken + Clone
    {
        let mut until = Vec::new();
        let mut tokens = in_tokens.into_iter();

        loop {
            match tokens.next() {
                Some(next_token) => {
                    if self.match_token(&next_token) {
                        break;
                    }
                    else {
                        until.push(next_token.clone());
                    }
                },

                None => {
                    //ran out of tokens before finding match
                    return Err(ParseError::UnexpectedEOF);
                },
            }
        }

        Ok(ParseOutput::new(until, tokens))
    }
}

#[derive(Clone, Debug)]
pub struct SequenceMatcher {
    sequence: Vec<TokenMatcher>,
}

impl SequenceMatcher {
    pub fn and_then(mut self, next_matcher: TokenMatcher) -> Self {
        self.sequence.push(next_matcher);
        self
    }

    pub fn match_tokens<TIter, TToken>(&self, in_tokens: TIter) -> ParseResult<Vec<TToken>, TToken>
        where TIter: Iterator<Item=TToken>,
              TToken: tokens::AsToken + Clone + 'static
    {
        let tokens: Vec<_> = in_tokens.collect();

        let expected_len = self.sequence.len();

        let matches: Vec<_> = tokens.iter()
            .zip(self.sequence.iter())
            .map(|(token, matcher)| (token.clone(), matcher.clone(), matcher.match_token(token)))
            .collect();

        if matches.len() < expected_len {
            Err(ParseError::UnexpectedEOF)
        } else {
            let first_fail = matches.iter()
                .find(|&&(_, _, matched)| !matched)
                .cloned();

            match first_fail {
                Some((token, matcher, _)) =>
                    Err(ParseError::UnexpectedToken(token, Some(matcher))),
                None => {
                    let matched_tokens = matches.into_iter()
                        .map(|(token, _, _)| token.clone())
                        .collect::<Vec<_>>();

                    let remaining_tokens = tokens.into_iter()
                        .skip(matched_tokens.len());

                    Ok(ParseOutput::new(matched_tokens, remaining_tokens))
                },
            }
        }
    }
}




