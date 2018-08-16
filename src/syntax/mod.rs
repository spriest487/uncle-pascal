pub mod function;
pub mod var_decl;
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

pub struct SplitResult<TToken> {
    pub split_at: TToken,
    pub before: Vec<TToken>,
}

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

    pub fn or(self, or: TokenMatcher) -> TokenMatcher {
        match self {
            TokenMatcher::OneOf(mut options) => {
                options.push(Box::from(or));
                TokenMatcher::OneOf(options)
            },
            _ => {
                TokenMatcher::OneOf(vec![
                    Box::new(self),
                    Box::new(or),
                ])
            }
        }
    }

    pub fn and_then(self, next_matcher: TokenMatcher) -> SequenceMatcher {
        SequenceMatcher {
            sequence: vec![self, next_matcher]
        }
    }

    pub fn closed_with(self, close: TokenMatcher) -> PairMatcher {
        PairMatcher {
            open: self,
            close
        }
    }

    pub fn match_one<TIter, TToken>(&self, in_tokens: TIter) -> ParseResult<TToken, TToken>
        where TIter: IntoIterator<Item=TToken> + 'static,
              TToken: tokens::AsToken {
        let mut tokens = in_tokens.into_iter();
        match tokens.next() {
            Some(ref token) if self.match_token(token) => {
                Ok(ParseOutput::new(token.clone(), tokens))
            },

            Some(ref unexpected) => {
                Err(ParseError::UnexpectedToken(unexpected.clone(), Some(self.clone())))
            },

            None => {
                Err(ParseError::UnexpectedEOF)
            }
        }
    }

    pub fn match_peek<TIter, TToken>(&self, in_tokens: TIter) -> ParseResult<TToken, TToken>
        where TIter: IntoIterator<Item=TToken> + 'static,
              TToken: tokens::AsToken {
        let mut peekable = in_tokens.into_iter().peekable();

        match peekable.peek().cloned() {
            Some(ref token) if self.match_token(token) => {
                Ok(ParseOutput::new(token.clone(), peekable))
            },

            Some(ref unexpected) => {
                Err(ParseError::UnexpectedToken(unexpected.clone(), Some(self.clone())))
            },

            None => {
                Err(ParseError::UnexpectedEOF)
            }
        }
    }

    pub fn split<TIter, TToken>(&self, in_tokens: TIter) -> ParseResult<SplitResult<TToken>, TToken>
        where TIter: IntoIterator<Item=TToken> + 'static,
              TToken: tokens::AsToken
    {
        let mut before_split = Vec::new();
        let split_at;

        let mut tokens = in_tokens.into_iter();

        loop {
            match tokens.next() {
                Some(next_token) => {
                    if self.match_token(&next_token) {
                        split_at = next_token.clone();
                        break;
                    }
                    else {
                        before_split.push(next_token.clone());
                    }
                },

                None => {
                    //ran out of tokens before finding match
                    return Err(ParseError::UnexpectedEOF);
                },
            }
        }

        Ok(ParseOutput::new(SplitResult {
            split_at: split_at,
            before: before_split,
        }, tokens))
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

#[derive(Clone, Debug)]
pub struct PairMatch<TToken>
    where TToken: Clone + fmt::Debug
{
    pub open: TToken,
    pub close: TToken,
    pub between: Vec<TToken>,
}

#[derive(Clone, Debug)]
pub struct PairMatcher {
    open: TokenMatcher,
    close: TokenMatcher,
}

impl PairMatcher {
    pub fn match_pair<TIter, TToken>(&self, in_tokens: TIter) -> ParseResult<PairMatch<TToken>, TToken>
        where TIter: IntoIterator<Item=TToken> + 'static,
              TToken: tokens::AsToken + 'static
    {
        let (open_token, after_open) = self.open.match_one(in_tokens)?.unwrap();

        let match_delim : TokenMatcher = self.open.clone().or(self.close.clone());
        let (split_at_delim, after_next_delim) = match_delim.split(after_open)?.unwrap();

        let next_delim = split_at_delim.split_at.clone();

        if self.open.match_token(next_delim.as_token()) {
//            let mut inner_tokens = Vec::new();

            /* found an inner pair, skip over it */
            let inner_pair_tokens = vec![next_delim.clone()]
                .into_iter()
                .chain(after_next_delim);

            let (_, after_inner_pair) = self.match_pair(inner_pair_tokens)?.unwrap();

            /* we must now be positioned after any inner pairs */
            let (final_close_token, after_final_closer) = self.close.split(after_inner_pair)
                .map_err(|err| match err {
                    ParseError::UnexpectedEOF => {
                        ParseError::UnbalancedPair(self.close.clone(), next_delim.clone())
                    },
                    _ => err
                })?
                .unwrap();

            let pair_match : PairMatch<TToken> = PairMatch {
                open: open_token,
                close: final_close_token.split_at,
                between: Vec::new() //TODO
            };

            Ok(ParseOutput::new(pair_match, after_final_closer))
        }
        else if self.close.match_token(next_delim.as_token()) {
            let pair_match : PairMatch<TToken> = PairMatch {
                open: open_token,
                close: split_at_delim.split_at,
                between: split_at_delim.before,
            };

            Ok(ParseOutput::new(pair_match, after_next_delim))
        }
        else {
            Err(ParseError::UnexpectedToken(next_delim, Some(match_delim)))
        }
    }
}



