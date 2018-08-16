pub mod function;
pub mod var_decl;
pub mod type_decl;
pub mod program;
pub mod block;
pub mod expression;

pub use self::function::*;
pub use self::block::*;
pub use self::var_decl::*;
pub use self::type_decl::*;
pub use self::program::*;
pub use self::expression::*;

use std::fmt;

use keywords;
use tokens;
use tokens::AsToken;

#[derive(Clone, Debug)]
pub enum ParseError<TToken> {
    UnexpectedToken(TToken, Option<Matcher>),
    UnbalancedPair(Matcher, TToken),
    UnexpectedEOF(Matcher, TToken),
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
            &ParseError::UnexpectedEOF(ref expected, ref context) =>
                write!(f, "unexpected end of input: expected {} after {}", expected, context),
        }
    }
}

pub struct ParseOutput<TToken, TValue> {
    pub value: TValue,
    pub last_parsed: TToken,
    pub next: Box<Iterator<Item=TToken>>,
}

impl<TToken, TValue> ParseOutput<TToken, TValue> {
    pub fn new<TNext>(value: TValue, last_parsed: TToken, next: TNext) -> Self
        where TNext: IntoIterator<Item=TToken> + 'static
    {
        Self {
            value,
            last_parsed,
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

    pub fn unwrap(self) -> (TValue, TToken, WrapIter<TToken>) {
        (
            self.value,
            self.last_parsed,
            WrapIter { wrapped: self.next }
        )
    }
}

pub struct WrapIter<TItem> {
    wrapped: Box<Iterator<Item=TItem>>
}

impl<TItem> WrapIter<TItem> {
    pub fn new<TIter>(iter: TIter) -> Self
        where TIter: IntoIterator<Item=TItem> + 'static
    {
        Self { wrapped: Box::from(iter.into_iter()) }
    }
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
    pub before_split: Vec<TToken>,
}

#[allow(dead_code)]
#[derive(Clone, Debug)]
pub enum Matcher {
    Keyword(keywords::Keyword),
    AnyKeyword,
    AnyIdentifier,
    AnyBinaryOperator,
    AnyLiteralInteger,
    AnyLiteralString,
    Exact(tokens::Token),
    OneOf(Vec<Matcher>),
}

impl fmt::Display for Matcher {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &Matcher::Keyword(kw) => write!(f, "{}", tokens::Keyword(kw)),
            &Matcher::AnyKeyword => write!(f, "keyword"),
            &Matcher::AnyIdentifier => write!(f, "identifier"),
            &Matcher::AnyBinaryOperator => write!(f, "binary operator"),
            &Matcher::AnyLiteralInteger => write!(f, "integer literal"),
            &Matcher::AnyLiteralString => write!(f, "string literal"),
            &Matcher::Exact(ref exact_token) => write!(f, "{}", exact_token),
            &Matcher::OneOf(ref matchers) => write!(f, "one of: {}", matchers.iter()
                .map(|matcher| format!("{}", matcher))
                .collect::<Vec<_>>()
                .join(", ")),
        }
    }
}

impl Matcher {
    pub fn is_match<T>(&self, token: &T) -> bool
        where T: tokens::AsToken
    {
        match self {
            &Matcher::Keyword(kw) => token.as_token().is_keyword(kw),
            &Matcher::AnyKeyword => token.as_token().is_any_keyword(),
            &Matcher::AnyIdentifier => token.as_token().is_any_identifier(),
            &Matcher::AnyBinaryOperator => token.as_token().is_any_binary_operator(),
            &Matcher::AnyLiteralInteger => token.as_token().is_any_literal_int(),
            &Matcher::AnyLiteralString => token.as_token().is_any_literal_string(),
            &Matcher::Exact(ref exact_token) => token.as_token() == exact_token,
            &Matcher::OneOf(ref matchers) => matchers.iter()
                .any(|matcher| matcher.is_match(token.as_token())),
        }
    }

    pub fn or(self, or: Matcher) -> Matcher {
        match self {
            Matcher::OneOf(mut options) => {
                options.push(or);
                Matcher::OneOf(options)
            },
            _ => {
                Matcher::OneOf(vec![
                    self,
                    or,
                ])
            }
        }
    }

    pub fn and_then(self, next_matcher: Matcher) -> SequenceMatcher {
        SequenceMatcher {
            sequence: vec![self, next_matcher]
        }
    }

    pub fn paired_with(self, close: Matcher) -> PairMatcher {
        PairMatcher {
            open: self,
            close
        }
    }

    pub fn match_one<TIter>(&self, in_tokens: TIter, context: &TIter::Item) -> ParseResult<TIter::Item, TIter::Item>
        where TIter: IntoIterator + 'static,
              TIter::Item: tokens::AsToken {
        let mut tokens = in_tokens.into_iter();
        match tokens.next() {
            Some(ref token) if self.is_match(token) => {
                Ok(ParseOutput::new(token.clone(), token.clone(), tokens))
            },

            Some(ref unexpected) => {
                Err(ParseError::UnexpectedToken(unexpected.clone(), Some(self.clone())))
            },

            None => {
                Err(ParseError::UnexpectedEOF(self.clone(), context.clone()))
            }
        }
    }

    pub fn match_peek<TIter>(&self, in_tokens: TIter,
                             context: &TIter::Item) -> ParseResult<Option<TIter::Item>, TIter::Item>
        where TIter: IntoIterator + 'static,
              TIter::Item: tokens::AsToken {
        let mut peekable = in_tokens.into_iter().peekable();

        match peekable.peek().cloned() {
            Some(ref token) if self.is_match(token) => {
                Ok(ParseOutput::new(Some(token.clone()),
                                    token.clone(),
                                    peekable))
            },

            Some(ref unexpected) => {
                Ok(ParseOutput::new(None,
                                     unexpected.clone(),
                                     peekable))
            },

            None => {
                Err(ParseError::UnexpectedEOF(self.clone(), context.clone()))
            }
        }
    }

    pub fn split_at_match<TIter>(&self, in_tokens: TIter, context: &TIter::Item) -> ParseResult<SplitResult<TIter::Item>, TIter::Item>
        where TIter: IntoIterator + 'static,
              TIter::Item: tokens::AsToken
    {
        let mut before_split = Vec::new();
        let split_at;

        let mut tokens = in_tokens.into_iter();

        loop {
            match tokens.next() {
                Some(next_token) => {
                    if self.is_match(&next_token) {
                        split_at = next_token.clone();
                        break;
                    }
                    else {
                        before_split.push(next_token.clone());
                    }
                },

                None => {
                    //ran out of tokens before finding match
                    return Err(ParseError::UnexpectedEOF(self.clone(), context.clone()));
                },
            }
        }

        Ok(ParseOutput::new(SplitResult {
            split_at: split_at.clone(),
            before_split: before_split,
        }, split_at, tokens))
    }
}

#[derive(Clone, Debug)]
pub struct SequenceMatcher {
    sequence: Vec<Matcher>,
}

impl SequenceMatcher {
    pub fn and_then(mut self, next_matcher: Matcher) -> Self {
        self.sequence.push(next_matcher);
        self
    }

    pub fn match_sequence<TIter>(&self, in_tokens: TIter, context: &TIter::Item) -> ParseResult<Vec<TIter::Item>, TIter::Item>
        where TIter: Iterator,
              TIter::Item: tokens::AsToken + Clone + 'static
    {
        let tokens: Vec<_> = in_tokens.collect();

        let expected_len = self.sequence.len();

        let matches: Vec<_> = tokens.iter()
            .zip(self.sequence.iter())
            .map(|(token, matcher)| (token.clone(), matcher.clone(), matcher.is_match(token)))
            .collect();

        if matches.len() < expected_len {
            Err(ParseError::UnexpectedEOF(self.sequence[0].clone(), context.clone()))
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

                    let last_parsed = matched_tokens.last().unwrap().clone();

                    Ok(ParseOutput::new(matched_tokens,
                                        last_parsed,
                                        remaining_tokens))
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
    pub inner: Vec<TToken>,
}

type PairMatchResult<TToken> = Result<ParseOutput<TToken, PairMatch<TToken>>, ParseError<TToken>>;

#[derive(Clone, Debug)]
pub struct PairMatcher {
    open: Matcher,
    close: Matcher,
}

impl PairMatcher {
    pub fn match_pair<TIter>(&self, in_tokens: TIter, context: &TIter::Item) -> PairMatchResult<TIter::Item>
        where TIter: IntoIterator + 'static,
              TIter::Item: tokens::AsToken + 'static
    {
        let (open_token, open_last, after_open) = self.open.match_one(in_tokens, context)?.unwrap();

        let eof_as_unbalanced = |err| {
            match err {
                ParseError::UnexpectedEOF(_, _) => {
                    ParseError::UnbalancedPair(self.close.clone(), open_token.clone())
                },
                _ => err,
            }
        };

        let match_delim : Matcher = self.open.clone().or(self.close.clone());
        let (split_at_delim, _, after_next_delim) = match_delim.split_at_match(after_open, &open_last)
            .map_err(&eof_as_unbalanced)?
            .unwrap();

        let next_delim = split_at_delim.split_at.clone();

        if self.open.is_match(next_delim.as_token()) {
            unimplemented!()
//            /* found an inner pair, skip over it */
//            let inner_pair_tokens = vec![next_delim.clone()]
//                .into_iter()
//                .chain(after_next_delim);
//
//            let (_, _, after_inner_pair) = self.match_pair(inner_pair_tokens, &next_delim)
//                .map_err(&eof_as_unbalanced)?
//                .unwrap();
//
//            /* we must now be positioned after any inner pairs */
//            let (final_close_token, _, after_final_closer) = self.close.split_at_match(after_inner_pair, &next_delim)
//                .map_err(&eof_as_unbalanced)?
//                .unwrap();
//
//            let pair_match : PairMatch<TIter::Item> = PairMatch {
//                open: open_token.clone(),
//                close: final_close_token.split_at,
//                inner: unimplemented!()
//            };
//
//            Ok(ParseOutput::new(pair_match,
//                                pair_match.close.clone(),
//                                after_final_closer))
        }
        else if self.close.is_match(next_delim.as_token()) {
            let pair_match : PairMatch<TIter::Item> = PairMatch {
                open: open_token.clone(),
                close: split_at_delim.split_at,
                inner: split_at_delim.before_split,
            };

            let last_parsed = pair_match.close.clone();

            Ok(ParseOutput::new(pair_match,
                                last_parsed,
                                after_next_delim))
        }
        else {
            Err(ParseError::UnexpectedToken(next_delim, Some(match_delim)))
        }
    }
}



