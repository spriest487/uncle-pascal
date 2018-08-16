use std::fmt;

use keywords;
use operators;
use tokens;
use source;

pub struct SplitResult {
    pub split_at: source::Token,
    pub before_split: Vec<source::Token>,
}

#[allow(dead_code)]
#[derive(Clone, Debug)]
pub enum Matcher {
    Keyword(keywords::Keyword),
    Operator(operators::Operator),
    AnyKeyword,
    AnyIdentifier,
    AnyOperator,
    AnyLiteralInteger,
    AnyLiteralString,
    AnyLiteralFloat,
    AnyLiteralBoolean,
    Exact(tokens::Token),
    OneOf(Vec<Matcher>),
}

impl From<tokens::Token> for Matcher {
    fn from(token: tokens::Token) -> Self {
        Matcher::Exact(token)
    }
}

impl From<operators::Operator> for Matcher {
    fn from(op: operators::Operator) -> Self {
        Matcher::Operator(op)
    }
}

impl From<keywords::Keyword> for Matcher {
    fn from(keyword: keywords::Keyword) -> Self {
        Matcher::Keyword(keyword)
    }
}

pub trait MatchOneOf {
    fn or<T>(self, next: T) -> Matcher where T: Into<Matcher>;
}

pub trait MatchSequenceOf {
    fn and_then<T>(self, next: T) -> SequenceMatcher where T: Into<Matcher>;
}

impl<TMatchable> MatchOneOf for TMatchable where TMatchable: Into<Matcher> {
    fn or<T>(self, next: T) -> Matcher where T: Into<Matcher> {
        self.into().or(next.into())
    }
}

impl<TMatchable> MatchSequenceOf for TMatchable where TMatchable: Into<Matcher> {
    fn and_then<T>(self, next: T) -> SequenceMatcher where T: Into<Matcher> {
        self.into().and_then(next.into())
    }
}

impl fmt::Display for Matcher {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &Matcher::Keyword(kw) => write!(f, "{}", tokens::Keyword(kw)),
            &Matcher::Operator(ref op) => write!(f, "{}", op),
            &Matcher::AnyKeyword => write!(f, "keyword"),
            &Matcher::AnyIdentifier => write!(f, "identifier"),
            &Matcher::AnyOperator => write!(f, "binary operator"),
            &Matcher::AnyLiteralInteger => write!(f, "integer literal"),
            &Matcher::AnyLiteralString => write!(f, "string literal"),
            &Matcher::AnyLiteralFloat => write!(f, "floating point literal"),
            &Matcher::AnyLiteralBoolean => write!(f, "boolean literal"),
            &Matcher::Exact(ref exact_token) => write!(f, "{}", exact_token),
            &Matcher::OneOf(ref matchers) => write!(f, "one of: {}", matchers.iter()
                .map(|matcher| format!("{}", matcher))
                .collect::<Vec<_>>()
                .join(", ")),
        }
    }
}

impl Matcher {
    pub fn any_operator_in_position(pos: operators::Position) -> Self {
        Matcher::OneOf(operators::for_position(pos)
            .map(|op| Matcher::Operator(op))
            .collect())
    }

    pub fn is_match<T>(&self, token: &T) -> bool
        where T: tokens::AsToken
    {
        match self {
            Matcher::Keyword(kw) => token.is_keyword(*kw),
            Matcher::AnyKeyword => token.is_any_keyword(),
            Matcher::Operator(op) => token.is_operator(*op),
            Matcher::AnyIdentifier => token.is_any_identifier(),
            Matcher::AnyOperator => token.is_any_operator(),
            Matcher::AnyLiteralInteger => token.is_any_literal_int(),
            Matcher::AnyLiteralFloat => token.is_any_literal_float(),
            Matcher::AnyLiteralString => token.is_any_literal_string(),
            Matcher::AnyLiteralBoolean => token.is_keyword(keywords::True) ||
                token.is_keyword(keywords::False),
            Matcher::Exact(exact_token) => token.as_token() == exact_token,
            Matcher::OneOf(matchers) => matchers.iter()
                .any(|matcher| matcher.is_match(token)),
        }
    }

    pub fn or<TMatchable>(self, or: TMatchable) -> Matcher
        where TMatchable: Into<Matcher>
    {
        match self {
            Matcher::OneOf(mut options) => {
                options.push(or.into());
                Matcher::OneOf(options)
            }
            _ => {
                Matcher::OneOf(vec![
                    self,
                    or.into(),
                ])
            }
        }
    }

    pub fn and_then<TMatchable>(self, next_matcher: TMatchable) -> SequenceMatcher
        where TMatchable: Into<Matcher>
    {
        SequenceMatcher {
            sequence: vec![self, next_matcher.into()]
        }
    }
}

pub trait Matchable {
    fn as_matcher(&self) -> Matcher;
}

impl Matchable for Matcher {
    fn as_matcher(&self) -> Matcher {
        self.clone()
    }
}

impl Matchable for tokens::Token {
    fn as_matcher(&self) -> Matcher {
        Matcher::Exact(self.clone())
    }
}

impl Matchable for keywords::Keyword {
    fn as_matcher(&self) -> Matcher {
        Matcher::Keyword(self.clone())
    }
}

impl Matchable for operators::Operator {
    fn as_matcher(&self) -> Matcher {
        Matcher::Operator(self.clone())
    }
}

#[derive(Clone, Debug)]
pub struct SequenceMatcher {
    sequence: Vec<Matcher>,
}

impl IntoIterator for SequenceMatcher {
    type Item = Matcher;
    type IntoIter = <Vec<Matcher> as IntoIterator>::IntoIter;

    fn into_iter(self) -> <Self as IntoIterator>::IntoIter {
        self.sequence.into_iter()
    }
}

impl SequenceMatcher {
    pub fn and_then<TMatchable>(mut self, next_matcher: TMatchable) -> Self
        where TMatchable: Into<Matcher>
    {
        self.sequence.push(next_matcher.into());
        self
    }
}

#[derive(Clone, Debug)]
pub struct BlockMatch {
    pub open: source::Token,
    pub close: source::Token,
    pub inner: Vec<source::Token>,
}

impl BlockMatch {
    pub fn len(&self) -> usize {
        self.inner.len() + 2
    }
}

#[derive(Clone, Debug)]
pub struct GroupMatch {
    pub tokens: Vec<source::Token>,
    pub context: source::Token,
}

#[derive(Clone, Debug)]
pub struct GroupsMatch {
    pub groups: Vec<GroupMatch>,
    pub separators: Vec<source::Token>,
}

#[derive(Clone, Debug)]
pub struct BlockGroupsMatch {
    pub open: source::Token,
    pub close: source::Token,
    pub groups: Vec<GroupMatch>,
    pub separators: Vec<source::Token>,
}
