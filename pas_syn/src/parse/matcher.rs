use {
    std::fmt,
    crate::{
        token_tree::*,
        operators::*,
        keyword::*,
    }
};

#[derive(Clone, Debug)]
pub enum Matcher {
    Keyword(Keyword),
    Operator(Operator),
    Separator(Separator),
    Ident(String),
    Delimited(DelimiterPair),
    AnyKeyword,
    AnyIdent,
    AnyOperator,
    AnyLiteralInteger,
    AnyLiteralString,
    AnyLiteralReal,
    AnyLiteralBoolean,
    Exact(TokenTree),
    OneOf(Vec<Matcher>),
}

impl From<TokenTree> for Matcher {
    fn from(token: TokenTree) -> Self {
        Matcher::Exact(token)
    }
}

impl From<Operator> for Matcher {
    fn from(op: Operator) -> Self {
        Matcher::Operator(op)
    }
}

impl From<Separator> for Matcher {
    fn from(sep: Separator) -> Self {
        Matcher::Separator(sep)
    }
}

impl From<DelimiterPair> for Matcher {
    fn from(delim: DelimiterPair) -> Self {
        Matcher::Delimited(delim)
    }
}

impl From<Keyword> for Matcher {
    fn from(keyword: Keyword) -> Self {
        Matcher::Keyword(keyword)
    }
}

impl<'a> From<&'a str> for Matcher {
    fn from(name: &str) -> Self {
        Matcher::Ident(name.to_string())
    }
}

pub trait MatchOneOf {
    fn or(self, next: impl Into<Matcher>) -> Matcher;
}

impl<M: Into<Matcher>> MatchOneOf for M {
    fn or(self, next: impl Into<Matcher>) -> Matcher{
        self.into().or(next.into())
    }
}

pub trait MatchSequenceOf {
    fn and_then(self, next: impl Into<Matcher>) -> SequenceMatcher;
}

impl<M: Into<Matcher>> MatchSequenceOf for M {
    fn and_then(self, next: impl Into<Matcher>) -> SequenceMatcher {
        self.into().and_then(next.into())
    }
}

impl fmt::Display for Matcher {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Matcher::Keyword(kw) => write!(f, "{}", kw),
            Matcher::Operator(op) => write!(f, "{}", op),
            Matcher::Separator(sep) => write!(f, "{}", sep),
            Matcher::Ident(name) => f.write_str(name),
            Matcher::Delimited(delim) => write!(f, "{}-delimited list", delim),
            Matcher::AnyKeyword => write!(f, "keyword"),
            Matcher::AnyIdent => write!(f, "identifier"),
            Matcher::AnyOperator => write!(f, "binary operator"),
            Matcher::AnyLiteralInteger => write!(f, "integer literal"),
            Matcher::AnyLiteralString => write!(f, "string literal"),
            Matcher::AnyLiteralReal => write!(f, "floating point literal"),
            Matcher::AnyLiteralBoolean => write!(f, "boolean literal"),
            Matcher::Exact(exact_token) => write!(f, "{}", exact_token),
            Matcher::OneOf(matchers) => write!(f, "one of: {}", matchers.iter()
                .map(|matcher| format!("{}", matcher))
                .collect::<Vec<_>>()
                .join(", ")),
        }
    }
}

impl Matcher {
    pub fn any_operator_in_position(pos: Position) -> Self {
        Matcher::OneOf(Operator::for_position(pos)
            .map(Matcher::Operator)
            .collect())
    }

    pub fn is_match(&self, token: &TokenTree) -> bool {
        match self {
            Matcher::Separator(sep) => token.is_separator(*sep),
            Matcher::Delimited(delim) => token.is_delimited(*delim),
            Matcher::Keyword(kw) => token.is_keyword(*kw),
            Matcher::AnyKeyword => token.as_keyword().is_some(),
            Matcher::Operator(op) => token.is_operator(*op),
            Matcher::Ident(name) => token.is_ident(name),
            Matcher::AnyIdent => token.as_ident().is_some(),
            Matcher::AnyOperator => token.as_operator().is_some(),
            Matcher::AnyLiteralInteger => token.as_literal_int().is_some(),
            Matcher::AnyLiteralReal => token.as_literal_real().is_some(),
            Matcher::AnyLiteralString => token.as_literal_string().is_some(),
            Matcher::AnyLiteralBoolean => token.is_keyword(Keyword::True) ||
                token.is_keyword(Keyword::False),
            Matcher::Exact(exact_token) => *token == *exact_token,
            Matcher::OneOf(matchers) => matchers.iter()
                .any(|matcher| matcher.is_match(token)),
        }
    }

    pub fn or(self, or: impl Into<Matcher>) -> Matcher {
        match (self, or.into()) {
            (Matcher::OneOf(mut options), Matcher::OneOf(others)) => {
                options.extend(others);
                Matcher::OneOf(options)
            }

            (Matcher::OneOf(mut options), other @ _) => {
                options.push(other);
                Matcher::OneOf(options)
            }

            (this, Matcher::OneOf(mut others)) => {
                others.insert(0, this);
                Matcher::OneOf(others)
            }

            (this, other) => {
                Matcher::OneOf(vec![this, other])
            }
        }
    }

    pub fn and_then(self, next_matcher: impl Into<Matcher>) -> SequenceMatcher {
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

impl Matchable for TokenTree {
    fn as_matcher(&self) -> Matcher {
        Matcher::Exact(self.clone())
    }
}

impl Matchable for Keyword {
    fn as_matcher(&self) -> Matcher {
        Matcher::Keyword(*self)
    }
}

//impl Matchable for operators::Operator {
//    fn as_matcher(&self) -> Matcher {
//        Matcher::Operator(*self)
//    }
//}

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
    pub fn and_then(mut self, next_matcher: impl Into<Matcher>) -> Self {
        self.sequence.push(next_matcher.into());
        self
    }
}
