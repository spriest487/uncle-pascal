use std::fmt;

use keywords;
use operators;
use ToSource;

pub use self::Token::*;

#[derive(Eq, PartialEq, Clone, Debug)]
pub enum Token {
    Keyword(keywords::Keyword),
    Identifier(String),
    BinaryOperator(operators::BinaryOperator),
    LiteralInteger(i64),
    LiteralString(String),
    Period,
    Colon,
    Semicolon,
    BracketLeft,
    BracketRight,
    Comma,
}

pub trait AsToken : Clone + fmt::Debug + fmt::Display {
    fn as_token(&self) -> &Token;

    fn is_keyword(&self, kw: keywords::Keyword) -> bool {
        match self.as_token() {
            &Token::Keyword(token_kw) => token_kw == kw,
            _ => false,
        }
    }

    fn is_any_keyword(&self) -> bool {
        match self.as_token() {
            &Token::Keyword(_) => true,
            _ => false,
        }
    }

    fn is_any_identifier(&self) -> bool {
        match self.as_token() {
            &Token::Identifier(_) => true,
            _ => false,
        }
    }

    fn is_binary_operator(&self, op: &operators::BinaryOperator) -> bool {
        match self.as_token() {
            &Token::BinaryOperator(ref token_op) => token_op.eq(op),
            _ => false
        }
    }

    fn is_any_binary_operator(&self) -> bool {
        match self.as_token() {
            &Token::BinaryOperator(_) => true,
            _ => false,
        }
    }

    fn is_any_literal_string(&self) -> bool {
        match self.as_token() {
            &Token::LiteralString(_) => true,
            _ => false,
        }
    }

    fn is_any_literal_int(&self) -> bool {
        match self.as_token() {
            &Token::LiteralInteger(_) => true,
            _ => false,
        }
    }

    fn unwrap_identifier(&self) -> &str {
        match self.as_token() {
            &Identifier(ref name) => name,
            _ => panic!("calling unwrap_identifier on {}", self),
        }
    }

    fn unwrap_binary_operator(&self) -> &operators::BinaryOperator {
        match self.as_token() {
            &BinaryOperator(ref op) => op,
            _ => panic!("calling unwrap_binary_operator on {}", self),
        }
    }

    fn unwrap_literal_string(&self) -> &str {
        match self.as_token() {
            &LiteralString(ref s) => s,
            _ => panic!("calling unwrap_literal_string on {}", self),
        }
    }

    fn unwrap_literal_integer(&self) -> i64 {
        match self.as_token() {
            &LiteralInteger(ref i) => *i,
            _ => panic!("calling unwrap_literal_integer on {}", self),
        }
    }
}

impl AsToken for Token {
    fn as_token(&self) -> &Token {
        self
    }
}

impl ToSource for Token {
    fn to_source(&self) -> String {
        match self {
            &Keyword(kw) => kw.to_source(),
            &Identifier(ref name) => name.clone(),
            &BinaryOperator(ref op) => op.to_source(),
            &LiteralInteger(i) => format!("{}", i),
            &LiteralString(ref s) => format!("'{}'", s.replace("'", "''")),
            &Period => ".".to_owned(),
            &Colon => ":".to_owned(),
            &Semicolon => ";".to_owned(),
            &BracketLeft => "(".to_owned(),
            &BracketRight => ")".to_owned(),
            &Comma => ",".to_owned(),
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &Keyword(kw) => write!(f, "keyword `{}`", kw),
            &Identifier(ref name) => write!(f, "identifier `{}`", name),
            &BinaryOperator(ref op) => write!(f, "binary operator `{}`", op),
            &LiteralString(ref s) => write!(f, "string literal '{}'", s),
            &LiteralInteger(i) => write!(f, "integer literal '{}'", i),
            _ => write!(f, "{}", self.to_source())
        }
    }
}

impl<T> ToSource for Vec<T>
    where T: AsToken
{
    fn to_source(&self) -> String {
        self.iter()
            .map(|t| t.as_token().to_source())
            .collect::<Vec<_>>()
            .join(" ")
    }
}