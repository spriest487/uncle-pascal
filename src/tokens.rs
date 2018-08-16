use std::fmt;

use keywords;
use operators;

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

pub trait ToSource {
    fn to_source(&self) -> String;
}

impl ToSource for Token {
    fn to_source(&self) -> String {
        match self {
            &Token::Keyword(kw) => kw.to_source(),
            &Token::Identifier(ref name) => name.clone(),
            &Token::BinaryOperator(ref op) => op.to_source(),
            &Token::LiteralInteger(i) => format!("{}", i),
            &Token::LiteralString(ref s) => format!("'{}'", s.replace("'", "''")),
            &Token::Period => ".".to_owned(),
            &Token::Colon => ":".to_owned(),
            &Token::Semicolon => ";".to_owned(),
            &Token::BracketLeft => "(".to_owned(),
            &Token::BracketRight => ")".to_owned(),
            &Token::Comma => ",".to_owned(),
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.to_source())
    }
}

impl Token {
    pub fn is_keyword(&self, kw: keywords::Keyword) -> bool {
        match self {
            &Token::Keyword(token_kw) => token_kw == kw,
            _ => false,
        }
    }

    pub fn is_any_keyword(&self) -> bool {
        match self {
            &Token::Keyword(_) => true,
            _ => false,
        }
    }

    pub fn is_any_identifier(&self) -> bool {
        match self {
            &Token::Identifier(_) => true,
            _ => false,
        }
    }

    pub fn is_any_binary_operator(&self) -> bool {
        match self {
            &Token::BinaryOperator(_) => true,
            _ => false,
        }
    }

    pub fn is_any_literal_string(&self) -> bool {
        match self {
            &Token::LiteralString(_) => true,
            _ => false,
        }
    }

    pub fn is_any_literal_int(&self) -> bool {
        match self {
            &Token::LiteralInteger(_) => true,
            _ => false,
        }
    }
}