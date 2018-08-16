use std::fmt;

use keywords;
use operators;
use node::ToSource;
use consts::{
    IntConstant,
    FloatConstant,
};

pub use self::Token::*;

#[derive(PartialEq, Clone, Debug)]
pub enum Token {
    Keyword(keywords::Keyword),
    Identifier(String),
    Operator(operators::Operator),
    LiteralInteger(IntConstant),
    LiteralFloat(FloatConstant),
    LiteralString(String),
    Period,
    Colon,
    Semicolon,
    BracketLeft,
    BracketRight,
    Comma,
    SquareBracketLeft,
    SquareBracketRight,
}

pub trait AsToken: Clone + fmt::Debug + fmt::Display {
    fn as_token(&self) -> &Token;

    fn is_token(&self, t: &Token) -> bool {
        *self.as_token() == *t
    }

    fn is_keyword(&self, kw: keywords::Keyword) -> bool {
        match self.as_token() {
            &Token::Keyword(token_kw) => token_kw == kw,
            _ => false,
        }
    }

    fn unwrap_keyword(&self) -> keywords::Keyword {
        match self.as_token() {
            Token::Keyword(kw) => *kw,
            _ => panic!("called unwrap_keyword on {}", self)
        }
    }

    fn is_any_keyword(&self) -> bool {
        match self.as_token() {
            &Token::Keyword(_) => true,
            _ => false,
        }
    }

    fn is_identifier(&self, id: &str) -> bool {
        match self.as_token() {
            Token::Identifier(token_id) => id == token_id,
            _ => false,
        }
    }

    fn is_any_identifier(&self) -> bool {
        match self.as_token() {
            &Token::Identifier(_) => true,
            _ => false,
        }
    }

    fn is_operator(&self, op: operators::Operator) -> bool {
        match self.as_token() {
            &Token::Operator(ref token_op) => *token_op == op,
            _ => false
        }
    }

    fn is_any_operator(&self) -> bool {
        match self.as_token() {
            &Token::Operator(_) => true,
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

    fn is_literal_nil(&self) -> bool {
        self.as_token().is_keyword(keywords::Nil)
    }

    fn unwrap_identifier(&self) -> &str {
        match self.as_token() {
            Identifier(name) => name,
            _ => panic!("calling unwrap_identifier on {}", self),
        }
    }

    fn is_any_literal_float(&self) -> bool {
        match self.as_token() {
            Token::LiteralFloat(_) => true,
            _ => false,
        }
    }

    fn unwrap_operator(&self) -> operators::Operator {
        match self.as_token() {
            Operator(op) => *op,
            _ => panic!("calling unwrap_operator on {}", self),
        }
    }

    fn unwrap_literal_string(&self) -> &str {
        match self.as_token() {
            LiteralString(ref s) => s,
            _ => panic!("calling unwrap_literal_string on {}", self),
        }
    }

    fn unwrap_literal_integer(&self) -> IntConstant {
        match self.as_token() {
            LiteralInteger(i) => *i,
            _ => panic!("calling unwrap_literal_integer on {}", self),
        }
    }

    fn unwrap_literal_float(&self) -> FloatConstant {
        match self.as_token() {
            LiteralFloat(f) => *f,
            _ => panic!("calling unwrap_literal_float on {}", self),
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
            Keyword(kw) => kw.to_source(),
            Identifier(name) => name.clone(),
            Operator(op) => op.to_source(),
            LiteralInteger(i) => format!("{}", i),
            LiteralString(s) => format!("'{}'", s.replace("'", "''")),
            LiteralFloat(f) => format!("{}", f),
            Period => ".".to_string(),
            Colon => ":".to_string(),
            Semicolon => ";".to_string(),
            BracketLeft => "(".to_string(),
            BracketRight => ")".to_string(),
            Comma => ",".to_string(),
            SquareBracketLeft => "[".to_string(),
            SquareBracketRight => "]".to_string(),
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Keyword(kw) => write!(f, "keyword `{}`", kw),
            Identifier(name) => write!(f, "identifier `{}`", name),
            Operator(op) => write!(f, "operator `{}`", op),
            LiteralString(s) => write!(f, "string literal `{}`", s),
            LiteralInteger(i) => write!(f, "integer literal `{}`", i),
            LiteralFloat(val) => write!(f, "float literal `{}`", val.as_f64()),
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

impl From<operators::Operator> for Token {
    fn from(op: operators::Operator) -> Self {
        Operator(op)
    }
}