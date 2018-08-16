use std::fmt;
use ToSource;

pub use self::BinaryOperator::*;

#[derive(Eq, PartialEq, Clone, Debug)]
pub enum BinaryOperator {
    Assignment,
    Equals,
    Plus,
    Minus,
}

impl BinaryOperator {
    pub fn try_parse(from: &str) -> Option<Self> {
        match from {
            ":=" => Some(Assignment),
            "=" => Some(Equals),
            "+" => Some(Plus),
            "-" => Some(Minus),
            _ => None
        }
    }
}

impl fmt::Display for BinaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", match self {
            &Assignment => ":=",
            &Equals => "=",
            &Plus => "+",
            &Minus => "-",
        })
    }
}

impl ToSource for BinaryOperator {
    fn to_source(&self) -> String {
        format!("{}", self)
    }
}
