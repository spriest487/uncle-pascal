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
            ":=" => Some(BinaryOperator::Assignment),
            "=" => Some(BinaryOperator::Equals),
            "+" => Some(BinaryOperator::Plus),
            "-" => Some(BinaryOperator::Minus),
            _ => None
        }
    }
}

impl fmt::Display for BinaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", match self {
            &BinaryOperator::Assignment => ":=",
            &BinaryOperator::Equals => "=",
            &BinaryOperator::Plus => "+",
            &BinaryOperator::Minus => "-",
        })
    }
}

impl ToSource for BinaryOperator {
    fn to_source(&self) -> String {
        format!("{}", self)
    }
}
