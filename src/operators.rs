use std::fmt;
use node::ToSource;

pub use self::BinaryOperator::*;

#[derive(Eq, PartialEq, Clone, Debug)]
pub enum BinaryOperator {
    Assignment,
    Equals,
    NotEquals,
    Plus,
    Minus,
}

pub static PRECEDENCE: [BinaryOperator; 5] = [
    Plus,
    Minus,
    Equals,
    NotEquals,
    Assignment,
];

impl fmt::Display for BinaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", match self {
            &Assignment => ":=",
            &Equals => "=",
            &NotEquals => "<>",
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
