use std::fmt;

use ToSource;

pub use self::Keyword::*;

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Keyword {
    Program,
    Var,
    Function,
    Begin,
    End,
    Uses,
    Type,
    Record,
    If,
    Then,
    Else,
}

impl Keyword {
    pub fn try_parse(from: &str) -> Option<Self> {
        match from {
            "program" => Some(Program),
            "var" => Some(Var),
            "function" => Some(Function),
            "begin" => Some(Begin),
            "end" => Some(End),
            "uses" => Some(Uses),
            "type" => Some(Type),
            "record" => Some(Record),
            "if" => Some(If),
            "then" => Some(Then),
            "else" => Some(Else),
            _ => None,
        }
    }
}

impl ToSource for Keyword {
    fn to_source(&self) -> String {
        match self {
            &Program => "program",
            &Var => "var",
            &Function => "function",
            &Begin => "begin",
            &End => "end",
            &Uses => "uses",
            &Type => "type",
            &Record => "record",
            &If => "if",
            &Then => "then",
            &Else => "else",
        }.to_owned()
    }
}

impl fmt::Display for Keyword {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", ToSource::to_source(self))
    }
}