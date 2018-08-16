use std::fmt;

use node::ToSource;

pub use self::Keyword::*;

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Keyword {
    Program,
    Var,
    Function,
    Procedure,
    Begin,
    End,
    Uses,
    Type,
    Record,
    If,
    Then,
    Else,
    Nil,
    For,
    To,
    Do,
}

impl Keyword {
    pub fn try_parse(from: &str) -> Option<Self> {
        match from {
            "program" => Some(Program),
            "var" => Some(Var),
            "function" => Some(Function),
            "procedure" => Some(Procedure),
            "begin" => Some(Begin),
            "end" => Some(End),
            "uses" => Some(Uses),
            "type" => Some(Type),
            "record" => Some(Record),
            "if" => Some(If),
            "then" => Some(Then),
            "else" => Some(Else),
            "nil" => Some(Nil),
            "for" => Some(For),
            "to" => Some(To),
            "do" => Some(Do),
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
            &Procedure => "procedure",
            &Begin => "begin",
            &End => "end",
            &Uses => "uses",
            &Type => "type",
            &Record => "record",
            &If => "if",
            &Then => "then",
            &Else => "else",
            &Nil => "nil",
            &For => "for",
            &To => "to",
            &Do => "do",
        }.to_owned()
    }
}

impl fmt::Display for Keyword {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", ToSource::to_source(self))
    }
}