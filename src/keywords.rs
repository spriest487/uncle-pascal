use std::fmt;

use tokens;

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
}

impl Keyword {
    pub fn try_parse(from: &str) -> Option<Self> {
        match from {
            "program" => Some(Keyword::Program),
            "var" => Some(Keyword::Var),
            "function" => Some(Keyword::Function),
            "begin" => Some(Keyword::Begin),
            "end" => Some(Keyword::End),
            "uses" => Some(Keyword::Uses),
            "type" => Some(Keyword::Type),
            "record" => Some(Keyword::Record),
            _ => None,
        }
    }
}

impl tokens::ToSource for Keyword {
    fn to_source(&self) -> String {
        match self {
            &Keyword::Program => "program",
            &Keyword::Var => "var",
            &Keyword::Function => "function",
            &Keyword::Begin => "begin",
            &Keyword::End => "end",
            &Keyword::Uses => "uses",
            &Keyword::Type => "type",
            &Keyword::Record => "record",
        }.to_owned()
    }
}

impl fmt::Display for Keyword {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", tokens::ToSource::to_source(self))
    }
}