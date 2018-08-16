use std::fmt;

use node::ToSource;

pub use self::Keyword::*;

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Keyword {
    Program,
    Var,
    Let,
    Function,
    Procedure,
    Constructor,
    Destructor,
    Begin,
    End,
    Uses,
    Type,
    Record,
    Class,
    If,
    Then,
    Else,
    Nil,
    For,
    To,
    Do,
    Unit,
    Interface,
    Implementation,
    Array,
    Of,
}

impl Keyword {
    pub fn try_parse(from: &str) -> Option<Self> {
        match from {
            "program" => Some(Program),
            "let" => Some(Let),
            "var" => Some(Var),
            "function" => Some(Function),
            "constructor" => Some(Constructor),
            "destructor" => Some(Destructor),
            "procedure" => Some(Procedure),
            "begin" => Some(Begin),
            "end" => Some(End),
            "uses" => Some(Uses),
            "type" => Some(Type),
            "class" => Some(Class),
            "record" => Some(Record),
            "if" => Some(If),
            "then" => Some(Then),
            "else" => Some(Else),
            "nil" => Some(Nil),
            "for" => Some(For),
            "to" => Some(To),
            "do" => Some(Do),
            "unit" => Some(Unit),
            "interface" => Some(Interface),
            "implementation" => Some(Implementation),
            "array" => Some(Array),
            "of" => Some(Of),
            _ => None,
        }
    }
}

impl ToSource for Keyword {
    fn to_source(&self) -> String {
        match self {
            Program => "program",
            Var => "var",
            Let => "let",
            Function => "function",
            Procedure => "procedure",
            Constructor => "constructor",
            Destructor => "destructor",
            Begin => "begin",
            End => "end",
            Uses => "uses",
            Type => "type",
            Class => "class",
            Record => "record",
            If => "if",
            Then => "then",
            Else => "else",
            Nil => "nil",
            For => "for",
            To => "to",
            Do => "do",
            Unit => "unit",
            Interface => "interface",
            Implementation => "implementation",
            Array => "array",
            Of => "of",
        }.to_string()
    }
}

impl fmt::Display for Keyword {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", ToSource::to_source(self))
    }
}