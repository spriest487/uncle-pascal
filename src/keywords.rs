use std::fmt;

pub use self::Keyword::*;
use opts::CompileOptions;

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Keyword {
    Program,
    Var,
    Let,
    Function,
    Procedure,
    Begin,
    End,
    Uses,
    Type,
    Record,
    Class,
    Case,
    If,
    While,
    Then,
    Else,
    Nil,
    For,
    To,
    Do,
    Unit,
    Interface,
    Implementation,
    Initialization,
    Finalization,
    Array,
    Of,
    Const,
    Out,
    True,
    False,
    Set,
    Try,
    Except,
    Finally,
    Raise,
    With,
    Exit,
}

impl Keyword {
    fn try_parse_lowercase(from: &str) -> Option<Self> {
        match from {
            "program" => Some(Program),
            "let" => Some(Let),
            "var" => Some(Var),
            "function" => Some(Function),
            "procedure" => Some(Procedure),
            "begin" => Some(Begin),
            "end" => Some(End),
            "uses" => Some(Uses),
            "type" => Some(Type),
            "class" => Some(Class),
            "record" => Some(Record),
            "case" => Some(Case),
            "if" => Some(If),
            "while" => Some(While),
            "then" => Some(Then),
            "else" => Some(Else),
            "nil" => Some(Nil),
            "for" => Some(For),
            "to" => Some(To),
            "do" => Some(Do),
            "unit" => Some(Unit),
            "interface" => Some(Interface),
            "implementation" => Some(Implementation),
            "initialization" => Some(Initialization),
            "finalization" => Some(Finalization),
            "array" => Some(Array),
            "of" => Some(Of),
            "const" => Some(Const),
            "out" => Some(Out),
            "true" => Some(True),
            "false" => Some(False),
            "set" => Some(Set),
            "try" => Some(Try),
            "except" => Some(Except),
            "finally" => Some(Finally),
            "raise" => Some(Raise),
            "with" => Some(With),
            "exit" => Some(Exit),
            _ => None,
        }
    }

    pub fn try_parse(from: &str, opts: &CompileOptions) -> Option<Self> {
        if opts.case_sensitive() {
            Self::try_parse_lowercase(from)
        } else {
            Self::try_parse_lowercase(&from.to_ascii_lowercase())
        }
    }
}

impl fmt::Display for Keyword {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(match self {
            Program => "program",
            Var => "var",
            Let => "let",
            Function => "function",
            Procedure => "procedure",
            Begin => "begin",
            End => "end",
            Uses => "uses",
            Type => "type",
            Class => "class",
            Record => "record",
            If => "if",
            Case => "case",
            While => "while",
            Then => "then",
            Else => "else",
            Nil => "nil",
            For => "for",
            To => "to",
            Do => "do",
            Unit => "unit",
            Interface => "interface",
            Implementation => "implementation",
            Initialization => "initialization",
            Finalization => "finalization",
            Array => "array",
            Of => "of",
            Const => "const",
            Out => "out",
            True => "true",
            False => "false",
            Set => "set",
            Try => "try",
            Except => "except",
            Finally => "finally",
            Raise => "raise",
            With => "with",
            Exit => "exit",
        })
    }
}