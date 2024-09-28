use crate::parse::{Matchable, Matcher, SequenceMatcher};
use std::ops::Add;
use std::ops::BitOr;
use std::fmt;

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub enum Keyword {
    Program,
    Library,
    Var,
    Function,
    Procedure,
    Lambda,
    Begin,
    End,
    Uses,
    Type,
    Record,
    Packed,
    Class,
    Case,
    Match,
    If,
    While,
    Then,
    Else,
    Nil,
    For,
    To,
    DownTo,
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
    Variant,
    Is,
    Break,
    Continue,
    Where,
    Unsafe,
    SizeOf,
    In,
}

impl Keyword {
    fn try_parse_lowercase(from: &str) -> Option<Self> {
        match from {
            "program" => Some(Keyword::Program),
            "library" => Some(Keyword::Library),
            "var" => Some(Keyword::Var),
            "function" => Some(Keyword::Function),
            "procedure" => Some(Keyword::Procedure),
            "lambda" => Some(Keyword::Lambda),
            "begin" => Some(Keyword::Begin),
            "end" => Some(Keyword::End),
            "uses" => Some(Keyword::Uses),
            "type" => Some(Keyword::Type),
            "class" => Some(Keyword::Class),
            "record" => Some(Keyword::Record),
            "packed" => Some(Keyword::Packed),
            "case" => Some(Keyword::Case),
            "match" => Some(Keyword::Match),
            "if" => Some(Keyword::If),
            "while" => Some(Keyword::While),
            "then" => Some(Keyword::Then),
            "else" => Some(Keyword::Else),
            "nil" => Some(Keyword::Nil),
            "for" => Some(Keyword::For),
            "to" => Some(Keyword::To),
            "downto" => Some(Keyword::DownTo),
            "do" => Some(Keyword::Do),
            "unit" => Some(Keyword::Unit),
            "interface" => Some(Keyword::Interface),
            "implementation" => Some(Keyword::Implementation),
            "initialization" => Some(Keyword::Initialization),
            "finalization" => Some(Keyword::Finalization),
            "array" => Some(Keyword::Array),
            "of" => Some(Keyword::Of),
            "const" => Some(Keyword::Const),
            "out" => Some(Keyword::Out),
            "true" => Some(Keyword::True),
            "false" => Some(Keyword::False),
            "set" => Some(Keyword::Set),
            "try" => Some(Keyword::Try),
            "except" => Some(Keyword::Except),
            "finally" => Some(Keyword::Finally),
            "raise" => Some(Keyword::Raise),
            "with" => Some(Keyword::With),
            "exit" => Some(Keyword::Exit),
            "variant" => Some(Keyword::Variant),
            "is" => Some(Keyword::Is),
            "break" => Some(Keyword::Break),
            "continue" => Some(Keyword::Continue),
            "where" => Some(Keyword::Where),
            "unsafe" => Some(Keyword::Unsafe),
            "sizeof" => Some(Keyword::SizeOf),
            "in" => Some(Keyword::In),
            _ => None,
        }
    }

    pub fn try_parse(from: &str, case_sensitive: bool) -> Option<Self> {
        if case_sensitive {
            Self::try_parse_lowercase(from)
        } else {
            Self::try_parse_lowercase(&from.to_ascii_lowercase())
        }
    }
}

impl fmt::Display for Keyword {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(match self {
            Keyword::Program => "program",
            Keyword::Library => "library",
            Keyword::Var => "var",
            Keyword::Function => "function",
            Keyword::Procedure => "procedure",
            Keyword::Lambda => "lambda",
            Keyword::Begin => "begin",
            Keyword::End => "end",
            Keyword::Uses => "uses",
            Keyword::Type => "type",
            Keyword::Class => "class",
            Keyword::Record => "record",
            Keyword::Packed => "packed",
            Keyword::If => "if",
            Keyword::Case => "case",
            Keyword::Match => "match",
            Keyword::While => "while",
            Keyword::Then => "then",
            Keyword::Else => "else",
            Keyword::Nil => "nil",
            Keyword::For => "for",
            Keyword::To => "to",
            Keyword::DownTo => "downto",
            Keyword::Do => "do",
            Keyword::Unit => "unit",
            Keyword::Interface => "interface",
            Keyword::Implementation => "implementation",
            Keyword::Initialization => "initialization",
            Keyword::Finalization => "finalization",
            Keyword::Array => "array",
            Keyword::Of => "of",
            Keyword::Const => "const",
            Keyword::Out => "out",
            Keyword::True => "true",
            Keyword::False => "false",
            Keyword::Set => "set",
            Keyword::Try => "try",
            Keyword::Except => "except",
            Keyword::Finally => "finally",
            Keyword::Raise => "raise",
            Keyword::With => "with",
            Keyword::Exit => "exit",
            Keyword::Variant => "variant",
            Keyword::Is => "is",
            Keyword::Break => "break",
            Keyword::Continue => "continue",
            Keyword::Where => "where",
            Keyword::Unsafe => "unsafe",
            Keyword::SizeOf => "sizeof",
            Keyword::In => "in",
        })
    }
}

impl Matchable for Keyword {
    fn as_matcher(&self) -> Matcher {
        Matcher::Keyword(*self)
    }
}

impl<Rhs> BitOr<Rhs> for Keyword
where 
    Rhs: Into<Matcher>
{
    type Output = Matcher;

    fn bitor(self, rhs: Rhs) -> Self::Output {
        self.as_matcher() | rhs.into()
    }
}

impl<Rhs> Add<Rhs> for Keyword
where
    Rhs: Into<Matcher>
{
    type Output = SequenceMatcher;

    fn add(self, rhs: Rhs) -> Self::Output {
        self.as_matcher() + rhs.into()
    }
}
