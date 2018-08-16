use std::fmt;
use opts::CompileOptions;

pub use self::Operator::*;

#[derive(Eq, PartialEq, Clone, Debug, Copy)]
pub enum Position {
    Prefix,
    Binary,
}

impl fmt::Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", match self {
            &Position::Prefix => "prefix",
            &Position::Binary => "binary"
        })
    }
}

#[derive(Eq, PartialEq, Clone, Debug, Copy)]
pub enum Operator {
    Assignment,
    Equals,
    NotEquals,
    Plus,
    Minus,
    Multiply,
    Divide,
    Deref,
    AddressOf,
    And,
    Not,
    Or,
    Gt,
    Gte,
    Lt,
    Lte,
    RangeInclusive,
    In,
}

pub static PRECEDENCE: [(Operator, Position); 20] = [
    (Deref, Position::Prefix),
    (AddressOf, Position::Prefix),
    (Plus, Position::Prefix),
    (Minus, Position::Prefix),
    (Not, Position::Prefix),

    (Multiply, Position::Binary),
    (Divide, Position::Binary),
    (Plus, Position::Binary),
    (Minus, Position::Binary),
    (Equals, Position::Binary),
    (NotEquals, Position::Binary),
    (Gt, Position::Binary),
    (Gte, Position::Binary),
    (Lt, Position::Binary),
    (Lte, Position::Binary),
    (In, Position::Binary),
    (And, Position::Binary),
    (Or, Position::Binary),
    (Assignment, Position::Binary),
    (RangeInclusive, Position::Binary),
];

pub fn for_position(position: Position) -> impl Iterator<Item = Operator> {
    PRECEDENCE.iter()
        .filter_map(move |(op, pos)| if *pos == position {
            Some(*op)
        } else {
            None
        })
}

impl Operator {
    pub fn precedence(&self, in_pos: Position) -> usize {
        PRECEDENCE.iter().enumerate()
            .find(|&(_, &(op, pos))| op.eq(self) && pos == in_pos)
            .map(|(index, _)| index)
            .unwrap_or_else(|| {
                panic!("operator {} must have a precedence value in position {}", self, in_pos)
            })
    }

    pub fn is_valid_in_pos(&self, in_pos: Position) -> bool {
        PRECEDENCE.iter()
            .any(|&(op, pos)| *self == op && in_pos == pos)
    }

    fn try_parse_text_lowercase(from: &str) -> Option<Self> {
        match from {
            "or" => Some(Or),
            "and" => Some(And),
            "not" => Some(Not),
            "in" => Some(In),
            _ => None
        }
    }

    /* parses operators with english names (and only those operators),
     because these names might also be valid identifiers. the tokenizer
     already knows how to parse the ones which have names which aren't
     valid identifiers*/
    pub fn try_parse_text(from: &str, opts: &CompileOptions) -> Option<Operator> {
        match opts.case_sensitive() {
            true => Self::try_parse_text_lowercase(from),
            false => Self::try_parse_text_lowercase(&from.to_ascii_lowercase()),
        }
    }
}

impl fmt::Display for Operator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", match self {
            Deref => "^",
            AddressOf => "@",
            Assignment => ":=",
            Equals => "=",
            NotEquals => "<>",
            Plus => "+",
            Minus => "-",
            Multiply => "*",
            Divide => "/",
            And => "and",
            Not => "not",
            Or => "or",
            Gt => ">",
            Gte => ">=",
            Lt => "<",
            Lte => "<=",
            In => "in",
            RangeInclusive => "..",
        })
    }
}

