use std::fmt;
use node::ToSource;

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
    Deref,
    AddressOf,
    And,
    Or,
    Gt,
    Gte,
    Lt,
    Lte,
}

pub static PRECEDENCE: [(Operator, Position); 15] = [
    (Deref, Position::Prefix),
    (AddressOf, Position::Prefix),
    (Plus, Position::Prefix),
    (Minus, Position::Prefix),

    //(Period, Position::Binary),
    (Plus, Position::Binary),
    (Minus, Position::Binary),
    (Equals, Position::Binary),
    (NotEquals, Position::Binary),
    (Gt, Position::Binary),
    (Gte, Position::Binary),
    (Lt, Position::Binary),
    (Lte, Position::Binary),
    (And, Position::Binary),
    (Or, Position::Binary),
    (Assignment, Position::Binary),
];

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

    /* parses operators with english names (and only those operators),
     because these names might also be valid identifiers. the tokenizer
     already knows how to parse the ones which have names which aren't
     valid identifiers*/
    pub fn try_parse_text(text: &str) -> Option<Operator> {
        match text {
            "or" => Some(Or),
            "and" => Some(And),
            _ => None
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
            And => "and",
            Or => "or",
            Gt => ">",
            Gte => ">=",
            Lt => "<",
            Lte => "<=",
        })
    }
}

impl ToSource for Operator {
    fn to_source(&self) -> String {
        format!("{}", self)
    }
}
