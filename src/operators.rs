use std::fmt;
use node::ToSource;
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
    Or,
    Gt,
    Gte,
    Lt,
    Lte,
}

pub static PRECEDENCE: [(Operator, Position); 17] = [
    (Deref, Position::Prefix),
    (AddressOf, Position::Prefix),
    (Plus, Position::Prefix),
    (Minus, Position::Prefix),

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
    (And, Position::Binary),
    (Or, Position::Binary),
    (Assignment, Position::Binary),
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
