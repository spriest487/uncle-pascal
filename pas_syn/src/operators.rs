use std::fmt;

#[derive(Eq, PartialEq, Clone, Debug, Copy)]
pub enum Position {
    Prefix,
    Postfix,
    Binary,
}

impl fmt::Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Position::Prefix => "prefix",
                Position::Binary => "binary",
                Position::Postfix => "postfix",
            }
        )
    }
}

#[derive(Eq, PartialEq, Clone, Debug, Copy, Hash)]
pub enum Operator {
    Assignment,
    Equals,
    NotEquals,
    Plus,
    Minus,
    Multiply,
    IntegerDivide,
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
    Member,
    Shl,
    Shr,

    // not used as a syntactical operator, but included so it can easily
    // participate in precedence comparisons
    Call,
    Index,
}

/// canonical operator precedence ordering. operations higher in the list
/// take precedence over ones below them
pub static PRECEDENCE: [(Operator, Position); 25] = [
    (Operator::Member, Position::Binary),
    (Operator::Call, Position::Postfix),
    (Operator::Index, Position::Binary),
    (Operator::Deref, Position::Postfix),
    (Operator::AddressOf, Position::Prefix),
    (Operator::Plus, Position::Prefix),
    (Operator::Minus, Position::Prefix),
    (Operator::Not, Position::Prefix),
    (Operator::Shl, Position::Binary),
    (Operator::Shr, Position::Binary),
    (Operator::Multiply, Position::Binary),
    (Operator::IntegerDivide, Position::Binary),
    (Operator::Plus, Position::Binary),
    (Operator::Minus, Position::Binary),
    (Operator::Equals, Position::Binary),
    (Operator::NotEquals, Position::Binary),
    (Operator::Gt, Position::Binary),
    (Operator::Gte, Position::Binary),
    (Operator::Lt, Position::Binary),
    (Operator::Lte, Position::Binary),
    (Operator::In, Position::Binary),
    (Operator::And, Position::Binary),
    (Operator::Or, Position::Binary),
    (Operator::RangeInclusive, Position::Binary),
    (Operator::Assignment, Position::Binary),
];

impl Operator {
    pub fn for_position(position: Position) -> impl Iterator<Item = Operator> {
        PRECEDENCE
            .iter()
            .filter_map(move |(op, pos)| if *pos == position { Some(*op) } else { None })
    }

    pub fn precedence(self, in_pos: Position) -> usize {
        PRECEDENCE
            .iter()
            .enumerate()
            .find(|(_, (op, pos))| *op == self && *pos == in_pos)
            .map(|(index, _)| index)
            .unwrap_or_else(|| {
                panic!(
                    "operator {} must have a precedence value in position {}",
                    self, in_pos
                )
            })
    }

    pub fn is_valid_in_pos(self, in_pos: Position) -> bool {
        PRECEDENCE
            .iter()
            .any(|&(op, pos)| self == op && in_pos == pos)
    }

    fn try_parse_text_lowercase(from: &str) -> Option<Self> {
        match from {
            "or" => Some(Operator::Or),
            "and" => Some(Operator::And),
            "not" => Some(Operator::Not),
            "in" => Some(Operator::In),
            "shl" => Some(Operator::Shl),
            "shr" => Some(Operator::Shr),
            _ => None,
        }
    }

    // parses operators with english names (and only those operators),
    // because these names might also be valid identifiers. the tokenizer
    // already knows how to parse the ones which have names which aren't
    // valid identifiers
    pub fn try_parse_text(from: &str, case_sensitive: bool) -> Option<Operator> {
        if case_sensitive {
            Self::try_parse_text_lowercase(from)
        } else {
            Self::try_parse_text_lowercase(&from.to_ascii_lowercase())
        }
    }
}

impl fmt::Display for Operator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let op_str = match self {
            Operator::Member => ".",
            Operator::Deref => "^",
            Operator::AddressOf => "@",
            Operator::Assignment => ":=",
            Operator::Equals => "=",
            Operator::NotEquals => "<>",
            Operator::Shl => "shl",
            Operator::Shr => "shr",
            Operator::Plus => "+",
            Operator::Minus => "-",
            Operator::Multiply => "*",
            Operator::IntegerDivide => "/",
            Operator::And => "and",
            Operator::Not => "not",
            Operator::Or => "or",
            Operator::Gt => ">",
            Operator::Gte => ">=",
            Operator::Lt => "<",
            Operator::Lte => "<=",
            Operator::In => "in",
            Operator::RangeInclusive => "..",
            Operator::Call => "(...)",
            Operator::Index => "[...]",
        };

        write!(f, "{}", op_str)
    }
}
