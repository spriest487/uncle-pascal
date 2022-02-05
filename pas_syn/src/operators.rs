use std::fmt;

#[derive(Eq, PartialEq, Clone, Debug, Copy)]
pub enum Position {
    Prefix,
    Postfix,
    Binary,
}

impl fmt::Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let name = match self {
            Position::Prefix => "prefix",
            Position::Binary => "binary",
            Position::Postfix => "postfix",
        };

        write!(f, "{}", name)
    }
}

#[derive(Eq, PartialEq, Clone, Debug, Copy, Hash)]
pub enum Operator {
    Assignment,
    CompoundAssignment(CompoundAssignmentOperator),

    Add,
    Subtract,
    Multiply,
    Divide,

    Deref,
    AddressOf,

    As,

    And,
    Not,
    Or,

    Equals,
    NotEquals,
    Gt,
    Gte,
    Lt,
    Lte,

    RangeInclusive,

    In,

    Member,
    Index,

    Shl,
    Shr,

    // not used as a syntactical operator, but included so it can easily
    // participate in precedence comparisons
    Call,
}

#[derive(Eq, PartialEq, Clone, Debug, Copy, Hash)]
pub enum CompoundAssignmentOperator {
    AddAssign,
    SubtractAssign,
    MultiplyAssign,
    DivideAssign,
}

impl CompoundAssignmentOperator {
    pub fn binary_operator(self) -> Operator {
        match self {
            CompoundAssignmentOperator::AddAssign => Operator::Add,
            CompoundAssignmentOperator::SubtractAssign => Operator::Subtract,
            CompoundAssignmentOperator::MultiplyAssign => Operator::Multiply,
            CompoundAssignmentOperator::DivideAssign => Operator::Divide,
        }
    }
}

impl From<CompoundAssignmentOperator> for Operator {
    fn from(op: CompoundAssignmentOperator) -> Self {
        Operator::CompoundAssignment(op)
    }
}

/// canonical operator precedence ordering. operations higher in the list
/// take precedence over ones below them
static PRECEDENCE: [(Operator, Position); 30] = [
    (Operator::Index, Position::Binary),
    (Operator::Member, Position::Binary),
    (Operator::Call, Position::Postfix),
    (Operator::Deref, Position::Postfix),
    (Operator::AddressOf, Position::Prefix),
    (Operator::Add, Position::Prefix),
    (Operator::Subtract, Position::Prefix),
    (Operator::Not, Position::Prefix),
    (Operator::As, Position::Postfix),
    (Operator::Shl, Position::Binary),
    (Operator::Shr, Position::Binary),
    (Operator::Multiply, Position::Binary),
    (Operator::Divide, Position::Binary),
    (Operator::Add, Position::Binary),
    (Operator::Subtract, Position::Binary),
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
    (Operator::CompoundAssignment(CompoundAssignmentOperator::AddAssign), Position::Binary),
    (Operator::CompoundAssignment(CompoundAssignmentOperator::SubtractAssign), Position::Binary),
    (Operator::CompoundAssignment(CompoundAssignmentOperator::MultiplyAssign), Position::Binary),
    (Operator::CompoundAssignment(CompoundAssignmentOperator::DivideAssign), Position::Binary),
];

impl Operator {
    pub fn for_position(position: Position) -> impl Iterator<Item = Operator> {
        PRECEDENCE
            .iter()
            .filter_map(move |(op, pos)| if *pos == position { Some(*op) } else { None })
    }

    pub fn precedence(self, in_pos: Position) -> usize {
        match self {
            Operator::Member | Operator::Index | Operator::Call => 0,

            _ => PRECEDENCE
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
            "as" => Some(Operator::As),
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
        match self {
            Operator::Member => write!(f, "."),
            Operator::Deref => write!(f, "^"),
            Operator::AddressOf => write!(f, "@"),
            Operator::Assignment => write!(f, ":="),
            Operator::CompoundAssignment(a) => write!(f, "{}", a),
            Operator::Equals => write!(f, "="),
            Operator::NotEquals => write!(f, "<>"),
            Operator::Shl => write!(f, "shl"),
            Operator::Shr => write!(f, "shr"),
            Operator::Add => write!(f, "+"),
            Operator::Subtract => write!(f, "-"),
            Operator::Multiply => write!(f, "*"),
            Operator::Divide => write!(f, "/"),
            Operator::And => write!(f, "and"),
            Operator::Not => write!(f, "not"),
            Operator::Or => write!(f, "or"),
            Operator::Gt => write!(f, ">"),
            Operator::Gte => write!(f, ">="),
            Operator::Lt => write!(f, "<"),
            Operator::Lte => write!(f, "<="),
            Operator::In => write!(f, "in"),
            Operator::RangeInclusive => write!(f, ".."),
            Operator::Call => write!(f, "(...)"),
            Operator::Index => write!(f, "[...]"),
            Operator::As => write!(f, "as"),
        }
    }
}

impl fmt::Display for CompoundAssignmentOperator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let op_str = match self {
            CompoundAssignmentOperator::AddAssign => "+=",
            CompoundAssignmentOperator::SubtractAssign => "-=",
            CompoundAssignmentOperator::MultiplyAssign => "*=",
            CompoundAssignmentOperator::DivideAssign => "/=",
        };
        write!(f, "{}", op_str)
    }
}