use tokens;

#[derive(Clone, Debug)]
pub enum BinaryOperator {
    Assignment,
    Equals,
    Plus,
    Minus,
}

impl BinaryOperator {
    pub fn try_parse(from: &str) -> Option<Self> {
        match from {
            ":=" => Some(BinaryOperator::Assignment),
            "=" => Some(BinaryOperator::Equals),
            "+" => Some(BinaryOperator::Plus),
            "-" => Some(BinaryOperator::Minus),
            _ => None
        }
    }
}

impl tokens::ToSource for BinaryOperator {
    fn to_source(&self) -> String {
        match self {
            &BinaryOperator::Assignment => " := ".to_owned(),
            &BinaryOperator::Equals => " = ".to_owned(),
            &BinaryOperator::Plus => " + ".to_owned(),
            &BinaryOperator::Minus => " - ".to_owned(),
        }
    }
}