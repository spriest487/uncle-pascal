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