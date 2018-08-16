#[derive(Copy, Clone, Debug)]
pub enum Keyword {
    Program,
    Var,
    Function,
    Begin,
    End,
    Uses,
    Type,
    Record,
}

impl Keyword {
    pub fn try_parse(from: &str) -> Option<Self> {
        match from {
            "program" => Some(Keyword::Program),
            "var" => Some(Keyword::Var),
            "function" => Some(Keyword::Function),
            "begin" => Some(Keyword::Begin),
            "end" => Some(Keyword::End),
            "uses" => Some(Keyword::Uses),
            "type" => Some(Keyword::Type),
            "record" => Some(Keyword::Record),
            _ => None,
        }
    }
}