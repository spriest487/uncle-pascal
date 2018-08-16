use keywords;
use operators;

#[derive(Clone, Debug)]
pub enum Token {
    Keyword(keywords::Keyword),
    Identifier(String),
    BinaryOperator(operators::BinaryOperator),
    LiteralInteger(i64),
    LiteralString(String),
    Period,
    Colon,
    Semicolon,
    BracketLeft,
    BracketRight,
    Comma,
}

pub trait ToSource {
    fn to_source(&self) -> String;
}

impl ToSource for Token {
    fn to_source(&self) -> String {
        match self {
            &Token::Keyword(kw) => kw.to_source(),
            &Token::Identifier(ref name) => name.clone(),
            &Token::BinaryOperator(ref op) => op.to_source(),
            &Token::LiteralInteger(i) => format!("{}", i),
            &Token::LiteralString(ref s) => format!("'{}'", s.replace("'", "''")),
            &Token::Period => ".".to_owned(),
            &Token::Colon => ":".to_owned(),
            &Token::Semicolon => ";".to_owned(),
            &Token::BracketLeft => "(".to_owned(),
            &Token::BracketRight => ")".to_owned(),
            &Token::Comma => ",".to_owned(),
        }
    }
}