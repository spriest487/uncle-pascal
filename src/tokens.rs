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
    LineComment,
}
