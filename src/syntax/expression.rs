use std::fmt;

use operators;
use types;
use syntax::*;
use tokens;
use tokens::AsToken;
use ToSource;

#[derive(Clone, Debug)]
pub enum Expression {
    BinaryOperator{
        lhs: Box<Expression>,
        op: operators::BinaryOperator,
        rhs: Box<Expression>,
    },
    FunctionCall{
        target: types::Identifier,
        args: Vec<Expression>,
    },
    LiteralInteger(i64),
    LiteralString(String),
    Identifier(types::Identifier),
}

fn parse_binary_op<TIter>(in_tokens: TIter, context: &TIter::Item) -> Result<Expression, ParseError<TIter::Item>>
    where TIter: IntoIterator + 'static,
          TIter::Item: tokens::AsToken + 'static
{
    let (lhs, op_last_parsed, after_op) = Matcher::AnyBinaryOperator
        .split_at_match(in_tokens, context)?
        .unwrap();

    let lhs_expr = Expression::parse(lhs.before_split, context)?;
    let op = lhs.split_at.as_token().unwrap_binary_operator().clone();
    let rhs_expr = Expression::parse(after_op, &op_last_parsed)?;

    Ok(Expression::BinaryOperator {
        lhs: Box::from(lhs_expr),
        op,
        rhs: Box::from(rhs_expr),
    })
}

impl Expression {
    pub fn parse<TIter>(in_tokens: TIter, context: &TIter::Item) -> Result<Expression, ParseError<TIter::Item>>
        where TIter: IntoIterator + 'static,
              TIter::Item: tokens::AsToken + 'static
    {
        parse_binary_op(in_tokens, context)
    }
}

impl ToSource for Expression {
    fn to_source(&self) -> String {
        match self {
            &Expression::BinaryOperator{ref lhs, ref op, ref rhs} => {
                format!("({} {} {})", *lhs, op, *rhs)
            }
            _ => unimplemented!()
        }
    }
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "expression: {}", self.to_source())
    }
}