use std::fmt;

use operators;
use types;
use syntax::*;
use tokens;
use tokens::AsToken;
use ToSource;

#[allow(dead_code)]
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
    let split_at_op = Matcher::AnyBinaryOperator
        .split_at_match(in_tokens, context)?;

    let lhs_expr = Expression::parse(split_at_op.value.before_split, context)?;
    let op = split_at_op.value.split_at.as_token().unwrap_binary_operator().clone();
    let rhs_expr = Expression::parse(split_at_op.next_tokens, &split_at_op.last_token)?;

    Ok(Expression::BinaryOperator {
        lhs: Box::from(lhs_expr),
        op,
        rhs: Box::from(rhs_expr),
    })
}

fn parse_identifier<TIter>(in_tokens: TIter, context: &TIter::Item) -> Result<Expression, ParseError<TIter::Item>>
    where TIter: IntoIterator + 'static,
          TIter::Item: tokens::AsToken + 'static
{
    let identifier = Matcher::AnyIdentifier
        .match_one(in_tokens, context)?
        .finish()?;

    let name = identifier.as_token().unwrap_identifier();

    Ok(Expression::Identifier(types::Identifier::parse(name)))
}

fn parse_literal_string<TIter>(in_tokens: TIter, context: &TIter::Item) -> Result<Expression, ParseError<TIter::Item>>
    where TIter: IntoIterator + 'static,
          TIter::Item: tokens::AsToken + 'static
{
    let string_token = Matcher::AnyLiteralString
        .match_one(in_tokens, context)?
        .finish()?;

    let string_value = string_token.as_token().unwrap_literal_string().to_owned();

    Ok(Expression::LiteralString(string_value))
}

fn parse_literal_integer<TIter>(in_tokens: TIter, context: &TIter::Item) -> Result<Expression, ParseError<TIter::Item>>
    where TIter: IntoIterator + 'static,
          TIter::Item: tokens::AsToken + 'static
{
    let integer_token = Matcher::AnyLiteralInteger
        .match_one(in_tokens, context)?
        .finish()?;

    Ok(Expression::LiteralInteger(integer_token.as_token().unwrap_literal_integer()))
}

fn parse_function_call<TIter>(in_tokens: TIter, context: &TIter::Item) -> Result<Expression, ParseError<TIter::Item>>
    where TIter: IntoIterator + 'static,
          TIter::Item: tokens::AsToken + 'static
{
    let func_name = Matcher::AnyIdentifier.match_one(in_tokens, context)?;

    let args = Matcher::Exact(tokens::BracketLeft)
        .terminated_by(Matcher::Exact(tokens::BracketRight))
        .match_block(func_name.next_tokens, &func_name.last_token)?;

    //crude arg match: split on all commas in args list
    let all_args = args.value.inner
        .split(|t| Matcher::Exact(tokens::Comma).is_match(t))
        .map(|arg_tokens| {
            let all_arg_tokens = arg_tokens.iter().cloned().collect::<Vec<_>>();

            //TODO; context is wrong here?
            Expression::parse(all_arg_tokens, context)
        })
        .collect::<Result<Vec<_>, _>>();

    Ok(Expression::FunctionCall {
        target: types::Identifier::parse(func_name.value.as_token().unwrap_identifier()),
        args: all_args?
    })
}

impl Expression {
    pub fn parse<TIter>(in_tokens: TIter, context: &TIter::Item) -> Result<Expression, ParseError<TIter::Item>>
        where TIter: IntoIterator + 'static,
              TIter::Item: tokens::AsToken + 'static
    {
        let all_tokens = in_tokens.into_iter().collect::<Vec<_>>();

        parse_binary_op(all_tokens.clone(), context)
            .or_else(|_| parse_function_call(all_tokens.clone(), context))
            .or_else(|_| parse_identifier(all_tokens.clone(), context))
            .or_else(|_| parse_literal_string(all_tokens.clone(), context))
            .or_else(|_| parse_literal_integer(all_tokens.clone(), context))
    }
}

impl ToSource for Expression {
    fn to_source(&self) -> String {
        match self {
            &Expression::BinaryOperator{ref lhs, ref op, ref rhs} => {
                format!("({} {} {})", lhs.to_source(), op, rhs.to_source())
            }

            &Expression::Identifier(ref id) => format!("{}", id),

            &Expression::FunctionCall { ref target, ref args } => {
                let args_str = args.iter()
                    .map(|arg| arg.to_source())
                    .collect::<Vec<_>>()
                    .join(", ");

                format!("{}({})", target, args_str)
            },

            &Expression::LiteralInteger(i) => format!("{}", i),

            &Expression::LiteralString(ref s) =>
                format!("'{}'", tokens::LiteralString(s.clone()).to_source()),
        }
    }
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "expression: {}", self.to_source())
    }
}