pub mod function;
pub mod var_decl;
pub mod type_decl;
pub mod program;
pub mod block;
pub mod expression;
pub mod iter;
pub mod matcher;

pub use self::function::*;
pub use self::block::*;
pub use self::var_decl::*;
pub use self::type_decl::*;
pub use self::program::*;
pub use self::expression::*;
pub use self::iter::*;
pub use self::matcher::*;

use std::fmt;

use tokens;

#[derive(Clone, Debug)]
pub enum ParseError<TToken> {
    UnexpectedToken(TToken, Option<Matcher>),
    UnexpectedEOF(Matcher, TToken),
}

#[allow(dead_code)]
impl<T> ParseError<T> {
    pub fn is_eof(&self) -> bool {
        match self {
            &ParseError::UnexpectedEOF(_, _) => true,
            _ => false
        }
    }
}

impl<TToken> fmt::Display for ParseError<TToken>
    where TToken: tokens::AsToken + fmt::Display
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &ParseError::UnexpectedToken(ref source_token, ref expected) => {
                write!(f, "unexpected token: {}", source_token)?;

                expected.as_ref()
                    .map(|matcher| write!(f, " (expected: {})", matcher))
                    .unwrap_or(Ok(()))
            }
            &ParseError::UnexpectedEOF(ref expected, ref context) =>
                write!(f, "unexpected end of input: expected {} after {}", expected, context),
        }
    }
}

pub struct ParseOutput<TToken, TValue> {
    pub value: TValue,
    pub last_token: TToken,
    pub next_tokens: Box<Iterator<Item=TToken>>,
}

impl<TToken, TValue> ParseOutput<TToken, TValue> {
    pub fn new<TNext>(value: TValue, last_token: TToken, next_tokens: TNext) -> Self
        where TNext: IntoIterator<Item=TToken> + 'static
    {
        Self {
            value,
            last_token,
            next_tokens: Box::from(next_tokens.into_iter()),
        }
    }

    pub fn finish(mut self) -> Result<TValue, ParseError<TToken>> {
        let unexpected = self.next_tokens.next();
        match unexpected {
            Some(token) => Err(ParseError::UnexpectedToken(token, None)),
            None => Ok(self.value)
        }
    }
}

type ParseResult<TValue, TToken> = Result<ParseOutput<TToken, TValue>, ParseError<TToken>>;
