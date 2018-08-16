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

use source;

pub trait ToContext {
    type Context;

    fn to_context(&self) -> Self::Context;
}

#[derive(Clone, Debug)]
pub enum ParseError {
    UnexpectedToken(source::Token, Option<Matcher>),
    UnexpectedEOF(Matcher, source::Token),
//    UnrecognizedSequence(Vec<source::Token>),
}

#[allow(dead_code)]
impl ParseError {
    pub fn is_eof(&self) -> bool {
        match self {
            &ParseError::UnexpectedEOF(_, _) => true,
            _ => false
        }
    }
}

impl fmt::Display for ParseError {
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

//            &ParseError::UnrecognizedSequence(ref tokens) =>
//                write!(f, "tokens `{}` could not be parsed", source::tokens_to_source(tokens)),
        }
    }
}

pub struct ParseOutput<TValue> {
    pub value: TValue,
    pub last_token: source::Token,
    pub next_tokens: Box<Iterator<Item=source::Token>>,
}

impl<TValue> ParseOutput<TValue> {
    pub fn new<TNext>(value: TValue,
                      last_token: source::Token,
                      next_tokens: TNext) -> Self
        where TNext: IntoIterator<Item=source::Token> + 'static
    {
        Self {
            value,
            last_token,
            next_tokens: Box::from(next_tokens.into_iter()),
        }
    }

    pub fn finish(mut self) -> Result<TValue, ParseError> {
        let unexpected = self.next_tokens.next();
        match unexpected {
            Some(token) => Err(ParseError::UnexpectedToken(token, None)),
            None => Ok(self.value)
        }
    }

    pub fn map<TFn, TOut>(self, mut f: TFn) -> ParseOutput<TOut>
        where TFn: FnMut(TValue) -> TOut
    {
        let mapped = f(self.value);

        ParseOutput::new(mapped, self.last_token, self.next_tokens)
    }
}

pub type ParseResult<TValue> = Result<ParseOutput<TValue>, ParseError>;
