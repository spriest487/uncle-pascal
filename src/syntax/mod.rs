pub mod function;
pub mod var_decl;
pub mod type_decl;
pub mod program;
pub mod block;
pub mod expression;
pub mod matcher;
pub mod unit;
pub mod token_stream;
pub mod const_decl;

pub use self::function::*;
pub use self::block::*;
pub use self::var_decl::*;
pub use self::type_decl::*;
pub use self::program::*;
pub use self::expression::*;
pub use self::matcher::*;
pub use self::unit::*;
pub use self::const_decl::*;
pub use self::token_stream::{
    TokenStream,
    Parse,
};

use std::fmt;

use source;
use node::{
    Identifier,
    Symbol,
    ToSource,
    TypeName,
    Context,
};

#[derive(Clone, Debug)]
pub struct ParsedContext {
    pub token: source::Token,
}

impl Context for ParsedContext {
    fn token(&self) -> &source::Token {
        &self.token
    }
}

impl From<source::Token> for ParsedContext {
    fn from(token: source::Token) -> Self {
        Self {
            token
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct ParsedSymbol(pub Identifier);

impl Symbol for ParsedSymbol {
    type Type = TypeName;
}

impl ToSource for ParsedSymbol {
    fn to_source(&self) -> String {
        self.0.to_source()
    }
}

impl fmt::Display for ParsedSymbol {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        self.0.fmt(f)
    }
}

pub trait ToContext {
    type Context;

    fn to_context(&self) -> Self::Context;
}

#[derive(Clone, Debug)]
pub enum ParseError {
    UnexpectedToken(source::Token, Option<Matcher>),
    UnexpectedEOF(Matcher, source::Token),
    ArrayDimensionOutOfBounds(source::Token),
    EmptyOperand {
        operator: source::Token,
        before: bool,
    },
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
            ParseError::UnexpectedToken(source_token, expected) => {
                write!(f, "unexpected {}", source_token)?;

                expected.as_ref()
                    .map(|matcher| write!(f, " (expected: {})", matcher))
                    .unwrap_or(Ok(()))
            }

            ParseError::UnexpectedEOF(expected, context) =>
                write!(f, "unexpected end of input - expected {} after {}", expected, context),

            ParseError::EmptyOperand { operator, before } => {
                let position = if *before { "before" } else { "after" };
                write!(f, "missing operand {} {}", position, operator)
            }

            ParseError::ArrayDimensionOutOfBounds(token) => {
                write!(f, "array dimension constant {} is too large for target machine's native signed int size", token)
            }
        }
    }
}


pub type ParseResult<TValue> = Result<TValue, ParseError>;

#[cfg(test)]
mod test {
    use super::*;
    use node::IndexRange;
    use opts::CompileOptions;

    #[test]
    fn parses_1d_array_type() {
        let mut tokens = TokenStream::tokenize("test", "array [0..10] of Integer",
                                               &CompileOptions::default())
            .unwrap();

        let parsed = TypeName::parse(&mut tokens)
            .unwrap();

        match parsed {
            TypeName::DataType { array_dimensions, .. } =>
                assert_eq!(array_dimensions, vec![IndexRange { from: 0,  to: 10 }]),

            _ =>
                panic!("wrong type")
        }
    }
}