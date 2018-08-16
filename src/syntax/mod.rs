pub mod function;
pub mod var_decl;
pub mod type_decl;
pub mod program;
pub mod block;
pub mod expression;
pub mod iter;
pub mod matcher;
pub mod unit;
pub mod token_stream;

pub use self::function::*;
pub use self::block::*;
pub use self::var_decl::*;
pub use self::type_decl::*;
pub use self::program::*;
pub use self::expression::*;
pub use self::iter::*;
pub use self::matcher::*;
pub use self::unit::*;
pub use self::token_stream::TokenStream;

use std::fmt;

use operators;
use source;
use node::{
    Identifier,
    Symbol,
    ToSource,
};

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct ParsedSymbol(pub Identifier);

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct ParsedType {
    pub name: Identifier,
    pub indirection: usize,
}

impl ToSource for ParsedType {
    fn to_source(&self) -> String {
        self.to_string()
    }
}

impl ParsedType {
    pub fn with_name(name: impl Into<Identifier>) -> Self {
        ParsedType {
            name: name.into(),
            indirection: 0,
        }
    }

    pub fn pointer(self) -> Self {
        ParsedType {
            name: self.name,
            indirection: self.indirection + 1,
        }
    }

    pub fn parse(tokens: &mut TokenStream) -> ParseResult<ParsedType> {
        let mut indirection = 0;

        loop {
            let pointer_sigil = tokens.match_peek(operators::Deref)?;
            if pointer_sigil.is_some() {
                indirection += 1;
            } else {
                let name = Identifier::parse(tokens)?;

                break Ok(ParsedType {
                    name,
                    indirection,
                });
            }
        }
    }
}

impl fmt::Display for ParsedType {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        for _ in 0..self.indirection {
            f.write_str("^")?;
        }

        self.name.fmt(f)
    }
}

impl Symbol for ParsedSymbol {
    type Type = ParsedType;
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
                write!(f, "unexpected token {}", source_token)?;

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

    pub fn map<TOut>(self, f: impl FnOnce(TValue) -> TOut) -> ParseOutput<TOut>
    {
        let mapped = f(self.value);

        ParseOutput::new(mapped, self.last_token, self.next_tokens)
    }
}

pub type ParseResult<TValue> = Result<TValue, ParseError>;
pub type ParseOutputResult<TValue> = Result<ParseOutput<TValue>, ParseError>;
