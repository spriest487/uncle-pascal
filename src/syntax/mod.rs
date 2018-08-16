pub mod function;
pub mod var_decl;
pub mod type_decl;
pub mod program;
pub mod block;
pub mod expression;
pub mod iter;
pub mod matcher;
pub mod unit;

pub use self::function::*;
pub use self::block::*;
pub use self::var_decl::*;
pub use self::type_decl::*;
pub use self::program::*;
pub use self::expression::*;
pub use self::iter::*;
pub use self::matcher::*;
pub use self::unit::*;

use std::fmt;

use operators;
use source;
use node::{
    Identifier,
    Symbol,
    ToSource
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

    pub fn parse(tokens: impl IntoIterator<Item = source::Token> + 'static,
                 context: &source::Token) -> ParseResult<ParsedType> {
        let mut indirection = 0;

        let mut next_tokens: Box<Iterator<Item = source::Token>> = Box::from(tokens.into_iter());
        let mut last_token = context.clone();

        loop {
            let pointer_sigil = operators::Deref.match_peek(next_tokens, &last_token)?;
            if pointer_sigil.value.is_some() {
                indirection += 1;

                next_tokens = Box::from(pointer_sigil.next_tokens.skip(1));
                last_token = pointer_sigil.last_token;
            } else {
                let name = Identifier::parse(pointer_sigil.next_tokens, &pointer_sigil.last_token)?;
                let parsed_type = ParsedType {
                    name: name.value,
                    indirection,
                };

                break Ok(ParseOutput::new(parsed_type, name.last_token, name.next_tokens))
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
    }
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
                write!(f, "unexpected< end of input - expected {} after {}", expected, context),

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

    pub fn map<TFn, TOut>(self, mut f: TFn) -> ParseOutput<TOut>
        where TFn: FnMut(TValue) -> TOut
    {
        let mapped = f(self.value);

        ParseOutput::new(mapped, self.last_token, self.next_tokens)
    }
}

pub type ParseResult<TValue> = Result<ParseOutput<TValue>, ParseError>;
