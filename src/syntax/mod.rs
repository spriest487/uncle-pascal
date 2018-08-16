pub mod function;
pub mod var_decl;
pub mod type_decl;
pub mod program;
pub mod block;
pub mod expression;
pub mod matcher;
pub mod unit;
pub mod token_stream;

pub use self::function::*;
pub use self::block::*;
pub use self::var_decl::*;
pub use self::type_decl::*;
pub use self::program::*;
pub use self::expression::*;
pub use self::matcher::*;
pub use self::unit::*;
pub use self::token_stream::TokenStream;

use std::fmt;

use keywords;
use tokens::{self, AsToken};
use operators;
use source;
use node::{
    Identifier,
    Symbol,
    ToSource,
};

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct ParsedSymbol(pub Identifier);

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct IndexRange {
    /* TODO: non-64bit support */
    from: i64,
    to: i64,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ParsedType {
    pub name: Identifier,
    pub indirection: usize,

    pub array_dimensions: Vec<IndexRange>,
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

            array_dimensions: Vec::new(),
        }
    }

    pub fn pointer(self) -> Self {
        ParsedType {
            name: self.name,
            indirection: self.indirection + 1,

            array_dimensions: self.array_dimensions,
        }
    }

    pub fn parse(tokens: &mut TokenStream) -> ParseResult<ParsedType> {
        let array_kw = tokens.match_peek(keywords::Array)?;
        let array_dimensions = match array_kw {
            Some(_) => {
                tokens.advance(1);

                let dims_groups = tokens.match_groups(tokens::SquareBracketLeft,
                                                      tokens::SquareBracketRight,
                                                      tokens::Comma)?;

                let dims = dims_groups.groups.into_iter()
                    .map(|dim_group| {
                        let mut dim_tokens = TokenStream::new(dim_group.tokens, &dim_group.context);
                        let dim_from = dim_tokens.match_one(Matcher::AnyLiteralInteger)?;
                        dim_tokens.match_one(tokens::Period)?;
                        dim_tokens.match_one(tokens::Period)?;
                        let dim_to = dim_tokens.match_one(Matcher::AnyLiteralInteger)?;

                        Ok(IndexRange {
                            from: dim_from.unwrap_literal_integer(),
                            to: dim_to.unwrap_literal_integer(),
                        })
                    })
                    .collect::<ParseResult<_>>()?;

                tokens.match_one(keywords::Of)?;
                dims
            }

            None => Vec::new(),
        };

        let mut indirection = 0;

        loop {
            let pointer_sigil = tokens.match_peek(operators::Deref)?;
            if pointer_sigil.is_some() {
                indirection += 1;
                tokens.advance(1);
            } else {
                let name = Identifier::parse(tokens)?;

                break Ok(ParsedType {
                    name,
                    indirection,

                    array_dimensions,
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


pub type ParseResult<TValue> = Result<TValue, ParseError>;

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn parses_1d_array_type() {
        let mut tokens = TokenStream::tokenize("test", "array [0..10] of Integer")
            .unwrap();

        let parsed = ParsedType::parse(&mut tokens)
            .unwrap();

        assert_eq!(parsed.array_dimensions, vec![IndexRange { from: 0,  to: 10 }]);
    }
}