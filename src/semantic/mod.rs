pub mod scope;
pub mod expression;
pub mod var_decl;
pub mod block;
pub mod function;
pub mod type_decl;
pub mod program;

pub use self::scope::*;
pub use self::var_decl::*;
pub use self::expression::*;
pub use self::block::*;
pub use self::function::*;
pub use self::type_decl::*;
pub use self::program::*;

use std::fmt;

use node;
use types::*;

use source;

#[derive(Clone, Debug)]
pub enum SemanticErrorKind {
    UnknownType(node::Identifier),
    UnknownSymbol(node::Identifier),
    UnexpectedType{
        expected: Option<DeclaredType>,
        actual: Option<DeclaredType>
    },
    InvalidFunctionType(ScopedSymbol),
    WrongNumberOfArgs {
        target: ScopedSymbol,
        expected: usize,
        actual: usize,
    },
    IllegalName(String),
    EmptyRecord(String),
}

impl fmt::Display for SemanticErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &SemanticErrorKind::UnknownType(ref missing_type) => {
                write!(f, "type not found: `{}`", missing_type)
            }

            &SemanticErrorKind::UnknownSymbol(ref missing_sym) => {
                write!(f, "symbol not found: `{}`", missing_sym)
            }

            &SemanticErrorKind::UnexpectedType { ref expected, ref actual } => {
                write!(f, "expected type `{}`, found `{}`",
                       expected.as_ref().map(|t| t.to_string()).unwrap_or_else(|| "(none)".to_string()),
                       actual.as_ref().map(|t| t.to_string()).unwrap_or_else(|| "(none)".to_string()))
            }

            &SemanticErrorKind::InvalidFunctionType(ref id) => {
                write!(f, "type `{}` is not a callable function", id)
            }

            &SemanticErrorKind::WrongNumberOfArgs { ref target, expected, actual } => {
                write!(f, "wrong number if arguments to function `{}`, expected {}, found `{}`",
                       target, expected, actual)
            }

            &SemanticErrorKind::IllegalName(ref name) => {
                write!(f, "illegal name: `{}`", name)
            }

            &SemanticErrorKind::EmptyRecord(ref name) => {
                write!(f, "record type `{}` must have at least one member", name)
            }
        }
    }
}

#[derive(Clone, Debug)]
pub struct SemanticError {
    kind: SemanticErrorKind,
    context: source::Token,
}

impl SemanticError {
    pub fn illegal_name(name: String, context: source::Token) -> Self {
        SemanticError {
            kind: SemanticErrorKind::IllegalName(name),
            context
        }
    }

    pub fn unknown_symbol(name: node::Identifier, context: source::Token) -> Self {
        SemanticError {
            kind: SemanticErrorKind::UnknownSymbol(name),
            context
        }
    }

    pub fn unexpected_type(expected: Option<DeclaredType>,
                           actual: Option<DeclaredType>,
                           context: source::Token) -> Self {
        SemanticError {
            kind: SemanticErrorKind::UnexpectedType { expected, actual },
            context
        }
    }

    pub fn unknown_type(missing_type: node::Identifier, context: source::Token) -> Self {
        SemanticError {
            kind: SemanticErrorKind::UnknownType(missing_type),
            context,
        }
    }

    pub fn empty_record(record_id: String, context: source::Token) -> Self {
        SemanticError {
            kind: SemanticErrorKind::EmptyRecord(record_id),
            context,
        }
    }

    pub fn wrong_num_args(target: ScopedSymbol,
                          expected: usize,
                          actual: usize,
                          context: source::Token) -> Self {
        SemanticError {
            kind: SemanticErrorKind::WrongNumberOfArgs {
                target,
                expected,
                actual,
            },
            context
        }
    }

    pub fn invalid_function_type(target: ScopedSymbol, context: source::Token) -> Self {
        SemanticError {
            kind: SemanticErrorKind::InvalidFunctionType(target),
            context
        }
    }
}

impl fmt::Display for SemanticError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}: {}", self.context, self.kind)
    }
}