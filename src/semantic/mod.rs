use std::fmt;

use node;
use types::*;

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

#[derive(Clone, Debug)]
pub enum SemanticError {
    UnknownType(node::Identifier),
    UnknownSymbol(node::Identifier),
    UnexpectedType{
        expected: DeclaredType,
        actual: DeclaredType
    },
    InvalidFunctionType(node::Identifier),
    WrongNumberOfArgs {
        target: node::Identifier,
        expected: usize,
        actual: usize,
    },
    IllegalName(String),
}

impl fmt::Display for SemanticError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &SemanticError::UnknownType(ref missing_type) => {
                write!(f, "type not found: {}", missing_type)
            }

            &SemanticError::UnknownSymbol(ref missing_sym) => {
                write!(f, "symbol not found: {}", missing_sym)
            }

            &SemanticError::UnexpectedType { ref expected, ref actual } => {
                write!(f, "expected type {}, found {}", expected, actual)
            }

            &SemanticError::InvalidFunctionType(ref id) => {
                write!(f, "type {} is not a callable function", id)
            }

            &SemanticError::WrongNumberOfArgs { ref target, expected, actual } => {
                write!(f, "wrong number if arguments to function {}, expected {}, found {}",
                       target, expected, actual)
            }

            &SemanticError::IllegalName(ref name) => {
                write!(f, "illegal name: {}", name)
            }
        }
    }
}
