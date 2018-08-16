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

use operators;
use node;
use types::*;

use source;

#[derive(Clone, Debug)]
pub enum SemanticErrorKind {
    UnknownType(node::Identifier),
    UnknownSymbol(node::Identifier),
    UnexpectedType {
        expected: Option<DeclaredType>,
        actual: Option<DeclaredType>,
    },
    InvalidFunctionType(Option<DeclaredType>),
    WrongNumberOfArgs {
        expected_sig: FunctionSignature,
        actual: usize,
    },
    MemberAccessOfNonRecord(Option<DeclaredType>, String),
    IllegalName(String),
    EmptyRecord(String),
    InvalidOperator {
        op: operators::Operator,
        args: Vec<Option<DeclaredType>>,
    },
    TypeNotAssignable(Option<DeclaredType>),
    TypesNotComparable(Option<DeclaredType>, Option<DeclaredType>),
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

            &SemanticErrorKind::MemberAccessOfNonRecord(ref actual, ref name) => {
                write!(f, "cannot access member {} of {} because it is not a record",
                       name,
                       actual.as_ref()
                           .map(|t| t.to_string())
                           .unwrap_or_else(|| "none".to_string()))
            }

            &SemanticErrorKind::UnexpectedType { ref expected, ref actual } => {
                write!(f, "expected type `{}`, found `{}`",
                       expected.as_ref().map(|t| t.to_string()).unwrap_or_else(|| "(none)".to_string()),
                       actual.as_ref().map(|t| t.to_string()).unwrap_or_else(|| "(none)".to_string()))
            }

            &SemanticErrorKind::InvalidFunctionType(ref actual) => {
                let actual_name = actual.as_ref()
                    .map(|t| t.to_string())
                    .unwrap_or_else(|| "(none)".to_owned());

                write!(f, "type `{}` is not a callable function", actual_name)
            }

            &SemanticErrorKind::WrongNumberOfArgs { ref expected_sig, actual } => {
                write!(f, "wrong number if arguments to function `{}`, expected {}, found {}",
                       expected_sig.name, expected_sig.arg_types.len(), actual)
            }

            &SemanticErrorKind::IllegalName(ref name) => {
                write!(f, "illegal name: `{}`", name)
            }

            &SemanticErrorKind::EmptyRecord(ref name) => {
                write!(f, "record type `{}` must have at least one member", name)
            }

            &SemanticErrorKind::InvalidOperator { ref op, ref args } => {
                let args_list = args.iter()
                    .map(|arg| DeclaredType::name(arg.as_ref()))
                    .collect::<Vec<_>>()
                    .join(", ");

                write!(f, "the operator {} cannot be applied to the argument types {}", op, args_list)
            }

            &SemanticErrorKind::TypeNotAssignable(ref t) => {
                write!(f, "type `{}` cannot be assigned to",
                    DeclaredType::name(t.as_ref()))
            }

            &SemanticErrorKind::TypesNotComparable(ref a, ref b) => {
                write!(f, "types cannot be compared: `{}` and `{}`",
                       DeclaredType::name(a.as_ref()),
                       DeclaredType::name(b.as_ref()))
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
            context,
        }
    }

    pub fn unknown_symbol(name: node::Identifier, context: source::Token) -> Self {
        SemanticError {
            kind: SemanticErrorKind::UnknownSymbol(name),
            context,
        }
    }

    pub fn unexpected_type(expected: Option<DeclaredType>,
                           actual: Option<DeclaredType>,
                           context: source::Token) -> Self {
        SemanticError {
            kind: SemanticErrorKind::UnexpectedType { expected, actual },
            context,
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

    pub fn member_of_non_record(actual: Option<DeclaredType>,
                                name: String,
                                context: source::Token) -> Self {
        SemanticError {
            kind: SemanticErrorKind::MemberAccessOfNonRecord(actual, name),
            context,
        }
    }

    pub fn wrong_num_args(sig: FunctionSignature,
                          actual: usize,
                          context: source::Token) -> Self {
        SemanticError {
            kind: SemanticErrorKind::WrongNumberOfArgs {
                expected_sig: sig,
                actual,
            },
            context,
        }
    }

    pub fn invalid_function_type(actual: Option<DeclaredType>, context: source::Token) -> Self {
        SemanticError {
            kind: SemanticErrorKind::InvalidFunctionType(actual),
            context,
        }
    }

    pub fn invalid_operator<TArgs>(operator: operators::Operator,
                                   args: TArgs,
                                   context: source::Token) -> SemanticError
        where TArgs: IntoIterator<Item=Option<DeclaredType>>
    {
        SemanticError {
            kind: SemanticErrorKind::InvalidOperator {
                op: operator,
                args: args.into_iter().collect(),
            },
            context,
        }
    }

    pub fn type_not_assignable(t: Option<DeclaredType>,
                               context: source::Token) -> SemanticError {
        SemanticError {
            kind: SemanticErrorKind::TypeNotAssignable(t),
            context,
        }
    }

    pub fn types_not_comparable(a: Option<DeclaredType>, b: Option<DeclaredType>,
                                context: source::Token) -> SemanticError {
        SemanticError {
            kind: SemanticErrorKind::TypesNotComparable(a, b),
            context,
        }
    }
}

impl fmt::Display for SemanticError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}: {}", self.context, self.kind)
    }
}