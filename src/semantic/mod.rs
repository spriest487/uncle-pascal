pub mod scope;
pub mod expression;
pub mod var_decl;
pub mod block;
pub mod function;
pub mod type_decl;
pub mod program;
pub mod unit;
pub mod module;

pub use self::{
    block::*,
    expression::*,
    function::*,
    program::*,
    scope::*,
    type_decl::*,
    unit::*,
    var_decl::*,
    module::*,
};

use std::{
    rc::Rc
};

use node::{Identifier, TypeName};
use operators;
use source;
use std::fmt;
use types::*;

#[derive(Clone, Debug)]
pub enum SemanticErrorKind {
    UnknownType(TypeName),
    UnknownSymbol(Identifier),
    UnexpectedType {
        expected: Option<Type>,
        actual: Option<Type>,
    },
    InvalidFunctionType(Option<Type>),
    InvalidConstructorType(Option<Type>),
    InvalidDestructorReturn(Type),
    InvalidDestructorArgs(Vec<Type>),
    WrongNumberOfArgs {
        expected_sig: FunctionSignature,
        actual: usize,
    },
    MemberAccessOfNonRecord(Option<Type>, String),
    IllegalName(String),
    EmptyRecord(Identifier),
    InvalidOperator {
        op: operators::Operator,
        args: Vec<Option<Type>>,
    },
    TypeNotAssignable(Option<Type>),
    TypesNotComparable(Option<Type>, Option<Type>),
}

impl fmt::Display for SemanticErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &SemanticErrorKind::UnknownType(ref missing_type) => {
                write!(f, "type `{}` was not found", missing_type)
            }

            &SemanticErrorKind::UnknownSymbol(ref missing_sym) => {
                write!(f, "symbol `{}` was not found", missing_sym)
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

            SemanticErrorKind::InvalidConstructorType(ref actual) => {
                let actual_name = actual.as_ref().map(|t| t.to_string())
                    .unwrap_or_else(|| "none".to_string());

                write!(f, "return type of constructor function must be a class, found `{}`",
                       actual_name)
            }

            SemanticErrorKind::InvalidDestructorReturn(actual) => {
                write!(f, "destructor must have no return type but `{}` was found", actual)
            }

            SemanticErrorKind::InvalidDestructorArgs(arg_types) => {
                let arg_names = if arg_types.len() > 0 {
                    arg_types.iter()
                        .map(|arg_type| format!("`{}`", arg_type))
                        .collect::<Vec<_>>()
                        .join(", ")
                } else {
                    "(nothing)".to_string()
                };

                write!(f, "destructor must have one argument of a class type from its own module, but found {}",
                    arg_names)
            }

            &SemanticErrorKind::WrongNumberOfArgs { ref expected_sig, actual } => {
                write!(f, "wrong number if arguments to function `{}`, expected {}, found {}",
                       expected_sig.name, expected_sig.arg_types.len(), actual)
            }

            &SemanticErrorKind::IllegalName(ref name) => {
                write!(f, "illegal name `{}`", name)
            }

            &SemanticErrorKind::EmptyRecord(ref name) => {
                write!(f, "record type `{}` must have at least one member", name)
            }

            &SemanticErrorKind::InvalidOperator { ref op, ref args } => {
                let args_list = args.iter()
                    .map(|arg| format!("`{}`", Type::name(arg.as_ref())))
                    .collect::<Vec<_>>()
                    .join(", ");

                write!(f, "the operator {} cannot be applied to the argument types {}", op, args_list)
            }

            &SemanticErrorKind::TypeNotAssignable(ref t) => {
                write!(f, "type `{}` cannot be assigned to",
                       Type::name(t.as_ref()))
            }

            &SemanticErrorKind::TypesNotComparable(ref a, ref b) => {
                write!(f, "`{}` cannot be used in comparison operations with `{}`",
                       Type::name(a.as_ref()),
                       Type::name(b.as_ref()))
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

    pub fn unknown_symbol(name: Identifier, context: source::Token) -> Self {
        SemanticError {
            kind: SemanticErrorKind::UnknownSymbol(name),
            context,
        }
    }

    pub fn unexpected_type(expected: Option<Type>,
                           actual: Option<Type>,
                           context: source::Token) -> Self {
        SemanticError {
            kind: SemanticErrorKind::UnexpectedType { expected, actual },
            context,
        }
    }

    pub fn unknown_type(missing_type: TypeName, context: source::Token) -> Self {
        SemanticError {
            kind: SemanticErrorKind::UnknownType(missing_type),
            context,
        }
    }

    pub fn empty_record(record_id: Identifier, context: source::Token) -> Self {
        SemanticError {
            kind: SemanticErrorKind::EmptyRecord(record_id),
            context,
        }
    }

    pub fn member_of_non_record(actual: Option<Type>,
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

    pub fn invalid_function_type(actual: Option<Type>, context: source::Token) -> Self {
        SemanticError {
            kind: SemanticErrorKind::InvalidFunctionType(actual),
            context,
        }
    }

    pub fn invalid_constructor_type(actual: Option<Type>, context: source::Token) -> Self {
        SemanticError {
            kind: SemanticErrorKind::InvalidConstructorType(actual),
            context,
        }
    }

    pub fn invalid_destructor_return(return_type: Type, context: source::Token)
                                     -> SemanticError {
        SemanticError {
            kind: SemanticErrorKind::InvalidDestructorReturn(return_type),
            context,
        }
    }

    pub fn invalid_destructor_args(args: impl IntoIterator<Item=Type>,
                                   context: source::Token)
                                   -> SemanticError {
        SemanticError {
            kind: SemanticErrorKind::InvalidDestructorArgs(args.into_iter().collect()),
            context
        }
    }

    pub fn invalid_operator<TArgs>(operator: operators::Operator,
                                   args: TArgs,
                                   context: source::Token) -> SemanticError
        where TArgs: IntoIterator<Item=Option<Type>>
    {
        SemanticError {
            kind: SemanticErrorKind::InvalidOperator {
                op: operator,
                args: args.into_iter().collect(),
            },
            context,
        }
    }

    pub fn type_not_assignable(t: Option<Type>,
                               context: source::Token) -> SemanticError {
        SemanticError {
            kind: SemanticErrorKind::TypeNotAssignable(t),
            context,
        }
    }

    pub fn types_not_comparable(a: Option<Type>, b: Option<Type>,
                                context: source::Token) -> SemanticError {
        SemanticError {
            kind: SemanticErrorKind::TypesNotComparable(a, b),
            context,
        }
    }
}

impl fmt::Display for SemanticError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}\n  {}", self.context, self.kind)
    }
}

pub type SemanticResult<T> = Result<T, SemanticError>;

pub struct SemanticContext {
    token: source::Token,
    scope: Rc<Scope>,
}