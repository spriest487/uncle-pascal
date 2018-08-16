pub mod scope;
pub mod expression;
pub mod var_decl;
pub mod const_decl;
pub mod block;
pub mod function;
pub mod type_decl;
pub mod program;
pub mod unit;
pub mod module;
pub mod array;

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
    const_decl::*,
    array::*,
};

use std::{
    rc::Rc
};

use node::{
    Identifier,
    Context,
    ToSource,
    ExpressionValue,
};
use operators;
use source;
use std::fmt;
use types::*;

#[derive(Clone, Debug)]
pub enum SemanticErrorKind {
    UnknownType(Identifier),
    UnknownSymbol(Identifier),
    UnexpectedType {
        expected: Option<Type>,
        actual: Option<Type>,
    },
    InvalidWithType(Option<Type>),
    InvalidFunctionType(Option<Type>),
    InvalidConstructorType(Option<Type>),
    InvalidDestructorReturn(Type),
    InvalidDestructorArgs(Vec<Type>),
    InvalidConstantValue(ExpressionValue<SemanticContext>),
    InvalidArrayIndex(Option<Type>),
    InvalidArrayType(Option<Type>),
    WrongNumberOfArgs {
        expected_sig: FunctionSignature,
        actual: usize,
    },
    WrongArgTypes {
        sig: FunctionSignature,
        actual: Vec<Option<Type>>,
    },
    MemberAccessOfNonRecord(Option<Type>, String),
    IllegalName(String),
    EmptyRecord(Identifier),
    InvalidOperator {
        op: operators::Operator,
        args: Vec<Option<Type>>,
    },
    InvalidTypecast {
        target_type: Type,
        from_type: Option<Type>,
    },
    UninitializedSymbol(Identifier),
    TypeNotAssignable(Option<Type>),
    ValueNotAssignable(Expression),
    TypesNotComparable(Option<Type>, Option<Type>),
    PrivateMemberAccessForbidden {
        base_type: Identifier,
        member_name: String,
        from_ns: Option<Identifier>,
    },
}

impl fmt::Display for SemanticErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            SemanticErrorKind::UnknownType(missing_type) => {
                write!(f, "type `{}` was not found", missing_type)
            }

            SemanticErrorKind::UnknownSymbol(missing_sym) => {
                write!(f, "symbol `{}` was not found", missing_sym)
            }

            SemanticErrorKind::UninitializedSymbol(name) => {
                write!(f, "symbol `{}` is not initialized", name)
            }

            SemanticErrorKind::MemberAccessOfNonRecord(actual, name) => {
                write!(f, "cannot access member {} of {} because it is not a record",
                       name,
                       actual.as_ref()
                           .map(|t| t.to_string())
                           .unwrap_or_else(|| "none".to_string()))
            }

            SemanticErrorKind::PrivateMemberAccessForbidden { base_type, from_ns, member_name } => {
                write!(f, "member `{}` of type `{}` cannot be accessed from unit `{}`",
                       member_name,
                       base_type,
                       from_ns.as_ref()
                           .map(|ns| ns.to_string())
                           .unwrap_or_else(|| "(root)".to_string()),
                )
            }

            SemanticErrorKind::UnexpectedType { expected, actual } => {
                write!(f, "expected type `{}`, found `{}`",
                       Type::name(expected.as_ref()),
                       Type::name(actual.as_ref()))
            }

            SemanticErrorKind::InvalidWithType(actual) => {
                write!(f, "{} is not a valid value for a `with` expression (expected record or class)",
                       Type::name(actual.as_ref()))
            }

            SemanticErrorKind::InvalidFunctionType(actual) => {
                let actual_name = actual.as_ref()
                    .map(|t| match t {
                        Type::Record(name) => format!("record `{}`", name),
                        Type::Class(name) => format!("class`{}`", name),
                        Type::Function(name) => format!("function `{}`", name),
                        _ => format!("type `{}`", t)
                    })
                    .unwrap_or_else(|| "(none)".to_owned());

                write!(f, "{} is not a callable function", actual_name)
            }

            SemanticErrorKind::InvalidConstantValue(expr) => {
                write!(f, "`{}` is not a valid constant value", expr.to_source())
            }

            SemanticErrorKind::InvalidConstructorType(actual) => {
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

            SemanticErrorKind::InvalidTypecast { target_type, from_type } => {
                match from_type {
                    None => write!(f, "can't cast untyped value"),
                    Some(from_type) => {
                        write!(f, "value of type `{}` cannot be cast to type `{}`",
                               from_type,
                               target_type)
                    }
                }
            }

            SemanticErrorKind::WrongNumberOfArgs { expected_sig, actual } => {
                write!(f, "wrong number of arguments passed to function (expected {}, found {})",
                       expected_sig.args.len(), actual)
            }

            SemanticErrorKind::WrongArgTypes { sig, actual } => {
                writeln!(f, "invalid arguments to function! expected:")?;
                for expected_arg in sig.args.iter() {
                    writeln!(f, "\t{}", expected_arg.to_source())?;
                }
                writeln!(f, "found: ")?;
                for actual_arg in actual.iter() {
                    writeln!(f, "\t{}", Type::name(actual_arg.as_ref()))?;
                }
                Ok(())
            }

            SemanticErrorKind::IllegalName(name) => {
                write!(f, "illegal name `{}`", name)
            }

            SemanticErrorKind::EmptyRecord(name) => {
                write!(f, "record type `{}` must have at least one member", name)
            }

            SemanticErrorKind::InvalidOperator { op, args } => {
                let args_list = args.iter()
                    .map(|arg| format!("`{}`", Type::name(arg.as_ref())))
                    .collect::<Vec<_>>()
                    .join(", ");

                write!(f, "the operator {} cannot be applied to the argument types {}", op, args_list)
            }

            SemanticErrorKind::InvalidArrayType(actual) => {
                write!(f, "type `{}` cannot be indexed as an array", Type::name(actual.as_ref()))
            }

            SemanticErrorKind::InvalidArrayIndex(actual) => {
                write!(f, "type `{}` cannot be used as an array index", Type::name(actual.as_ref()))
            }

            SemanticErrorKind::TypeNotAssignable(t) => {
                write!(f, "type `{}` cannot be assigned to",
                       Type::name(t.as_ref()))
            }

            SemanticErrorKind::ValueNotAssignable(expr) => {
                write!(f, "expression `{}` cannot be assigned to", expr.to_source())
            }

            SemanticErrorKind::TypesNotComparable(a, b) => {
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
    context: SemanticContext,
}

impl SemanticError {
    pub fn illegal_name(name: String, context: SemanticContext) -> Self {
        SemanticError {
            kind: SemanticErrorKind::IllegalName(name),
            context,
        }
    }

    pub fn unknown_symbol(name: Identifier, context: SemanticContext) -> Self {
        SemanticError {
            kind: SemanticErrorKind::UnknownSymbol(name),
            context,
        }
    }

    pub fn unexpected_type(expected: Option<Type>,
                           actual: Option<Type>,
                           context: SemanticContext) -> Self {
        SemanticError {
            kind: SemanticErrorKind::UnexpectedType { expected, actual },
            context,
        }
    }

    pub fn unknown_type(missing_type: Identifier, context: SemanticContext) -> Self {
        SemanticError {
            kind: SemanticErrorKind::UnknownType(missing_type),
            context,
        }
    }

    pub fn empty_record(record_id: Identifier, context: SemanticContext) -> Self {
        SemanticError {
            kind: SemanticErrorKind::EmptyRecord(record_id),
            context,
        }
    }

    pub fn member_of_non_record(actual: Option<Type>,
                                name: String,
                                context: SemanticContext) -> Self {
        SemanticError {
            kind: SemanticErrorKind::MemberAccessOfNonRecord(actual, name),
            context,
        }
    }

    pub fn private_member_access_forbidden(base_type: impl Into<Identifier>,
                                           from_ns: impl Into<Option<Identifier>>,
                                           member_name: impl ToString,
                                           context: impl Into<SemanticContext>)
                                           -> Self {
        SemanticError {
            kind: SemanticErrorKind::PrivateMemberAccessForbidden {
                base_type: base_type.into(),
                from_ns: from_ns.into(),
                member_name: member_name.to_string(),
            },
            context: context.into(),
        }
    }

    pub fn wrong_num_args(sig: FunctionSignature,
                          actual: usize,
                          context: SemanticContext) -> Self {
        SemanticError {
            kind: SemanticErrorKind::WrongNumberOfArgs {
                expected_sig: sig,
                actual,
            },
            context,
        }
    }

    pub fn wrong_arg_types(sig: impl Into<FunctionSignature>,
                           actual: impl IntoIterator<Item=Option<Type>>,
                           context: impl Into<SemanticContext>) -> Self {
        SemanticError {
            context: context.into(),
            kind: SemanticErrorKind::WrongArgTypes {
                sig: sig.into(),
                actual: actual.into_iter().collect(),
            },
        }
    }

    pub fn invalid_with_type(actual: Option<Type>, context: SemanticContext) -> Self {
        SemanticError {
            kind: SemanticErrorKind::InvalidWithType(actual),
            context,
        }
    }

    pub fn invalid_function_type(actual: Option<Type>, context: SemanticContext) -> Self {
        SemanticError {
            kind: SemanticErrorKind::InvalidFunctionType(actual),
            context,
        }
    }

    pub fn invalid_typecast(target_type: impl Into<Type>,
                            from_type: impl Into<Option<Type>>,
                            context: impl Into<SemanticContext>) -> Self {
        SemanticError {
            kind: SemanticErrorKind::InvalidTypecast {
                from_type: from_type.into(),
                target_type: target_type.into(),
            },
            context: context.into(),
        }
    }

    pub fn uninitialized_symbol(name: impl Into<Identifier>,
                                context: impl Into<SemanticContext>)
                                -> Self {
        SemanticError {
            kind: SemanticErrorKind::UninitializedSymbol(name.into()),
            context: context.into(),
        }
    }

    pub fn invalid_const_value(value_expr: Expression) -> Self {
        SemanticError {
            kind: SemanticErrorKind::InvalidConstantValue(value_expr.value),
            context: value_expr.context,
        }
    }

    pub fn invalid_constructor_type(actual: Option<Type>, context: SemanticContext) -> Self {
        SemanticError {
            kind: SemanticErrorKind::InvalidConstructorType(actual),
            context,
        }
    }

    pub fn invalid_destructor_return(return_type: Type, context: SemanticContext)
                                     -> SemanticError {
        SemanticError {
            kind: SemanticErrorKind::InvalidDestructorReturn(return_type),
            context,
        }
    }

    pub fn invalid_destructor_args(args: impl IntoIterator<Item=Type>,
                                   context: SemanticContext)
                                   -> SemanticError {
        SemanticError {
            kind: SemanticErrorKind::InvalidDestructorArgs(args.into_iter().collect()),
            context,
        }
    }

    pub fn invalid_operator(operator: operators::Operator,
                            args: impl IntoIterator<Item=Option<Type>>,
                            context: SemanticContext) -> SemanticError {
        SemanticError {
            kind: SemanticErrorKind::InvalidOperator {
                op: operator,
                args: args.into_iter().collect(),
            },
            context,
        }
    }

    pub fn invalid_array_type(actual: impl Into<Option<Type>>,
                              context: impl Into<SemanticContext>)
                              -> Self {
        SemanticError {
            kind: SemanticErrorKind::InvalidArrayType(actual.into()),
            context: context.into(),
        }
    }

    pub fn invalid_array_index(actual: impl Into<Option<Type>>,
                               context: impl Into<SemanticContext>)
                               -> Self {
        SemanticError {
            kind: SemanticErrorKind::InvalidArrayIndex(actual.into()),
            context: context.into(),
        }
    }

    pub fn type_not_assignable(t: Option<Type>,
                               context: SemanticContext) -> SemanticError {
        SemanticError {
            kind: SemanticErrorKind::TypeNotAssignable(t),
            context,
        }
    }

    pub fn value_not_assignable(expr: Expression) -> SemanticError {
        let context = expr.context.clone();
        SemanticError {
            kind: SemanticErrorKind::ValueNotAssignable(expr),
            context,
        }
    }

    pub fn types_not_comparable(a: Option<Type>, b: Option<Type>,
                                context: SemanticContext) -> SemanticError {
        SemanticError {
            kind: SemanticErrorKind::TypesNotComparable(a, b),
            context,
        }
    }
}

impl fmt::Display for SemanticError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}\n  {}", self.context.token(), self.kind)
    }
}

pub type SemanticResult<T> = Result<T, SemanticError>;

#[derive(Clone)]
pub struct SemanticContext {
    token: source::Token,
    scope: Rc<Scope>,
}

impl SemanticContext {
    pub fn new(token: impl Into<source::Token>, scope: impl Into<Rc<Scope>>) -> Self {
        Self {
            token: token.into(),
            scope: scope.into(),
        }
    }
}

impl fmt::Debug for SemanticContext {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} in {}", self.token, self.scope.local_namespace()
            .map(|id| id.to_string())
            .unwrap_or("module root".to_string()))
    }
}

impl Context for SemanticContext {
    type Type = Type;

    fn token(&self) -> &source::Token {
        &self.token
    }
}