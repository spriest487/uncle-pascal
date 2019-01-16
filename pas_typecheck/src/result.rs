use {
    std::fmt,
    pas_common::span::*,
    pas_syn::Ident,
    crate::{
        Type,
        context::NameError,
        ast::{
            ExpressionNode,
            Call,
        },
    },
};

#[derive(Debug)]
pub enum TypecheckError {
    ScopeError(NameError),
    NotCallable(Box<ExpressionNode>),
    InvalidArgs {
        expected: Vec<Type>,
        actual: Vec<Type>,
        span: Span
    },
    InvalidCallInExpression(Call),
    TypeMismatch {
        expected: Type,
        actual: Type,
        span: Span,
    },
    MemberNotFound {
        base: Type,
        member: Ident,
        span: Span,
    },
}

pub type TypecheckResult<T> = Result<T, TypecheckError>;

impl From<NameError> for TypecheckError {
    fn from(err: NameError) -> Self {
        TypecheckError::ScopeError(err)
    }
}

impl Spanned for TypecheckError {
    fn span(&self) -> &Span {
        match self {
            TypecheckError::ScopeError(err) => err.span(),
            TypecheckError::NotCallable(expr) => expr.annotation.span(),
            TypecheckError::InvalidArgs { span, .. } => span,
            TypecheckError::InvalidCallInExpression(call) => call.annotation.span(),
            TypecheckError::TypeMismatch { span, .. } => span,
            TypecheckError::MemberNotFound { span, .. } => span,
        }
    }
}

fn write_args<'a>(f: &mut fmt::Formatter, args: impl IntoIterator<Item=&'a Type>) -> fmt::Result {
    for (i, arg) in args.into_iter().enumerate() {
        if i > 0 {
            write!(f, ", ")?;
        }
        write!(f, "{}", arg)?;
    }
    Ok(())
}

impl fmt::Display for TypecheckError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TypecheckError::ScopeError(err) => write!(f, "{}", err),
            TypecheckError::NotCallable(expr) => {
                write!(f, "expression `{}` is not a callable function", expr)
            }

            TypecheckError::InvalidArgs { expected, actual, .. } => {
                if expected.len() != actual.len() {
                    write!(f, "expected {} args, got {}", expected.len(), actual.len())
                } else {
                    write!(f, "expected arguments (")?;
                    write_args(f, expected.iter())?;
                    write!(f, "), found (")?;
                    write_args(f, actual.iter())?;
                    write!(f, ")")
                }
            }

            TypecheckError::InvalidCallInExpression(call) => {
                write!(f, "function call `{}` returns no value and cannot be used as part of an expression", call)
            }

            TypecheckError::TypeMismatch { expected, actual, .. } => {
                write!(f, "type mismatch: expected ")?;
                write!(f, "{}", expected)?;
                write!(f, ", found ")?;
                write!(f, "{}", actual)
            }

            TypecheckError::MemberNotFound { base, member, .. } => {
                write!(f, "type ")?;
                write!(f, "{}", base)?;
                write!(f, " does not have a member named `{}`", member)
            }
        }
    }
}