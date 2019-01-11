use {
    std::fmt,
    pas_syn::{
        Span,
        Spanned,
    },
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
        }
    }
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
                    fn write_args(f: &mut fmt::Formatter, args: &[Type]) -> fmt::Result {
                        for (i, arg) in args.iter().enumerate() {
                            if i > 0 {
                                write!(f, ", ")?;
                            }
                            write!(f, "{}", arg)?;
                        }
                        Ok(())
                    };

                    write!(f, "expected arguments (")?;
                    write_args(f, &expected)?;
                    write!(f, "), found (")?;
                    write_args(f, &actual)?;
                    write!(f, ")")
                }
            }

            TypecheckError::InvalidCallInExpression(call) => {
                write!(f, "function call `{}` returns no value and cannot be used as part of an expression", call)
            }
        }
    }
}