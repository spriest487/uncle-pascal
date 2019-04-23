use {
    crate::{
        Type,
        ValueKind,
        context::NameError,
        ast::{
            ExpressionNode,
            Call,
        },
    },
    std::fmt,
    pas_common::span::*,
    pas_syn::{
        Ident,
        Operator,
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
    NotMutable(Box<ExpressionNode>),
    NotAddressable {
        ty: Type,
        value_kind: Option<ValueKind>,
        span: Span
    },
    NotDerefable {
        ty: Type,
        span: Span,
    },
    InvalidUnaryOp {
        op: Operator,
        operand: Type,
        span: Span,
    },
    InvalidBinOp {
        lhs: Type,
        rhs: Type,
        op: Operator,
        span: Span,
    },
    InvalidBlockOutput(Box<ExpressionNode>),
    AmbiguousMethod {
        method: Ident,
        span: Span,
    }
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
            TypecheckError::InvalidCallInExpression(call) => call.annotation().span(),
            TypecheckError::TypeMismatch { span, .. } => span,
            TypecheckError::NotMutable(expr) => expr.annotation.span(),
            TypecheckError::NotAddressable { span, .. } => span,
            TypecheckError::NotDerefable { span, .. } => span,
            TypecheckError::InvalidBinOp { span, .. } => span,
            TypecheckError::InvalidUnaryOp { span, .. } => span,
            TypecheckError::InvalidBlockOutput(expr) => expr.annotation.span(),
            TypecheckError::AmbiguousMethod { span, .. } => span,
        }
    }

    fn fmt_context(&self, mut f: impl fmt::Write, source: &str) -> fmt::Result {
        match self {
            TypecheckError::ScopeError(err) => err.fmt_context(f, source),
            _ => {
                writeln!(f, "{}", self)?;
                self.span().fmt_context(f, source)
            }
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
                write!(f, "type mismatch: expected {}, found {}", expected, actual)
            }

            TypecheckError::NotMutable(expr) => {
                write!(f, "`{}` does not refer to a mutable value", expr)
            }

            TypecheckError::NotAddressable { ty, value_kind, .. } => {
                match value_kind {
                    Some(value_kind) => write!(f, "{} of type {} cannot have its address taken",  value_kind, ty),
                    None => write!(f, "expression without a value cannot have its address taken"),
                }
            }

            TypecheckError::NotDerefable { ty, .. } => {
                write!(f, "value of type {} cannot be dereferenced", ty,)
            }

            TypecheckError::InvalidBinOp { lhs, rhs, op, .. } => {
                write!(f, "operator {} cannot be applied to the operand types {} and {}", op, lhs, rhs)
            }

            TypecheckError::InvalidUnaryOp { operand, op, .. } => {
                write!(f, "operator {} cannot be applied to an operand of type {}", op, operand)
            }

            TypecheckError::InvalidBlockOutput(expr) => {
                write!(f, "expression `{}` is not valid as the final expression in a block because it is not a complete statement", expr)
            }

            TypecheckError::AmbiguousMethod { method, .. } => {
                // todo: show ambiguous options
                write!(f, "call to method `{}` was ambiguous", method)
            }
        }
    }
}