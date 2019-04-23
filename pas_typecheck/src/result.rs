use crate::{
    ast::{
        Call,
        ExpressionNode,
    },
    context::NameError,
    Type,
    ValueKind,
};
use pas_common::{
    span::*,
    Backtrace,
    DiagnosticMessage,
    DiagnosticOutput,
};
use pas_syn::{
    Ident,
    Operator,
};
use std::fmt;

#[derive(Debug)]
pub enum TypecheckError {
    ScopeError(NameError),
    NotCallable(Box<ExpressionNode>),
    InvalidArgs {
        expected: Vec<Type>,
        actual: Vec<Type>,
        span: Span,
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
        span: Span,
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
    },
    InvalidCtorType {
        ty: Type,
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
            TypecheckError::InvalidCallInExpression(call) => call.annotation().span(),
            TypecheckError::TypeMismatch { span, .. } => span,
            TypecheckError::NotMutable(expr) => expr.annotation.span(),
            TypecheckError::NotAddressable { span, .. } => span,
            TypecheckError::NotDerefable { span, .. } => span,
            TypecheckError::InvalidBinOp { span, .. } => span,
            TypecheckError::InvalidUnaryOp { span, .. } => span,
            TypecheckError::InvalidBlockOutput(expr) => expr.annotation.span(),
            TypecheckError::AmbiguousMethod { span, .. } => span,
            TypecheckError::InvalidCtorType { span, .. } => span,
        }
    }
}

impl DiagnosticOutput for TypecheckError {
    fn title(&self) -> String {
        match self {
            TypecheckError::ScopeError(err) => err.title(),
            TypecheckError::NotCallable(_) => "Not callable".to_string(),
            TypecheckError::InvalidArgs { .. } => "Invalid arguments".to_string(),
            TypecheckError::InvalidCallInExpression(_) => "Invalid call in expression".to_string(),
            TypecheckError::TypeMismatch { .. } => "Type mismatch".to_string(),
            TypecheckError::NotMutable(_) => "Value not mutable".to_string(),
            TypecheckError::NotAddressable { .. } => "Value not addressable".to_string(),
            TypecheckError::NotDerefable { .. } => "Value cannot be dereferenced".to_string(),
            TypecheckError::InvalidBinOp { .. } => "Invalid binary operation".to_string(),
            TypecheckError::InvalidUnaryOp { .. } => "Invalid unary operation".to_string(),
            TypecheckError::InvalidBlockOutput(_) => "Invalid block output expression".to_string(),
            TypecheckError::AmbiguousMethod { .. } => "Method reference is ambiguous".to_string(),
            TypecheckError::InvalidCtorType { .. } => "Invalid constructor expression type".to_string(),
        }
    }

    fn label(&self) -> Option<String> {
        match self {
            TypecheckError::ScopeError(err) => err.label(),
            _ => Some(self.to_string()),
        }
    }

    fn see_also(&self) -> Vec<DiagnosticMessage> {
        match self {
            TypecheckError::ScopeError(err) => err.see_also(),
            _ => Vec::new(),
        }
    }

    fn backtrace(&self) -> Option<&Backtrace> {
        None
    }
}

fn write_args<'a>(f: &mut fmt::Formatter, args: impl IntoIterator<Item = &'a Type>) -> fmt::Result {
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
                    Some(value_kind) => write!(f, "{} of type {} cannot have its address taken", value_kind, ty),
                    None => write!(f, "expression without a value cannot have its address taken"),
                }
            }

            TypecheckError::NotDerefable { ty, .. } => {
                write!(f, "value of type {} cannot be dereferenced", ty, )
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

            TypecheckError::InvalidCtorType { ty, .. } => {
                write!(f, "type `{}` cannot be created with a constructor expression", ty)
            }
        }
    }
}
