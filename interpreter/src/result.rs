use pas_common::span::{Span, Spanned};
use pas_common::{ DiagnosticLabel, DiagnosticOutput};
use pas_ir::Type;
use std::fmt;

#[derive(Debug)]
pub enum ExecError {
    Raised {
        msg: String,
        span: Span,
    },
    MarshallingFailed {
        failed_ty: Type,
        span: Span,
    },
    ExternSymbolLoadFailed {
        lib: String,
        symbol: String,
        span: Span,
        msg: String,
    },
}

impl fmt::Display for ExecError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ExecError::Raised { .. } => write!(f, "Runtime error raised"),
            ExecError::MarshallingFailed { .. } => write!(f, "Marshalling failed"),
            ExecError::ExternSymbolLoadFailed { lib, symbol, .. } => write!(f, "Failed to load {}::{}", lib, symbol),
        }
    }
}

impl Spanned for ExecError {
    fn span(&self) -> &Span {
        match &self {
            ExecError::Raised { span, .. } => span,
            ExecError::MarshallingFailed { span, .. } => span,
            ExecError::ExternSymbolLoadFailed { span, .. } => span,
        }
    }
}

impl DiagnosticOutput for ExecError {
    fn label(&self) -> Option<DiagnosticLabel> {
        match self {
            ExecError::Raised { msg, span } => Some(DiagnosticLabel {
                text: Some(msg.clone()),
                span: span.clone(),
            }),
            ExecError::MarshallingFailed { span, failed_ty } => Some(DiagnosticLabel {
                text: Some(format!("marshalling failed for type: {}", failed_ty)),
                span: span.clone(),
            }),
            ExecError::ExternSymbolLoadFailed {  span, msg, .. } => Some(DiagnosticLabel {
                text: Some(msg.clone()),
                span: span.clone(),
            })
        }
    }
}

pub type ExecResult<T> = Result<T, ExecError>;
