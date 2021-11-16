use crate::{HeapAddress, ValueType, Pointer};
use pas_common::span::Span;
use pas_common::{DiagnosticLabel, DiagnosticOutput};
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
    IllegalDereference {
        ptr: Pointer,
        span: Span,
    },
    IllegalState {
        msg: String,
        span: Span,
    },
    IllegalHeapAccess{
        addr: HeapAddress,
        span: Span,
    },
}

impl ExecError {
    pub fn illegal_state(msg: impl Into<String>, span: Span) -> Self {
        Self::IllegalState {
            msg: msg.into(),
            span }
    }

    pub fn expected_ty(msg: &str, expected: ValueType, actual: ValueType, span: Span) -> Self {
        Self::IllegalState {
            msg: format!("{} - expected {} value, got {}", msg, expected, actual),
            span,
        }
    }
}

impl fmt::Display for ExecError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ExecError::Raised { .. } => write!(f, "Runtime error raised"),
            ExecError::MarshallingFailed { .. } => write!(f, "Marshalling failed"),
            ExecError::ExternSymbolLoadFailed { lib, symbol, .. } => {
                write!(f, "Failed to load {}::{}", lib, symbol)
            }
            ExecError::IllegalDereference{ .. } => write!(f, "Illegal dereference"),
            ExecError::IllegalState { .. } => write!(f, "Illegal interpreter state"),
            ExecError::IllegalHeapAccess{ .. } => write!(f, "Illegal heap access at address"),
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
            ExecError::ExternSymbolLoadFailed { span, msg, .. } => Some(DiagnosticLabel {
                text: Some(msg.clone()),
                span: span.clone(),
            }),
            ExecError::MarshallingFailed { failed_ty, span } => Some(DiagnosticLabel {
                text: Some(format!("failed to marshal value of type `{}`", failed_ty)),
                span: span.clone(),
            }),
            ExecError::IllegalDereference { ptr, span } => Some(DiagnosticLabel {
                text: Some(match ptr {
                    Pointer::Null => "dereferenced null pointer",
                    Pointer::Uninit => "dereferenced uninitialized pointer",
                    Pointer::IntoArray { .. } => "dereferenced invalid array element pointer",
                    Pointer::IntoStruct { .. } => "dereferenced invalid struct member pointer",
                    Pointer::VariantTag { .. } => "dereferenced invalid variant tag pointer",
                    Pointer::VariantData { .. } => "dereferenced invalid variant data pointer",
                    Pointer::External(..) => "interpreter cannot dereference pointer from external code",
                    _ => "illegal pointer dereference",
                }.to_string()),
                span: span.clone(),
            }),
            ExecError::IllegalHeapAccess { addr, span } => Some(DiagnosticLabel {
                text: Some(format!("illegal access of unallocated heap location {}", addr)),
                span: span.clone(),
            }),
            ExecError::IllegalState { msg, span } => Some(DiagnosticLabel {
                text: Some(msg.clone()),
                span: span.clone(),
            })
        }
    }
}

pub type ExecResult<T> = Result<T, ExecError>;