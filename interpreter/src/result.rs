use crate::{HeapAddress, MemCellKind};
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
    IllegalDereference(IllegalDereference),
    IllegalState {
        msg: String,
    },
    IllegalHeapAccess(HeapAddress),
}

impl ExecError {
    pub fn illegal_state(msg: impl Into<String>) -> Self {
        Self::IllegalState { msg: msg.into() }
    }

    pub fn expected_ty(msg: &str, expected: MemCellKind, actual: MemCellKind) -> Self {
        Self::IllegalState {
            msg: format!("{} - expected {} value, got {}", msg, expected, actual),
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
            ExecError::IllegalDereference(..) => write!(f, "Illegal dereference"),
            ExecError::IllegalState { msg } => write!(f, "Illegal interpreter state: {}", msg),
            ExecError::IllegalHeapAccess(addr) => write!(f, "Illegal heap access at address {}", addr),
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
            ExecError::ExternSymbolLoadFailed { span, msg, .. } => Some(DiagnosticLabel {
                text: Some(msg.clone()),
                span: span.clone(),
            }),
            _ => None,
        }
    }
}

#[derive(Debug)]
pub enum IllegalDereference {
    Null,
    Uninit,
    Array,
    Struct,
    ExternalPointer,
}

impl fmt::Display for IllegalDereference {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            IllegalDereference::Array => write!(f, "array pointer does not refer to an array"),
            IllegalDereference::Struct => write!(f, "struct pointer does not refer to a struct"),
            IllegalDereference::ExternalPointer => {
                write!(f, "interpreter cannot dereference external pointer")
            }
            IllegalDereference::Null => write!(f, "dereferencing null pointer"),
            IllegalDereference::Uninit => write!(f, "dereferencing uninitialized pointer"),
        }
    }
}

pub type ExecResult<T> = Result<T, ExecError>;
