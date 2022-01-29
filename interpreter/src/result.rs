use crate::{heap::NativeHeapError, marshal::MarshalError, Pointer};
use pas_common::{span::Span, DiagnosticLabel, DiagnosticOutput};
use std::fmt;
use crate::stack::StackError;

#[derive(Debug)]
pub enum ExecError {
    Raised {
        msg: String,
    },
    MarshalError(MarshalError),
    StackError(StackError),
    ExternSymbolLoadFailed {
        msg: String,
        lib: String,
        symbol: String,
    },
    IllegalDereference {
        ptr: Pointer,
    },
    IllegalState {
        msg: String,
    },
    NativeHeapError(NativeHeapError),
    ZeroLengthAllocation,
    WithDebugContext {
        err: Box<ExecError>,
        span: Span,
    }
}

impl ExecError {
    pub fn illegal_state(msg: impl Into<String>) -> Self {
        Self::IllegalState {
            msg: msg.into(),
        }
    }

    fn label_text(&self) -> Option<String> {
        match self {
            ExecError::Raised { msg } => Some(msg.clone()),
            ExecError::ExternSymbolLoadFailed { msg, .. } => Some(msg.clone()),
            ExecError::MarshalError(err) => Some(err.to_string()),
            ExecError::IllegalDereference { ptr } => Some(format!("failed to dereference pointer: {}", ptr)),
            ExecError::IllegalState { msg } => Some(msg.clone()),
            ExecError::NativeHeapError(err) => Some(err.to_string()),
            ExecError::ZeroLengthAllocation => None,
            ExecError::StackError(err) => Some(err.to_string()),
            _ => None,
        }
    }

    pub fn with_debug_ctx(self, context: Span) -> Self {
        ExecError::WithDebugContext {
            err: Box::new(self),
            span: context,
        }
    }
}

impl fmt::Display for ExecError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ExecError::Raised { .. } => write!(f, "Runtime error raised"),
            ExecError::MarshalError(err) => write!(f, "{}", err),
            ExecError::StackError(err) => write!(f, "{}", err),
            ExecError::ExternSymbolLoadFailed { lib, symbol, .. } => {
                write!(f, "Failed to load {}::{}", lib, symbol)
            }
            ExecError::IllegalDereference { .. } => write!(f, "Illegal dereference"),
            ExecError::IllegalState { .. } => write!(f, "Illegal interpreter state"),
            ExecError::NativeHeapError(err) => write!(f, "{}", err),
            ExecError::ZeroLengthAllocation => write!(f, "Dynamic allocation with length 0"),
            ExecError::WithDebugContext { .. } => write!(f, "Execution error"),
        }
    }
}

impl DiagnosticOutput for ExecError {
    fn label(&self) -> Option<DiagnosticLabel> {
        match self {
            ExecError::WithDebugContext { err, span } => {
                let label_text = err.label_text();

                Some(DiagnosticLabel {
                    text: label_text,
                    span: span.clone()
                })
            },

            _ => None,
        }
    }
}

impl From<NativeHeapError> for ExecError {
    fn from(err: NativeHeapError) -> Self {
        ExecError::NativeHeapError(err)
    }
}

impl From<MarshalError> for ExecError {
    fn from(err: MarshalError) -> Self {
        ExecError::MarshalError(err)
    }
}

impl From<StackError> for ExecError {
    fn from(err: StackError) -> Self { ExecError::StackError(err) }
}

pub type ExecResult<T> = Result<T, ExecError>;
