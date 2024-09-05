use crate::{heap::NativeHeapError, marshal::MarshalError, Pointer};
use pas_common::{span::Span, DiagnosticLabel, DiagnosticOutput};
use std::fmt;
use pas_ir::{Instruction, InstructionFormatter, RawInstructionFormatter};
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
    IllegalInstruction(Instruction),
    IllegalState {
        msg: String,
    },
    NativeHeapError(NativeHeapError),
    ZeroLengthAllocation,
    WithDebugContext {
        err: Box<ExecError>,
        span: Option<Span>,
        stack_trace: Vec<String>,
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
            ExecError::IllegalInstruction(i) => Some(i.to_string()),
            ExecError::WithDebugContext { err, .. } => err.label_text(),
        }
    }

    fn label_span(&self) -> Option<&Span> {
        match self {
            ExecError::WithDebugContext { span, .. } => span.as_ref(),
            _ => None,
        }
    }

    pub fn fmt_pretty<Pretty>(&self, f: &mut fmt::Formatter, pretty: &Pretty) -> fmt:: Result
    where
        Pretty: InstructionFormatter
    {
        match self {
            ExecError::WithDebugContext { err, stack_trace, .. } => {
                err.fmt_pretty(f, pretty)?;
                for stack_trace_line in stack_trace {
                    writeln!(f)?;
                    write!(f, "\tat {}", stack_trace_line)?;
                }
                Ok(())
            },
            _ => {
                match self {
                    ExecError::Raised { .. } => write!(f, "Runtime error raised"),
                    ExecError::MarshalError(err) => err.fmt_pretty(f, pretty),
                    ExecError::StackError(err) => write!(f, "{}", err),
                    ExecError::ExternSymbolLoadFailed { lib, symbol, .. } => {
                        write!(f, "Failed to load {}::{}", lib, symbol)
                    }
                    ExecError::IllegalDereference { .. } => write!(f, "Illegal dereference"),
                    ExecError::IllegalState { .. } => write!(f, "Illegal interpreter state"),
                    ExecError::NativeHeapError(..) => write!(f, "Heap error"),
                    ExecError::ZeroLengthAllocation => write!(f, "Dynamic allocation with length 0"),
                    ExecError::IllegalInstruction(..) => write!(f, "Illegal instruction"),
                    ExecError::WithDebugContext { .. } => unreachable!(),
                }?;

                // if no spanned label will be shown, output the label text as part of the main message
                if let (Some(label_text), None) = (self.label_text(), self.label_span()) {
                    write!(f, ": {}", label_text)?;
                }
                
                Ok(())
            },
        }?;

        Ok(())
    }
}

impl fmt::Display for ExecError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.fmt_pretty(f, &RawInstructionFormatter)
    }
}

impl DiagnosticOutput for ExecError {
    fn label(&self) -> Option<DiagnosticLabel> {
        match self {
            ExecError::WithDebugContext { err, span: Some(span), .. } => {
                let label_text = err.label_text();

                Some(DiagnosticLabel {
                    text: label_text,
                    span: span.clone()
                })
            },

            _ => None,
        }
    }

    fn notes(&self) -> Vec<String> {
        match self {
            ExecError::WithDebugContext { .. } => Vec::new(),
            _ => self.label_text().map(|text| vec![text]).unwrap_or_else(Vec::new),
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
