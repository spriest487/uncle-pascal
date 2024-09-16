use frontend::ast::IdentPath;
use frontend::parse::ParseError;
use frontend::pp::error::PreprocessorError;
use frontend::typ::TypecheckError;
use frontend::TokenizeError;
use common::span::Span;
use common::Backtrace;
use common::DiagnosticLabel;
use common::DiagnosticMessage;
use common::DiagnosticOutput;
use common::TracedError;
use interpreter::result::ExecError;
use std::path::PathBuf;
use std::{fmt, io};

#[derive(Debug)]
pub enum CompileError {
    TokenizeError(TracedError<TokenizeError>),
    ParseError(TracedError<ParseError>),
    TypecheckError(TypecheckError),
    PreprocessorError(PreprocessorError),
    ExecError(ExecError),

    FileNotFound(PathBuf, Option<Span>),
    ReadSourceFileFailed {
        path: PathBuf,
        msg: String,
    },
    OutputFailed(Span, io::Error),
    DuplicateUnit {
        unit_ident: IdentPath,
        duplicate_path: PathBuf,
    },
    CircularDependency {
        unit_ident: IdentPath,
        used_unit: IdentPath,
        span: Span,
    }
}

impl From<TracedError<TokenizeError>> for CompileError {
    fn from(err: TracedError<TokenizeError>) -> Self {
        CompileError::TokenizeError(err)
    }
}

impl From<TracedError<ParseError>> for CompileError {
    fn from(err: TracedError<ParseError>) -> Self {
        CompileError::ParseError(err)
    }
}

impl From<TypecheckError> for CompileError {
    fn from(err: TypecheckError) -> Self {
        CompileError::TypecheckError(err)
    }
}

impl From<PreprocessorError> for CompileError {
    fn from(err: PreprocessorError) -> Self {
        CompileError::PreprocessorError(err)
    }
}

impl From<ExecError> for CompileError {
    fn from(err: ExecError) -> Self {
        CompileError::ExecError(err)
    }
}

impl DiagnosticOutput for CompileError {
    fn main(&self) -> DiagnosticMessage {
        match self {
            CompileError::TokenizeError(err) => err.err.main(),
            CompileError::ParseError(err) => err.err.main(),
            CompileError::TypecheckError(err) => err.main(),
            CompileError::PreprocessorError(err) => err.main(),
            CompileError::OutputFailed(span, err) => DiagnosticMessage {
                title: format!(
                    "Writing output file `{}` failed: {}",
                    span.file.display(),
                    err
                ),
                label: None,
                notes: Vec::new(),
            },
            CompileError::ExecError(ExecError::Raised { msg, .. }) => DiagnosticMessage {
                title: msg.clone(),
                label: None,
                notes: Vec::new(),
            },
            CompileError::ExecError(err) => err.main(),
            CompileError::FileNotFound(path, span) => DiagnosticMessage {
                title: format!("file not found: {}", path.display()),
                label: span.as_ref().map(|span| DiagnosticLabel {
                    text: None,
                    span: span.clone(),
                }),
                notes: Vec::new(),
            },
            CompileError::ReadSourceFileFailed { path, .. } => DiagnosticMessage {
                title: format!("failed to read source file {}", path.to_string_lossy()),
                label: None,
                notes: Vec::new(),
            },
            CompileError::DuplicateUnit { unit_ident, duplicate_path } => DiagnosticMessage {
                title: format!("`{}` @ {} was already loaded", unit_ident, duplicate_path.display()),
                label: None,
                notes: Vec::new(),
            },
            CompileError::CircularDependency { unit_ident, used_unit, span } => DiagnosticMessage {
                title: format!("unit `{}` used from `{}` creates a circular reference", used_unit, unit_ident),
                label: Some(DiagnosticLabel {
                    text: Some("unit used here".to_string()),
                    span: span.clone(),
                }),
                notes: Vec::new(),
            }
        }
    }

    fn see_also(&self) -> Vec<DiagnosticMessage> {
        match self {
            CompileError::TokenizeError(err) => err.see_also(),
            CompileError::ParseError(err) => err.see_also(),
            CompileError::TypecheckError(err) => err.see_also(),
            CompileError::PreprocessorError(err) => err.see_also(),
            CompileError::ExecError(err) => err.see_also(),
            _ => Vec::new(),
        }
    }

    fn backtrace(&self) -> Option<&Backtrace> {
        match self {
            CompileError::TokenizeError(err) => Some(&err.bt),
            CompileError::ParseError(err) => Some(&err.bt),
            _ => None,
        }
    }
}

impl fmt::Display for CompileError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            CompileError::TokenizeError(err) => write!(f, "{}", err.err),
            CompileError::ParseError(err) => write!(f, "{}", err.err),
            CompileError::TypecheckError(err) => write!(f, "{}", err),
            CompileError::PreprocessorError(err) => write!(f, "{}", err),
            CompileError::ReadSourceFileFailed { msg, .. } => write!(f, "{}", msg),
            CompileError::OutputFailed(span, err) => {
                write!(f, "writing to file {} failed: {}", span.file.display(), err,)
            }
            CompileError::ExecError(err) => write!(f, "{}", err),
            CompileError::DuplicateUnit { .. } => write!(f, "unit was already loaded"),
            CompileError::FileNotFound(_, _) => write!(f, "file not found"),
            CompileError::CircularDependency { .. } => write!(f, "circular unit reference"),
        }
    }
}
