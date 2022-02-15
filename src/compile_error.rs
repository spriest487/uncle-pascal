use std::{fmt, io};
use std::path::PathBuf;

use pas_common::span::Span;
use pas_common::{Backtrace, DiagnosticLabel, DiagnosticMessage, DiagnosticOutput, TracedError};
use pas_interpreter::result::ExecError;
use pas_pp::PreprocessorError;
use pas_syn::parse::ParseError;
use pas_syn::{IdentPath, TokenizeError};
use pas_typecheck::TypecheckError;

#[derive(Debug)]
pub enum CompileError {
    TokenizeError(TracedError<TokenizeError>),
    ParseError(TracedError<ParseError>),
    TypecheckError(TypecheckError),
    PreprocessorError(PreprocessorError),
    InvalidUnitFilename(Span),
    DuplicateUnit {
        unit_ident: IdentPath,
        duplicate_path: PathBuf,
    },
    FileNotFound(PathBuf, Option<Span>),
    OutputFailed(Span, io::Error),
    ExecError(ExecError),
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
            },
            CompileError::InvalidUnitFilename(at) => DiagnosticMessage {
                title: "Invalid unit filename".to_string(),
                label: Some(DiagnosticLabel {
                    text: None,
                    span: at.clone(),
                }),
            },
            CompileError::ExecError(ExecError::Raised { msg, .. }) => DiagnosticMessage {
                title: msg.clone(),
                label: None,
            },
            CompileError::ExecError(err) => err.main(),
            CompileError::FileNotFound(path, span) => DiagnosticMessage {
                title: format!("file not found: {}", path.display()),
                label: span.as_ref().map(|span| DiagnosticLabel {
                    text: None,
                    span: span.clone(),
                })
            },
            CompileError::DuplicateUnit { unit_ident, duplicate_path } => DiagnosticMessage {
                title: format!("`{}` @ {} was already loaded", unit_ident, duplicate_path.display()),
                label: None,
            }
        }
    }

    fn see_also(&self) -> Vec<DiagnosticMessage> {
        match self {
            CompileError::TokenizeError(err) => err.see_also(),
            CompileError::ParseError(err) => err.see_also(),
            CompileError::TypecheckError(err) => err.see_also(),
            CompileError::PreprocessorError(err) => err.see_also(),
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
            CompileError::InvalidUnitFilename(span) => write!(
                f,
                "invalid unit identifier in filename: {}",
                span.file.display()
            ),
            CompileError::OutputFailed(span, err) => {
                write!(f, "writing to file {} failed: {}", span.file.display(), err,)
            }
            CompileError::ExecError(err) => write!(f, "{}", err),
            CompileError::DuplicateUnit { .. } => write!(f, "Unit was already loaded"),
            CompileError::FileNotFound(_, _) => write!(f, "File not found"),
        }
    }
}
