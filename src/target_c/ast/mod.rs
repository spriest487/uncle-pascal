use std::fmt;

pub mod function;
pub mod ctype;
pub mod translation_unit;

pub use self::function::*;
pub use self::ctype::*;
pub use self::translation_unit::*;

pub type TranslationResult<T> = Result<T, TranslationError>;

#[derive(Debug)]
pub enum TranslationError {
    WriteFailed(fmt::Error),
}

impl From<fmt::Error> for TranslationError {
    fn from(err: fmt::Error) -> Self {
        TranslationError::WriteFailed(err)
    }
}

impl fmt::Display for TranslationError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TranslationError::WriteFailed(err) => write!(f, "{}", err),
        }
    }
}