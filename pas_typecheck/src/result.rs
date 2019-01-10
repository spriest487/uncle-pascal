use {
    std::fmt,
    crate::context::NameError,
    pas_syn::{
        Span,
        Spanned,
    }
};

#[derive(Debug)]
pub enum TypecheckError {
    ScopeError(NameError),
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
        }
    }
}

impl fmt::Display for TypecheckError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TypecheckError::ScopeError(err) => write!(f, "{}", err),
        }
    }
}