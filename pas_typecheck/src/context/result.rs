use {
    crate::{
        context::*,
        Type,
    },
    std::{
        fmt
    },
    pas_syn::{
        Ident,
    },
    pas_common::{
        span::*,
    },
};

#[derive(Debug)]
pub enum UnexpectedValue {
    Decl(Decl),
    Namespace(Ident),
}

impl From<Decl> for UnexpectedValue {
    fn from(decl: Decl) -> Self {
        UnexpectedValue::Decl(decl)
    }
}

impl fmt::Display for UnexpectedValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            UnexpectedValue::Decl(decl) => write!(f, "{}", decl),
            UnexpectedValue::Namespace(ident) => write!(f, "namespace `{}`", ident),
        }
    }
}

#[derive(Debug)]
pub enum NameError {
    NotFound(Ident),
    MemberNotFound {
        base: Type,
        member: Ident,
        span: Span,
    },
    ExpectedType(Ident, UnexpectedValue),
    ExpectedInterface(Ident, UnexpectedValue),
    ExpectedBinding(Ident, UnexpectedValue),
    ExpectedFunction(Ident, UnexpectedValue),
    ExpectedNamespace(Ident, UnexpectedValue),
    AlreadyDeclared { new: Ident, existing: Ident },
    AlreadyDefined { ident: Ident, existing: Span },
    Ambiguous { ident: Ident, options: Vec<Ident> },
}

impl Spanned for NameError {
    fn span(&self) -> &Span {
        match self {
            NameError::NotFound(ident) => &ident.span,
            NameError::MemberNotFound { span, .. } => span,
            NameError::ExpectedType(ident, _) => &ident.span,
            NameError::ExpectedInterface(ident, _) => &ident.span,
            NameError::ExpectedBinding(ident, _) => &ident.span,
            NameError::ExpectedFunction(ident, _) => &ident.span,
            NameError::ExpectedNamespace(ident, _) => &ident.span,
            NameError::AlreadyDeclared { new, .. } => &new.span,
            NameError::AlreadyDefined { ident, .. } => &ident.span,
            NameError::Ambiguous { ident, .. } => &ident.span,
        }
    }

    fn fmt_context(&self, mut f: impl fmt::Write, source: &str) -> fmt::Result {
        match self {
            NameError::AlreadyDeclared { new, existing } => {
                new.span.fmt_context(&mut f, source)?;
                writeln!(f, "Previously declared at:")?;
                existing.span.fmt_context(f, source)
            }

            NameError::AlreadyDefined { ident, existing } => {
                ident.span.fmt_context(&mut f, source)?;
                writeln!(f, "Previously defined at:")?;
                existing.fmt_context(f, source)
            }

            NameError::Ambiguous { ident, options } => {
                ident.span.fmt_context(&mut f, source)?;
                writeln!(f, "Could be one of the following:")?;
                for option in options {
                    option.span.fmt_context(&mut f, source)?;
                }
                Ok(())
            }

            _ => {
                writeln!(f, "{}", self)?;
                self.span().fmt_context(f, source)
            }
        }
    }
}

impl fmt::Display for NameError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            NameError::NotFound(ident) => {
                write!(f, "`{}` was not found in this scope", ident)
            }
            NameError::MemberNotFound { base, member, .. } => {
                write!(f, "type {} does not have a member named `{}`", base, member)
            }
            NameError::ExpectedType(ident, unexpected) => {
                write!(f, "`{}` did not refer to a type in this scope (found: {})", ident, unexpected)
            }
            NameError::ExpectedInterface(ident, unexpected) => {
                write!(f, "`{}` did not refer to an interface in this scope (found: {})", ident, unexpected)
            }
            NameError::ExpectedBinding(ident, unexpected) => {
                write!(f, "`{}` did not refer to a value in this scope (found: {})", ident, unexpected)
            }
            NameError::ExpectedFunction(ident, unexpected) => {
                write!(f, "`{}` did not refer to a function in this scope (found: {})", ident, unexpected)
            }
            NameError::ExpectedNamespace(ident, unexpected) => {
                write!(f, "`{}` did not refer to a namespace in this scope (found: {})", ident, unexpected)
            }
            NameError::AlreadyDeclared { new, .. } => {
                write!(f, "`{}` was already declared in this scope", new)
            }
            NameError::AlreadyDefined { ident, .. } => {
                write!(f, "`{}` was already defined", ident)
            }
            NameError::Ambiguous { ident, .. } => {
                write!(f, "`{}` is ambiguous in this context", ident)
            }
        }
    }
}

pub type NamingResult<T> = Result<T, NameError>;