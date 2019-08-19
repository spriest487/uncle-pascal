use crate::{context::*, Type};
use pas_common::{span::*, DiagnosticLabel, DiagnosticMessage, DiagnosticOutput};
use pas_syn::{Ident, IdentPath};
use std::{fmt, path::PathBuf};

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
    DefDeclMismatch {
        ident: IdentPath,
        decl: Span,
        def: Span,
    },
    ExpectedType(IdentPath, UnexpectedValue),
    ExpectedInterface(IdentPath, UnexpectedValue),
    ExpectedBinding(IdentPath, UnexpectedValue),
    ExpectedFunction(IdentPath, UnexpectedValue),
    ExpectedNamespace(IdentPath, UnexpectedValue),
    AlreadyDeclared {
        new: Ident,
        existing: IdentPath,
    },
    AlreadyDefined {
        ident: IdentPath,
        existing: Span,
    },
    AlreadyImplemented {
        iface: Box<Interface>,
        for_ty: Type,
        method: Ident,
        existing: Span,
    },
    Ambiguous {
        ident: Ident,
        options: Vec<IdentPath>,
    },
}

impl Spanned for NameError {
    fn span(&self) -> &Span {
        match self {
            NameError::NotFound(ident) => &ident.span,
            NameError::MemberNotFound { span, .. } => span,
            NameError::ExpectedType(ident, _) => ident.span(),
            NameError::ExpectedInterface(ident, _) => ident.span(),
            NameError::ExpectedBinding(ident, _) => ident.span(),
            NameError::ExpectedFunction(ident, _) => ident.span(),
            NameError::ExpectedNamespace(ident, _) => ident.span(),
            NameError::AlreadyDeclared { new, .. } => &new.span,
            NameError::AlreadyDefined { ident, .. } => &ident.span(),
            NameError::Ambiguous { ident, .. } => &ident.span,
            NameError::AlreadyImplemented { method, .. } => method.span(),
            NameError::DefDeclMismatch { ident, .. } => ident.span(),
        }
    }
}

impl DiagnosticOutput for NameError {
    fn title(&self) -> String {
        match self {
            NameError::NotFound(_) => "Name not found",
            NameError::MemberNotFound { .. } => "Named member not found",
            NameError::ExpectedType(_, _) => "Expected name to refer to type",
            NameError::ExpectedInterface(_, _) => "Expected name to refer to interface",
            NameError::ExpectedBinding(_, _) => "Expected name to refer to binding",
            NameError::ExpectedFunction(_, _) => "Expected name to refer to function",
            NameError::ExpectedNamespace(_, _) => "Expected name to refer to namespace",
            NameError::AlreadyDeclared { .. } => "Name already declared",
            NameError::AlreadyDefined { .. } => "Name already defined",
            NameError::Ambiguous { .. } => "Name is ambiguous",
            NameError::AlreadyImplemented { .. } => "Method already implemented",
            NameError::DefDeclMismatch { .. } => "Definition does not match previous declaration",
        }
        .to_string()
    }

    fn label(&self) -> Option<DiagnosticLabel> {
        Some(DiagnosticLabel {
            text: Some(self.to_string()),
            span: self.span().clone(),
        })
    }

    fn see_also(&self) -> Vec<DiagnosticMessage> {
        match self {
            NameError::AlreadyDeclared { new, existing } => {
                if *existing.span().file.as_ref() == PathBuf::from("<builtin>") {
                    // don't show this message for conflicts with builtin identifiers
                    Vec::new()
                } else {
                    vec![DiagnosticMessage {
                        title: format!("`{}` previously declared here", new),
                        label: Some(DiagnosticLabel {
                            text: None,
                            span: existing.span().clone(),
                        }),
                    }]
                }
            }

            NameError::AlreadyDefined { ident, existing } => vec![DiagnosticMessage {
                title: format!("`{}` previously defined here", ident),
                label: Some(DiagnosticLabel {
                    text: None,
                    span: existing.span().clone(),
                }),
            }],

            NameError::AlreadyImplemented {
                iface,
                method,
                existing,
                ..
            } => vec![DiagnosticMessage {
                title: format!("`{}.{}` previously implemented here", iface, method),
                label: Some(DiagnosticLabel {
                    text: None,
                    span: existing.clone(),
                }),
            }],

            NameError::Ambiguous { ident, options } => {
                let mut see_also: Vec<_> = options
                    .iter()
                    .map(|option| DiagnosticMessage {
                        title: format!("`{}` could refer to `{}`", ident, option.join(".")),
                        label: Some(DiagnosticLabel {
                            text: None,
                            span: option.last().span().clone(),
                        }),
                    })
                    .collect();
                see_also.sort();
                see_also
            }

            NameError::DefDeclMismatch { def, decl, ident } => {
                vec![
                    DiagnosticMessage {
                        title: format!("Previous declaration of `{}`", ident),
                        label: Some(DiagnosticLabel {
                            text: None,
                            span: decl.clone(),
                        }),
                    },

                    DiagnosticMessage {
                        title: format!("Conflicting definition of `{}`", ident),
                        label: Some(DiagnosticLabel {
                            text: None,
                            span: def.clone(),
                        }),
                    },
                ]
            }

            _ => Vec::new(),
        }
    }
}

impl fmt::Display for NameError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            NameError::NotFound(ident) => write!(f, "`{}` was not found in this scope", ident),
            NameError::MemberNotFound { base, member, .. } => {
                write!(f, "type {} does not have a member named `{}`", base, member)
            }
            NameError::ExpectedType(ident, unexpected) => write!(
                f,
                "`{}` did not refer to a type in this scope (found: {})",
                ident, unexpected
            ),
            NameError::ExpectedInterface(ident, unexpected) => write!(
                f,
                "`{}` did not refer to an interface in this scope (found: {})",
                ident, unexpected
            ),
            NameError::ExpectedBinding(ident, unexpected) => write!(
                f,
                "`{}` did not refer to a value in this scope (found: {})",
                ident, unexpected
            ),
            NameError::ExpectedFunction(ident, unexpected) => write!(
                f,
                "`{}` did not refer to a function in this scope (found: {})",
                ident, unexpected
            ),
            NameError::ExpectedNamespace(ident, unexpected) => write!(
                f,
                "`{}` did not refer to a namespace in this scope (found: {})",
                ident, unexpected
            ),
            NameError::AlreadyDeclared { new, .. } => {
                write!(f, "`{}` was already declared in this scope", new)
            }
            NameError::AlreadyDefined { ident, .. } => write!(f, "`{}` was already defined", ident),
            NameError::Ambiguous { ident, .. } => {
                write!(f, "`{}` is ambiguous in this context", ident)
            }

            NameError::AlreadyImplemented {
                iface,
                method,
                for_ty,
                ..
            } => write!(
                f,
                "`{}.{}` already implemented for `{}`",
                iface.name, method, for_ty
            ),

            NameError::DefDeclMismatch { ident, .. } => {
                write!(f, "definition of `{}` does not match previous declaration", ident)
            }
        }
    }
}

pub type NamingResult<T> = Result<T, NameError>;
