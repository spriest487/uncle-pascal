use crate::{ast::Interface, context::Decl, Type, FunctionSig};
use pas_common::{span::*, DiagnosticLabel, DiagnosticMessage, DiagnosticOutput};
use pas_syn::{Ident, IdentPath};
use std::{fmt, path::PathBuf};

#[derive(Debug)]
pub enum GenericTarget {
    Name(IdentPath),
    FunctionSig(FunctionSig)
}

impl fmt::Display for GenericTarget {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            GenericTarget::Name(name) => write!(f, "name `{}`", name),
            GenericTarget::FunctionSig(sig) => write!(f, "function signature `{}`", sig),
        }
    }
}

#[derive(Debug)]
pub enum GenericError {
    ArgsLenMismatch {
        target: GenericTarget,
        expected: usize,
        actual: usize,
        span: Span,
    },
    CannotInferArgs {
        target: GenericTarget,
        expected: Type,
        span: Span,
    }
}

pub type GenericResult<T> = Result<T, GenericError>;

impl Spanned for GenericError {
    fn span(&self) -> &Span {
        match self {
            GenericError::ArgsLenMismatch { span, .. } => span,
            GenericError::CannotInferArgs { span, .. } => span,
        }
    }
}

impl DiagnosticOutput for GenericError {
    fn title(&self) -> String {
        match self {
            GenericError::ArgsLenMismatch { .. } => "Wrong number of type arguments".to_string(),
            GenericError::CannotInferArgs { .. } => "Cannot infer type arguments".to_string(),
        }
    }

    fn label(&self) -> Option<DiagnosticLabel> {
        Some(DiagnosticLabel {
            text: Some(self.to_string()),
            span: self.span().clone(),
        })
    }
}

impl fmt::Display for GenericError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            GenericError::ArgsLenMismatch {
                target,
                expected,
                actual,
                ..
            } => write!(
                f,
                "`{}` expects {} type arguments, found {}",
                target, expected, actual
            ),

            GenericError::CannotInferArgs {
                target,
                expected,
                ..
            } => write!(
                f,
                "cannot infer type arguments for {} from expected type `{}`",
                target,
                expected,
            )
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub enum ExpectedKind {
    AnyType,
    Class,
    Variant,
    Interface,
    AnyBinding,
    Function,
    Namespace,
}

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
    Unexpected {
        ident: IdentPath,
        expected: ExpectedKind,
        actual: UnexpectedValue,
    },
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
    GenericError(GenericError),
}

impl From<GenericError> for NameError {
    fn from(err: GenericError) -> Self {
        NameError::GenericError(err)
    }
}

impl Spanned for NameError {
    fn span(&self) -> &Span {
        match self {
            NameError::NotFound(ident) => &ident.span,
            NameError::MemberNotFound { span, .. } => span,
            NameError::Unexpected { ident, .. } => ident.span(),
            NameError::AlreadyDeclared { new, .. } => &new.span,
            NameError::AlreadyDefined { ident, .. } => &ident.span(),
            NameError::Ambiguous { ident, .. } => &ident.span,
            NameError::AlreadyImplemented { method, .. } => method.span(),
            NameError::DefDeclMismatch { ident, .. } => ident.span(),
            NameError::GenericError(err) => err.span(),
        }
    }
}

impl DiagnosticOutput for NameError {
    fn title(&self) -> String {
        match self {
            NameError::NotFound(_) => "Name not found".to_string(),
            NameError::MemberNotFound { .. } => "Named member not found".to_string(),
            NameError::Unexpected { .. } => "Name had unexpected type".to_string(),
            NameError::AlreadyDeclared { .. } => "Name already declared".to_string(),
            NameError::AlreadyDefined { .. } => "Name already defined".to_string(),
            NameError::Ambiguous { .. } => "Name is ambiguous".to_string(),
            NameError::AlreadyImplemented { .. } => "Method already implemented".to_string(),
            NameError::DefDeclMismatch { .. } => {
                "Definition does not match previous declaration".to_string()
            }
            NameError::GenericError(err) => err.title(),
        }
    }

    fn label(&self) -> Option<DiagnosticLabel> {
        match self {
            NameError::GenericError(err) => err.label(),

            _ => Some(DiagnosticLabel {
                text: Some(self.to_string()),
                span: self.span().clone(),
            }),
        }
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

            NameError::DefDeclMismatch { def, decl, ident } => vec![
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
            ],

            NameError::GenericError(err) => err.see_also(),

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
            NameError::Unexpected {
                ident,
                expected,
                actual,
            } => {
                let expected_desc = match expected {
                    ExpectedKind::Class => "class",
                    ExpectedKind::Variant => "variant",
                    ExpectedKind::Interface => "interface",
                    ExpectedKind::Function => "function",
                    ExpectedKind::AnyBinding => "value",
                    ExpectedKind::Namespace => "namespace",
                    ExpectedKind::AnyType => "type",
                };

                write!(
                    f,
                    "`{}` did not refer to a {} in this scope (found: {})",
                    ident, expected_desc, actual,
                )
            }
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

            NameError::DefDeclMismatch { ident, .. } => write!(
                f,
                "definition of `{}` does not match previous declaration",
                ident
            ),

            NameError::GenericError(err) => write!(f, "{}", err),
        }
    }
}

pub type NamingResult<T> = Result<T, NameError>;
