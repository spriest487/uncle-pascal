use crate::{context::Decl, Type, FunctionSig, TypeAnnotation, NameKind};
use pas_common::{span::*, DiagnosticLabel, DiagnosticMessage, DiagnosticOutput};
use pas_syn::{Ident, IdentPath};
use std::{fmt, path::PathBuf};
use std::fmt::Debug;

#[derive(Debug)]
pub enum GenericTarget {
    Name(IdentPath),
    FunctionSig(FunctionSig)
}

impl fmt::Display for GenericTarget {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            GenericTarget::Name(name) => write!(f, "name `{}`", name),
            GenericTarget::FunctionSig(sig) => write!(f, "`{}`", sig),
        }
    }
}

#[derive(Debug)]
pub enum GenericTypeHint {
    Unknown,
    ExpectedValueType(Type),
    ArgTypes(Vec<Type>),
}

impl fmt::Display for GenericTypeHint {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            GenericTypeHint::Unknown => write!(f, "unknown"),

            GenericTypeHint::ExpectedValueType(ty) => {
                write!(f, "expected type {}", ty)
            }

            GenericTypeHint::ArgTypes(tys) => {
                write!(f, "argument type")?;
                if tys.len() > 1 {
                    write!(f, "s")?;
                }

                if !tys.is_empty() {
                    write!(f, " ")?;
                    for (i, ty) in tys.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        write!(f, "`{}`", ty)?;
                    }
                }

                Ok(())
            }
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
    ArgConstraintNotSatisfied {
        arg_ty: Type,
        is_not_ty: Type,
        span: Span,
    },
    CannotInferArgs {
        target: GenericTarget,
        hint: GenericTypeHint,
        span: Span,
    },
    IllegalUnspecialized {
        ty: Type,
        span: Span,
    }
}

pub type GenericResult<T> = Result<T, GenericError>;

impl Spanned for GenericError {
    fn span(&self) -> &Span {
        match self {
            GenericError::ArgsLenMismatch { span, .. } => span,
            GenericError::ArgConstraintNotSatisfied { span, .. } => span,
            GenericError::CannotInferArgs { span, .. } => span,
            GenericError::IllegalUnspecialized { span, .. } => span,
        }
    }
}

impl DiagnosticOutput for GenericError {
    fn title(&self) -> String {
        match self {
            GenericError::ArgsLenMismatch { .. } => "Wrong number of type arguments".to_string(),
            GenericError::ArgConstraintNotSatisfied { .. } => "Type paramter constraint not satisfied by argument".to_string(),
            GenericError::CannotInferArgs { .. } => "Cannot infer type arguments".to_string(),
            GenericError::IllegalUnspecialized { .. } => "Illegal use of unspecialized type".to_string(),
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
                "{} expects {} type argument(s), found {}",
                target, expected, actual
            ),

            GenericError::ArgConstraintNotSatisfied {
                is_not_ty,
                arg_ty,
                ..
            } => write!(
                f,
                "argument type {} does not meet the type constraint on this parameter: must be {}",
                arg_ty,
                is_not_ty,
            ),

            GenericError::CannotInferArgs {
                target,
                hint,
                ..
            } => {
                write!(f, "cannot infer type arguments for {}", target)?;

                match hint {
                    GenericTypeHint::Unknown => Ok(()),
                    hint => write!(f, " from {}", hint)
                }
            },

            GenericError::IllegalUnspecialized {
                ty, ..
            } => write!(
                f,
                "the type `{}` cannot be used without type arguments in this context",
                ty,
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
pub enum Named {
    Decl(Decl),
    Namespace(IdentPath),
}

impl From<Decl> for Named {
    fn from(decl: Decl) -> Self {
        Named::Decl(decl)
    }
}

impl fmt::Display for Named {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Named::Decl(decl) => write!(f, "{}", decl),
            Named::Namespace(ident_path) => write!(f, "namespace `{}`", ident_path),
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum NameContainer {
    Namespace(IdentPath),
    Type(Type),
}

impl NameContainer {
    pub fn for_annotated(a: &TypeAnnotation) -> Self {
        match a {
            TypeAnnotation::Namespace(ns, ..) => NameContainer::Namespace(ns.clone()),
            _ => NameContainer::Type(a.ty().clone()),
        }
    }
}

impl fmt::Display for NameContainer {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            NameContainer::Namespace(ns) => write!(f, "namespace `{}`", ns),
            NameContainer::Type(ty) => write!(f, "type `{}`", ty),
        }
    }
}

#[derive(Debug)]
pub enum NameError {
    NotFound(Ident),
    MemberNotFound {
        base: NameContainer,
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
        actual: Named,
    },
    AlreadyDeclared {
        new: Ident,
        existing: IdentPath,
        existing_kind: NameKind,
    },
    AlreadyDefined {
        ident: IdentPath,
        existing: Span,
    },
    AlreadyImplemented {
        iface: IdentPath,
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
            NameError::AlreadyDeclared { new, existing, existing_kind } => {
                if *existing.span().file.as_ref() == PathBuf::from("<builtin>") {
                    // don't show this message for conflicts with builtin identifiers
                    Vec::new()
                } else {
                    vec![DiagnosticMessage {
                        title: format!("{} `{}` previously declared here", existing_kind, new),
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
                write!(f, "{} does not have a member named `{}`", base, member)
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
                iface, method, for_ty
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
