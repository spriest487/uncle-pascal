use crate::{context::Decl, Type, FunctionSig, TypeAnnotation, ScopeMemberKind};
use pas_common::{span::*};
use pas_syn::{Ident, IdentPath};
use std::{fmt};
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
    },
    ArgConstraintNotSatisfied {
        is_not_ty: Type,
        actual_ty: Option<Type>, // may be unknown/not yet resolved when processing generics
    },
    CannotInferArgs {
        target: GenericTarget,
        hint: GenericTypeHint,
    },
    IllegalUnspecialized {
        ty: Type,
    }
}

pub type GenericResult<T> = Result<T, GenericError>;

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
                actual_ty: Some(arg_ty),
                ..
            } => write!(
                f,
                "argument type {} does not meet the type constraint on this parameter: must be {}",
                arg_ty,
                is_not_ty,
            ),

            GenericError::ArgConstraintNotSatisfied {
                is_not_ty,
                actual_ty: None,
                ..
            } => write!(
                f,
                "argument does not meet the type constraint on this parameter: must be {}",
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
            _ => NameContainer::Type(a.ty().into_owned()),
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
    NotFound {
        ident: IdentPath,
    },
    MemberNotFound {
        base: NameContainer,
        member: Ident,
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
        existing_kind: ScopeMemberKind,
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

impl fmt::Display for NameError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            NameError::NotFound { ident, .. } => write!(f, "`{}` was not found in this scope", ident),
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

pub type NameResult<T> = Result<T, NameError>;
