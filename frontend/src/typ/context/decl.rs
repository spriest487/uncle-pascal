use crate::ast::IdentPath;
use crate::ast::Visibility;
use crate::typ::ast::{FunctionDecl, Literal};
use crate::typ::{builtin_comparable_name, builtin_displayable_name, builtin_disposable_name, builtin_string_name, Binding, DeclConflict};
use crate::typ::Type;
use common::span::Span;
use std::fmt;
use std::rc::Rc;

#[derive(Clone, Debug, PartialEq)]
pub enum Decl {
    Type {
        ty: Type,
        visibility: Visibility,
        forward: bool,
    },
    BoundValue(Binding),
    Function {
        overloads: Vec<Rc<FunctionDecl>>,
        visibility: Visibility,
    },
    Alias(IdentPath),
    Const {
        ty: Type,
        val: Literal,
        visibility: Visibility,
        span: Span,
    },
    Namespace(IdentPath),
}


impl Decl {
    pub fn visibility(&self) -> Visibility {
        match self {
            | Decl::Type { visibility, .. }
            | Decl::Function { visibility, .. }
            | Decl::Const { visibility, .. } => *visibility,

            | Decl::Alias(_)
            | Decl::Namespace(_)
            | Decl::BoundValue(_) => Visibility::Implementation,
        }
    }

    /// If no conflict, it's OK to replace `self` with `new_decl` in the same scope.
    /// Handles forward declarations and source code definitions of builtin decls.
    pub fn get_conflict(&self, new_decl: &Decl) -> Option<DeclConflict> {
        if self.visibility() != new_decl.visibility() {
            return Some(DeclConflict::Visibility);
        }

        if new_decl.is_valid_builtin_redecl() {
            return None;
        }

        // forward type declarations can be replaced by any other declaration (forward or not) 
        // as long as the declared type matches exactly
        if let Decl::Type { ty: old_ty, forward: true, ..} = self {
            if let Decl::Type { ty: new_ty, .. } = new_decl {
                return if old_ty == new_ty {
                    None
                } else {
                    Some(DeclConflict::Type)
                }
            }
        }

        Some(DeclConflict::Name)
    }

    // builtin types are OK to redeclare in System.pas
    pub fn is_valid_builtin_redecl(&self) -> bool {
        match self {
            Decl::Type { ty: Type::Interface(iface), .. } => {
                **iface == builtin_displayable_name().full_path
                    || **iface == builtin_comparable_name().full_path
                    || **iface == builtin_disposable_name().full_path
            },

            Decl::Type { ty: Type::Class(sym), .. } => {
                **sym == builtin_string_name()
            },

            _ => false,
        }
    }
}

impl fmt::Display for Decl {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Decl::Type { ty, .. } => write!(f, "type `{}`", ty),
            Decl::Const { ty, val, .. } => write!(f, "const {}: {}", ty, val),
            Decl::BoundValue(binding) => write!(f, "{} of `{}`", binding.kind, binding.ty),
            Decl::Function { overloads, .. } => {
                write!(f, "function group of {} overloads", overloads.len())
            },
            Decl::Alias(aliased) => write!(f, "{}", aliased),
            Decl::Namespace(namespace) => write!(f, "{}", namespace)
        }
    }
}
