use crate::ast::IdentPath;
use crate::ast::Visibility;
use crate::typ::ast::Literal;
use crate::typ::Binding;
use crate::typ::FunctionSig;
use crate::typ::Type;
use common::span::Span;
use std::fmt;
use std::rc::Rc;

#[derive(Clone, Debug, PartialEq)]
pub enum Decl {
    Type {
        ty: Type,
        visibility: Visibility,
    },
    BoundValue(Binding),
    Function {
        sig: Rc<FunctionSig>,
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
}

impl fmt::Display for Decl {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Decl::Type { ty, .. } => write!(f, "type `{}`", ty),
            Decl::Const { ty, val, .. } => write!(f, "const {}: {}", ty, val),
            Decl::BoundValue(binding) => write!(f, "{} of `{}`", binding.kind, binding.ty),
            Decl::Function { sig, .. } => write!(f, "{}", sig),
            Decl::Alias(aliased) => write!(f, "{}", aliased),
            Decl::Namespace(namespace) => write!(f, "{}", namespace)
        }
    }
}
