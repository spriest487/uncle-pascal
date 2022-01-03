use std::rc::Rc;
use std::fmt;
use crate::{ast::Literal, Binding, FunctionSig, Type};
use pas_common::span::Span;
use pas_syn::{ast::Visibility, IdentPath};

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
}

impl fmt::Display for Decl {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Decl::Type { ty, .. } => write!(f, "type `{}`", ty),
            Decl::Const { ty, val, .. } => write!(f, "const {}: {}", ty, val),
            Decl::BoundValue(binding) => write!(f, "{} of `{}`", binding.kind, binding.ty),
            Decl::Function { sig, .. } => write!(f, "{}", sig),
            Decl::Alias(aliased) => write!(f, "{}", aliased),
        }
    }
}
