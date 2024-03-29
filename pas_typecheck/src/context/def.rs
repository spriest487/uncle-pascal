use std::rc::Rc;
use pas_common::span::{Span, Spanned};
use pas_syn::Ident;
use crate::ast::{Class, FunctionDecl, FunctionDef, Interface, Variant};

#[derive(Clone, Debug)]
pub enum Def {
    External(FunctionDecl),
    Function(FunctionDef),
    Class(Rc<Class>),
    Interface(Rc<Interface>),
    Variant(Rc<Variant>),
}

impl Def {
    pub fn ident(&self) -> &Ident {
        match self {
            Def::External(func_decl) => func_decl.ident.last(),
            Def::Function(func_def) => func_def.decl.ident.last(),
            Def::Class(class) => &class.name.decl_name.ident,
            Def::Interface(iface) => &iface.name.decl_name.ident,
            Def::Variant(variant) => &variant.name.decl_name.ident,
        }
    }
}

impl Spanned for Def {
    fn span(&self) -> &Span {
        match self {
            Def::External(decl) => decl.span(),
            Def::Function(def) => def.span(),
            Def::Class(def) => def.span(),
            Def::Interface(def) => def.span(),
            Def::Variant(def) => def.span(),
        }
    }
}

// result of comparing a defined name with its previous decl
pub enum DefDeclMatch {
    // the def matches the decl
    Match,

    // the def is the right kind but doesn't match, e.g. function with wrong signature
    Mismatch,

    // the decl with the same name as this def is the wrong kind
    WrongKind,
}
