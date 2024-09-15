use crate::ast::Ident;
use crate::typecheck::ast::EnumDecl;
use crate::typecheck::ast::FunctionDecl;
use crate::typecheck::ast::FunctionDef;
use crate::typecheck::ast::InterfaceDecl;
use crate::typecheck::ast::StructDef;
use crate::typecheck::ast::VariantDef;
use pas_common::span::Span;
use pas_common::span::Spanned;
use std::rc::Rc;

#[derive(Clone, Debug)]
pub enum Def {
    External(FunctionDecl),
    Function(FunctionDef),
    Class(Rc<StructDef>),
    Interface(Rc<InterfaceDecl>),
    Variant(Rc<VariantDef>),
    Enum(Rc<EnumDecl>),
}

impl Def {
    pub fn ident(&self) -> &Ident {
        match self {
            Def::External(func_decl) => func_decl.ident.last(),
            Def::Function(func_def) => func_def.decl.ident.last(),
            Def::Class(class) => &class.name.decl_name.ident,
            Def::Interface(iface) => &iface.name.decl_name.ident,
            Def::Variant(variant) => &variant.name.decl_name.ident,
            Def::Enum(enum_decl) => &enum_decl.name.decl_name.ident,
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
            Def::Enum(decl) => decl.span(),
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
