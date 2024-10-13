use crate::ast::Ident;
use crate::typ::ast::EnumDecl;
use crate::typ::ast::FunctionDecl;
use crate::typ::ast::FunctionDef;
use crate::typ::ast::InterfaceDecl;
use crate::typ::ast::StructDef;
use crate::typ::ast::VariantDef;
use crate::typ::Decl;
use crate::typ::FunctionSig;
use common::span::Span;
use common::span::Spanned;
use std::rc::Rc;

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Def {
    External(Rc<FunctionDecl>),
    Function(Rc<FunctionDef>),
    Class(Rc<StructDef>),
    Interface(Rc<InterfaceDecl>),
    Variant(Rc<VariantDef>),
    Enum(Rc<EnumDecl>),
}

impl Def {
    pub fn ident(&self) -> &Ident {
        match self {
            Def::External(func_decl) => &func_decl.name.ident,
            Def::Function(func_def) => &func_def.decl.name.ident,
            Def::Class(class) => &class.name.ident(),
            Def::Interface(iface) => &iface.name.ident(),
            Def::Variant(variant) => &variant.name.ident(),
            Def::Enum(enum_decl) => &enum_decl.name.ident(),
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

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub enum DefKey {
    Unique,
    Sig(Rc<FunctionSig>),
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

impl DefDeclMatch {
    pub fn always_match(_: &Decl) -> DefDeclMatch {
        DefDeclMatch::Match
    }
}

// 
// #[derive(Debug, Clone, Hash, Eq, PartialEq)]
// pub struct FunctionDefKey {
//     name: IdentPath,
//     sig: Rc<FunctionSig>,
// }
