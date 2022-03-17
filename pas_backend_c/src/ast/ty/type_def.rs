use std::fmt;
use crate::ast::{FuncAliasDef, StructDef, TypeDecl, VariantDef};

#[derive(Clone, Eq, PartialEq, Hash)]
pub enum TypeDef {
    Struct(StructDef),
    Variant(VariantDef),
    FuncAlias(FuncAliasDef),
}

impl TypeDef {
    pub fn decl(&self) -> &TypeDecl {
        match self {
            TypeDef::Struct(s) => &s.decl,
            TypeDef::Variant(v) => &v.decl,
            TypeDef::FuncAlias(f) => &f.decl,
        }
    }

    pub fn forward_decl(&self) -> Option<&TypeDecl> {
        match self {
            TypeDef::Struct(s) => Some(&s.decl),
            TypeDef::Variant(v) => Some(&v.decl),
            TypeDef::FuncAlias(..) => None,
        }
    }
}

impl fmt::Display for TypeDef {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TypeDef::Struct(s) => write!(f, "{}", s),
            TypeDef::Variant(v) => write!(f, "{}", v),
            TypeDef::FuncAlias(func) => write!(f, "{}", func),
        }
    }
}
