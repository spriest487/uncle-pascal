mod r#struct;
mod variant;
mod interface;

use crate::FunctionSig;
use crate::NamePath;
use crate::Type;
use crate::TypeDefID;
use common::span::Location;
use common::span::Span;
pub use interface::*;
pub use r#struct::*;
use std::borrow::Cow;
use std::fmt;
use std::fmt::Write;
use serde::{Deserialize, Serialize};
pub use variant::*;

#[derive(Clone, Debug, Eq, PartialEq, Hash, Serialize, Deserialize)]
pub enum StructIdentity {
    Record(NamePath),
    Class(NamePath),
    Array(Type, usize),
    DynArray(Type),
    Closure(ClosureIdentity),
    
    // bitmask type for set flag collections. value is the number of bits
    SetFlags { bits: usize }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash, Serialize, Deserialize)]
pub struct ClosureIdentity {
    /// the type of the closure's virtual call function alias, which has the sig of the closure's
    /// target type plus a type-erased pointer inserted as parameter 0
    pub virt_func_ty: TypeDefID,

    pub module: String,
    pub line: usize,
    pub col: usize,
}

impl ClosureIdentity {
    pub fn src_span(&self) -> Span {
        let location = Location::new(self.line, self.col);
        Span::new(self.module.clone(), location, location)
    }
}

impl StructIdentity {
    pub fn is_ref_type(&self) -> bool {
        match self {
            StructIdentity::Array(..) 
            | StructIdentity::Record(..) 
            | StructIdentity::SetFlags { .. } => false,

            StructIdentity::Class(..)
            | StructIdentity::DynArray(..)
            | StructIdentity::Closure(..) => true,
        }
    }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum TypeDecl {
    Reserved,
    Forward(NamePath),
    Def(TypeDef),
}

impl TypeDecl {
    pub fn name(&self) -> Option<&NamePath> {
        match self {
            TypeDecl::Reserved => None,
            TypeDecl::Forward(name) => Some(name),
            TypeDecl::Def(def) => def.name(),
        }
    }

    pub fn is_forward(&self) -> bool {
        !matches!(self, TypeDecl::Def(..))
    }
}

impl fmt::Display for TypeDecl {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TypeDecl::Reserved => write!(f, "reserved type ID"),
            TypeDecl::Forward(name) => write!(f, "forward decl of `{}`", name),
            TypeDecl::Def(def) => write!(f, "defined type `{}`", def),
        }
    }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum TypeDef {
    Struct(Struct),
    Variant(VariantDef),
    Function(FunctionSig),
}

impl TypeDef {
    pub fn name(&self) -> Option<&NamePath> {
        match self {
            TypeDef::Struct(s) => s.name(),
            TypeDef::Variant(v) => Some(&v.name),
            TypeDef::Function(..) => None,
        }
    }

    pub fn src_span(&self) -> Option<&Span> {
        match self {
            TypeDef::Struct(def) => def.src_span.as_ref(),
            TypeDef::Variant(def) => def.src_span.as_ref(),
            TypeDef::Function(..) => None,
        }
    }
    
    pub fn to_pretty_string<'a, TyFormat>(&self, ty_format: TyFormat) -> String
    where
        TyFormat: Fn(&Type) -> Cow<'a, str>
    {
        match self {
            TypeDef::Struct(def) => match &def.identity {
                StructIdentity::Class(name) | StructIdentity::Record(name) => {
                    name.to_pretty_string(ty_format)
                },
                StructIdentity::Closure(identity) => {
                    let func_ty_name = ty_format(&Type::Function(identity.virt_func_ty));
                    format!("closure of {} @ {}:{}:{}", func_ty_name, identity.module, identity.line, identity.col)
                },
                StructIdentity::Array(ty, dim) => {
                    let ty_name = ty_format(ty);
                    format!("array[{}] of {}", dim, ty_name)
                }
                StructIdentity::DynArray(ty) => {
                    let ty_name = ty_format(ty);
                    format!("array of {}", ty_name)
                }
                StructIdentity::SetFlags { bits } => {
                    format!("set<{bits}>")
                }
            },
            TypeDef::Variant(def) => def.name.to_pretty_string(ty_format),
            TypeDef::Function(def) => {
                let mut string = String::new();
                let f = &mut string;
                write!(f, "function (").unwrap();

                for (i, param_ty) in def.param_tys.iter().enumerate() {
                    if i > 0 {
                        write!(f, "; ").unwrap();
                    }
                    write!(f, "{}", ty_format(param_ty).as_ref()).unwrap();
                }

                write!(f, "): {}", ty_format(&def.return_ty).as_ref()).unwrap();

                string
            }
        }
    }
}

impl fmt::Display for TypeDef {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TypeDef::Struct(s) => write!(f, "{}", s),
            TypeDef::Variant(v) => write!(f, "{}", v.name),
            TypeDef::Function(func_ty) => {
                write!(f, "function (")?;

                for (i, param_ty) in func_ty.param_tys.iter().enumerate() {
                    if i > 0 {
                        write!(f, "; ")?;
                    }
                    write!(f, "{}", param_ty)?;
                }

                write!(f, "): {}", func_ty.return_ty)
            },
        }
    }
}
