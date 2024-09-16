mod function;
mod iface_def;
mod struct_def;
mod variant_def;

pub use self::function::*;
pub use self::iface_def::*;
pub use self::struct_def::*;
pub use self::variant_def::*;
use crate::metadata::NamePathExt;
use ir_lang::*;
use std::borrow::Cow;
use std::fmt::Write;

pub trait TypeDefExt {
    fn to_pretty_string<'a, TyFormat>(&self, ty_format: TyFormat) -> String
    where
        TyFormat: Fn(&Type) -> Cow<'a, str>;
}

impl TypeDefExt for TypeDef {
    fn to_pretty_string<'a, TyFormat>(&self, ty_format: TyFormat) -> String
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
                    let ty_name = ty_format(&ty);
                    format!("array[{}] of {}", dim, ty_name)
                }
                StructIdentity::DynArray(ty) => {
                    let ty_name = ty_format(&ty);
                    format!("array of {}", ty_name)
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
