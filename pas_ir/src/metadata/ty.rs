mod function;
mod iface_def;
mod struct_def;
mod variant_def;

pub use self::{function::*, iface_def::*, struct_def::*, variant_def::*};
use crate::{
    name_path::NamePath, InterfaceID, StructID,
    STRING_ID,
};
use pas_common::span::Span;
use std::{
    fmt,
    fmt::Write,
    rc::Rc,
    borrow::Cow
};

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum ClassID {
    Class(StructID),
    Interface(InterfaceID),
}

impl ClassID {
    pub fn as_class(&self) -> Option<StructID> {
        match self {
            ClassID::Class(id) => Some(*id),
            ClassID::Interface(..) => None,
        }
    }
}

impl fmt::Display for ClassID {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ClassID::Class(struct_id) => write!(f, "{}", struct_id),
            ClassID::Interface(iface_id) => write!(f, "{}", iface_id),
        }
    }
}

#[derive(Clone, Debug)]
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
        match self {
            TypeDecl::Def(..) => false,
            _ => true,
        }
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

#[derive(Clone, Debug)]
pub enum TypeDef {
    Struct(Struct),
    Variant(Variant),
    Function(FunctionSig),
}

impl TypeDef {
    pub fn name(&self) -> Option<&NamePath> {
        match self {
            TypeDef::Struct(s) => Some(&s.name),
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

    pub fn to_pretty_str<'a, TyFormat>(&self, ty_format: TyFormat) -> String
    where
        TyFormat: Fn(&Type) -> Cow<'a, str>
    {
        match self {
            TypeDef::Struct(def) => def.name.to_pretty_string(ty_format),
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
            TypeDef::Struct(s) => write!(f, "{}", s.name),
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
            }
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum Type {
    /// no type or unknown type (used for raw pointers like void*)
    Nothing,

    Pointer(Rc<Type>),
    Struct(StructID),
    Variant(StructID),
    Array {
        element: Rc<Type>,
        dim: usize,
    },

    /// pointer to an RC object somewhere on the heap, which can be dereferenced to yield a value
    /// of the inner type. the resource type is Some in the case that the type is known, and
    /// None for the Any type
    RcPointer(Option<ClassID>),

    /// RC shared object struct of known or unknown type
    RcObject(Option<StructID>),

    Function(StructID),

    Bool,
    U8,
    I8,
    I16,
    U16,
    I32,
    U32,
    I64,
    U64,
    USize,
    ISize,
    F32,
}

impl Type {
    pub fn ptr(self) -> Self {
        Type::Pointer(Rc::new(self))
    }

    pub const fn rc_ptr_to(class: ClassID) -> Self {
        Type::RcPointer(Some(class))
    }

    pub const fn rc_ptr_any() -> Self {
        Type::RcPointer(None)
    }

    pub const fn string_ptr() -> Self {
        Type::rc_ptr_to(ClassID::Class(STRING_ID))
    }

    pub fn deref_ty(&self) -> Option<&Self> {
        match self {
            Type::Pointer(target) => Some(&target),
            _ => None,
        }
    }

    pub fn array(self, dim: usize) -> Self {
        Type::Array {
            dim,
            element: Rc::new(self),
        }
    }

    pub fn as_struct(&self) -> Option<StructID> {
        match self {
            Type::Struct(struct_id) => Some(*struct_id),
            _ => None,
        }
    }

    pub fn is_struct(&self, id: StructID) -> bool {
        match self {
            Type::Struct(ty_id) => *ty_id == id,
            _ => false,
        }
    }

    pub fn as_iface(&self) -> Option<InterfaceID> {
        match self {
            Type::RcPointer(Some(ClassID::Interface(id))) => Some(*id),
            _ => None,
        }
    }

    pub fn is_rc(&self) -> bool {
        match self {
            Type::RcPointer(..) => true,
            _ => false,
        }
    }

    pub fn is_complex(&self) -> bool {
        match self {
            Type::Variant(..) | Type::Array { .. } | Type::Struct(..) => true,
            _ => false,
        }
    }

    pub fn rc_resource_class_id(&self) -> Option<ClassID> {
        match self {
            Type::RcPointer(Some(class_id)) => Some(*class_id),
            _ => None,
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Type::Nothing => write!(f, "none"),
            Type::F32 => write!(f, "f32"),
            Type::Bool => write!(f, "bool"),
            Type::U8 => write!(f, "u8"),
            Type::I8 => write!(f, "i8"),
            Type::I16 => write!(f, "i16"),
            Type::U16 => write!(f, "u16"),
            Type::I32 => write!(f, "i32"),
            Type::U32 => write!(f, "u32"),
            Type::I64 => write!(f, "i64"),
            Type::U64 => write!(f, "u64"),
            Type::ISize => write!(f, "isize"),
            Type::USize => write!(f, "usize"),
            Type::Pointer(target) => write!(f, "^{}", target),
            Type::Struct(id) => write!(f, "{{struct {}}}", id),
            Type::Variant(id) => write!(f, "{{variant {}}}", id),
            Type::RcPointer(id) => match id {
                None => write!(f, "any"),
                Some(ClassID::Class(id)) => write!(f, "class {}", id),
                Some(ClassID::Interface(id)) => write!(f, "iface {}", id),
            },
            Type::RcObject(id) => match id {
                Some(id) => write!(f, "{{rc {}}}", id),
                None => write!(f, "{{rc}}"),
            },
            Type::Array { element, dim } => write!(f, "{}[{}]", element, dim),
            Type::Function(id) => write!(f, "function {}", id),
        }
    }
}
