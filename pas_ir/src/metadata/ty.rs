use std::collections::HashMap;
use std::fmt;
use pas_common::span::Span;
use crate::name_path::NamePath;
use crate::{FunctionID, InterfaceID, MethodID, STRING_ID, StructID};

#[derive(Clone, Debug)]
pub struct Method {
    pub name: String,
    pub return_ty: Type,
    pub params: Vec<Type>,
}

#[derive(Clone, Debug)]
pub struct Interface {
    pub name: NamePath,
    pub methods: Vec<Method>,
    pub impls: HashMap<Type, InterfaceImpl>,
}

impl Interface {
    pub fn new(name: impl Into<NamePath>, methods: impl Into<Vec<Method>>) -> Self {
        Self {
            name: name.into(),
            methods: methods.into(),
            impls: HashMap::new(),
        }
    }

    pub fn add_impl(&mut self, implementor: Type, method: MethodID, func_id: FunctionID) {
        assert!(method.0 < self.methods.len());

        let methods_len = self.methods.len();
        let impl_entry = self
            .impls
            .entry(implementor.clone())
            .or_insert_with(|| InterfaceImpl::new(methods_len));
        assert!(
            !impl_entry.methods.contains_key(&method),
            "adding duplicate impl (func {}) of method {}.{} for {}, already defined as {}",
            func_id,
            self.name,
            method.0,
            implementor,
            impl_entry.methods[&method],
        );

        impl_entry.methods.insert(method, func_id);
    }

    pub fn method_index(&self, name: &str) -> Option<MethodID> {
        self.methods
            .iter()
            .position(|m| m.name.as_str() == name)
            .map(MethodID)
    }

    pub fn get_method(&self, id: MethodID) -> Option<&Method> {
        self.methods.get(id.0)
    }
}

#[derive(Clone, Debug)]
pub struct InterfaceImpl {
    // method index -> method impl
    pub methods: HashMap<MethodID, FunctionID>,
}

impl InterfaceImpl {
    fn new(method_count: usize) -> Self {
        Self {
            methods: HashMap::with_capacity(method_count),
        }
    }
}

#[derive(Clone, Debug)]
pub struct StructFieldDef {
    pub name: String,
    pub ty: Type,
    pub rc: bool,
}

#[derive(Clone, Debug)]
pub struct Struct {
    pub name: NamePath,
    pub fields: HashMap<FieldID, StructFieldDef>,

    pub src_span: Option<Span>,
}

impl Struct {
    pub fn find_field(&self, name: &str) -> Option<FieldID> {
        self.fields.iter().find_map(|(id, field)| {
            if field.name.as_str() == name {
                Some(*id)
            } else {
                None
            }
        })
    }

    pub fn get_field(&self, id: FieldID) -> Option<&StructFieldDef> {
        self.fields.get(&id)
    }

    pub fn new(name: impl Into<NamePath>, src_span: Option<Span>) -> Self {
        Self {
            name: name.into(),
            fields: HashMap::new(),
            src_span,
        }
    }

    pub fn with_field(mut self, name: impl Into<String>, ty: Type, rc: bool) -> Self {
        let id = self
            .fields
            .keys()
            .max_by_key(|id| id.0)
            .map(|id| FieldID(id.0 + 1))
            .unwrap_or(FieldID(0));

        self.fields.insert(
            id,
            StructFieldDef {
                name: name.into(),
                ty,
                rc,
            },
        );

        self
    }

    pub fn with_fields(mut self, fields: HashMap<FieldID, StructFieldDef>) -> Self {
        self.fields.extend(fields);
        self
    }
}

#[derive(Eq, PartialEq, Hash, Clone, Copy, Debug, Ord, PartialOrd)]
pub struct FieldID(pub usize);

impl fmt::Display for FieldID {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

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
pub struct VariantCase {
    pub name: String,
    pub ty: Option<Type>,
    pub rc: bool,
}

#[derive(Clone, Debug)]
pub struct Variant {
    pub name: NamePath,
    pub cases: Vec<VariantCase>,

    pub src_span: Option<Span>,
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
            TypeDecl::Def(def) => Some(def.name()),
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
            TypeDecl::Def(def) => write!(f, "defined type `{}`", def.name()),
        }
    }
}

#[derive(Clone, Debug)]
pub enum TypeDef {
    Struct(Struct),
    Variant(Variant),
}

impl TypeDef {
    pub fn name(&self) -> &NamePath {
        match self {
            TypeDef::Struct(s) => &s.name,
            TypeDef::Variant(v) => &v.name,
        }
    }

    pub fn src_span(&self) -> Option<&Span> {
        match self {
            TypeDef::Struct(def) => def.src_span.as_ref(),
            TypeDef::Variant(def) => def.src_span.as_ref(),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum Type {
    /// no type or unknown type (used for raw pointers like void*)
    Nothing,

    Pointer(Box<Type>),
    Struct(StructID),
    Variant(StructID),
    Array {
        element: Box<Type>,
        dim: usize,
    },

    /// pointer to an RC object somewhere on the heap, which can be dereferenced to yield a value
    /// of the inner type. the resource type is Some in the case that the type is known, and
    /// None for the Any type
    RcPointer(Option<ClassID>),

    /// RC shared object struct of known or unknown type
    RcObject(Option<StructID>),

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
        Type::Pointer(Box::new(self))
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
            element: Box::new(self),
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
            Type::Variant(..) | Type::Array {..} | Type::Struct(..) => true,
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
            }
            Type::Array { element, dim } => write!(f, "{}[{}]", element, dim),
        }
    }
}