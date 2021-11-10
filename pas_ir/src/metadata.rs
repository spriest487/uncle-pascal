use std::{collections::hash_map::{HashMap}, fmt};

use crate::formatter::{InstructionFormatter, RawInstructionFormatter};
use pas_syn as syn;
use pas_syn::{Ident, IdentPath, Path};
use pas_typecheck as pas_ty;

use linked_hash_map::LinkedHashMap;
use std::borrow::Cow;
use crate::dep_sort::sort_defs;

#[derive(Eq, PartialEq, Hash, Clone, Copy, Debug, Ord, PartialOrd)]
pub struct StringID(pub usize);

impl fmt::Display for StringID {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Eq, PartialEq, Hash, Clone, Copy, Debug, Ord, PartialOrd)]
pub struct StructID(pub usize);

impl fmt::Display for StructID {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Eq, PartialEq, Hash, Clone, Copy, Debug, Ord, PartialOrd)]
pub struct FieldID(pub usize);

impl fmt::Display for FieldID {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Eq, PartialEq, Hash, Clone, Copy, Debug, Ord, PartialOrd)]
pub struct InterfaceID(pub usize);

#[derive(Eq, PartialEq, Hash, Clone, Copy, Debug, Ord, PartialOrd)]
pub struct MethodID(pub usize);

impl fmt::Display for InterfaceID {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

// builtin fixed IDs
pub const DISPOSABLE_ID: InterfaceID = InterfaceID(0);
pub const DISPOSABLE_DISPOSE_METHOD: &str = "Dispose";
pub const DISPOSABLE_DISPOSE_INDEX: MethodID = MethodID(0);

pub const STRING_ID: StructID = StructID(1);
pub const STRING_CHARS_FIELD: FieldID = FieldID(0);
pub const STRING_LEN_FIELD: FieldID = FieldID(1);

pub const DYNARRAY_LEN_FIELD: FieldID = FieldID(0);
pub const DYNARRAY_PTR_FIELD: FieldID = FieldID(1);

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct NamePath {
    pub path: Path<String>,
    pub type_args: Option<Vec<Type>>,
}

impl fmt::Display for NamePath {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        RawInstructionFormatter.format_name(self, f)
    }
}

impl NamePath {
    pub fn from_decl(name: pas_ty::Symbol, metadata: &Metadata) -> Self {
        let path_parts = name
            .qualified
            .into_parts()
            .into_iter()
            .map(|ident| ident.to_string());

        let type_args = match name.type_args {
            Some(name_type_args) => {
                let types = name_type_args.items.iter()
                    .map(|arg| metadata.find_type(arg))
                    .collect();

                Some(types)
            },

            None => None,
        };


        NamePath {
            path: Path::from_parts(path_parts),
            type_args,
        }
    }

    pub fn from_ident_path(ident: &syn::IdentPath, type_args: Option<Vec<Type>>) -> Self {
        let path = Path::from_parts(ident.iter().map(|ident| ident.to_string()));

        NamePath { path, type_args }
    }

    pub fn from_parts<Iter: IntoIterator<Item = String>>(iter: Iter) -> Self {
        NamePath {
            path: Path::from_parts(iter),
            type_args: None,
        }
    }

    pub fn to_pretty_string<'a, TyFormat>(&self, ty_format: TyFormat) -> String
        where TyFormat: Fn(&Type) -> Cow<'a, str>,
    {
        let mut buf = self.path.join("::");

        if let Some(type_args) = self.type_args.as_ref() {
            buf.push('<');
            for (i, ty_arg) in type_args.iter().enumerate() {
                if i > 0 {
                    buf.push_str(", ");
                }

                let ty_name = ty_format(ty_arg);
                buf.push_str(&ty_name);
            }
            buf.push('>');
        }

        buf
    }
}

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
pub struct StructField {
    pub name: String,
    pub ty: Type,
    pub rc: bool,
}

#[derive(Clone, Debug)]
pub struct Struct {
    pub name: NamePath,
    pub fields: HashMap<FieldID, StructField>,
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

    pub fn get_field(&self, id: FieldID) -> Option<&StructField> {
        self.fields.get(&id)
    }

    pub fn new(name: impl Into<NamePath>) -> Self {
        Self {
            name: name.into(),
            fields: HashMap::new(),
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
            StructField {
                name: name.into(),
                ty,
                rc,
            },
        );

        self
    }

    pub fn with_fields(mut self, fields: HashMap<FieldID, StructField>) -> Self {
        self.fields.extend(fields);
        self
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

    Bool,
    U8,
    I32,
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
            Type::I32 => write!(f, "i32"),
            Type::F32 => write!(f, "f32"),
            Type::Bool => write!(f, "bool"),
            Type::U8 => write!(f, "u8"),
            Type::Pointer(target) => write!(f, "^{}", target),
            Type::Struct(id) => write!(f, "{{struct {}}}", id),
            Type::Variant(id) => write!(f, "{{variant {}}}", id),
            Type::RcPointer(id) => match id {
                None => write!(f, "any"),
                Some(ClassID::Class(id)) => write!(f, "class {}", id),
                Some(ClassID::Interface(id)) => write!(f, "iface {}", id),
            },
            Type::Array { element, dim } => write!(f, "{}[{}]", element, dim),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct GlobalName {
    path: Vec<String>,
    type_args: Option<pas_ty::TypeList>,
}

impl GlobalName {
    pub fn new(name: impl Into<String>, ns: impl IntoIterator<Item = impl Into<String>>) -> Self {
        let mut path: Vec<String> = ns.into_iter().map(Into::into).collect();
        path.push(name.into());

        Self {
            path,
            type_args: None,
        }
    }

    pub fn path(&self) -> impl Iterator<Item = &String> {
        self.path.iter()
    }

    pub fn with_ty_args(self, args: pas_ty::TypeList) -> Self {
        assert_eq!(
            None,
            self.type_args,
            "shouldn't already have type args when building a specialized GlobalName"
        );

        Self {
            type_args: Some(args),
            ..self
        }
    }
}

impl fmt::Display for GlobalName {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for (i, part) in self.path.iter().enumerate() {
            if i > 0 {
                write!(f, "::")?;
            }
            write!(f, "{}", part)?;
        }

        if let Some(type_args) = self.type_args.as_ref() {
            write!(f, "<")?;
            for (i, arg) in type_args.items.iter().enumerate() {
                if i > 0 {
                    write!(f, ", ")?;
                }
                write!(f, "{}", arg)?;
            }
            write!(f, ">")?;
        }

        Ok(())
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub struct FunctionID(pub usize);

impl fmt::Display for FunctionID {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "function {}", self.0)
    }
}

#[derive(Debug, Clone)]
pub struct FunctionDecl {
    pub global_name: Option<GlobalName>,
}

#[derive(Debug, Clone)]
pub enum InterfaceDecl {
    Forward(NamePath),
    Def(Interface),
}

impl InterfaceDecl {
    pub fn name(&self) -> &NamePath {
        match self {
            InterfaceDecl::Def(def) => &def.name,
            InterfaceDecl::Forward(name) => name,
        }
    }
}

#[derive(Clone, Debug)]
pub struct RcBoilerplatePair {
    pub release: FunctionID,
    pub retain: FunctionID,
}

#[derive(Debug, Clone, Default)]
pub struct Metadata {
    type_decls: LinkedHashMap<StructID, TypeDecl>,
    string_literals: LinkedHashMap<StringID, String>,
    ifaces: LinkedHashMap<InterfaceID, InterfaceDecl>,

    dyn_array_structs: LinkedHashMap<Type, StructID>,

    functions: LinkedHashMap<FunctionID, FunctionDecl>,

    rc_boilerplate_funcs: HashMap<Type, RcBoilerplatePair>,
}

impl Metadata {
    pub fn new() -> Self {
        Self {
            ..Default::default()
        }
    }

    pub fn extend(&mut self, other: &Metadata) {
        for (id, decl) in &other.type_decls {
            if let Some(conflict) = self.type_decls.get(id) {
                panic!(
                    "duplicate struct ID {} in metadata (new: {}, existing: {})",
                    id,
                    decl,
                    conflict,
                );
            };

            self.type_decls.insert(*id, decl.clone());
        }

        for (id, string_lit) in &other.string_literals {
            if self.string_literals.contains_key(id) {
                panic!("duplicate string ID {} in metadata", id);
            }

            self.string_literals.insert(*id, string_lit.clone());
        }

        for (id, iface_decl) in &other.ifaces {
            if let Some(conflict) = self.ifaces.get(id) {
                panic!(
                    "duplicate iface ID {} in metadata (new: {}, existing: {})",
                    id,
                    iface_decl.name(),
                    conflict.name()
                );
            }

            self.ifaces.insert(*id, iface_decl.clone());
        }

        for (id, func_decl) in &other.functions {
            if self.functions.contains_key(id) {
                let existing = &self.functions[id];

                let name = func_decl
                    .global_name
                    .as_ref()
                    .map(GlobalName::to_string)
                    .unwrap_or_else(|| "<unnamed>".to_string());
                let existing_name = existing
                    .global_name
                    .as_ref()
                    .map(GlobalName::to_string)
                    .unwrap_or_else(|| "<unnamed>".to_string());

                panic!(
                    "duplicate function ID {} in metadata (new: {}, existing: {})",
                    id, name, existing_name
                );
            }
            self.functions.insert(*id, func_decl.clone());
        }

        for (ty, funcs) in &other.rc_boilerplate_funcs {
            if self.rc_boilerplate_funcs.contains_key(ty) {
                panic!("duplicate rc boilerplate definitions for type {}", ty);
            }

            self.rc_boilerplate_funcs.insert(ty.clone(), funcs.clone());
        }

        for (el_ty, struct_id) in &other.dyn_array_structs {
            if self.dyn_array_structs.contains_key(el_ty) {
                panic!("duplicate dyn array type definition for element type {}", el_ty);
            }
            self.dyn_array_structs.insert(el_ty.clone(), *struct_id);
        }
    }

    pub fn type_defs(&self) -> impl Iterator<Item = (StructID, &TypeDef)> {
        self.type_decls.iter().filter_map(|(id, decl)| match decl {
            TypeDecl::Def(def) => Some((*id, def)),

            TypeDecl::Reserved |
            TypeDecl::Forward(..) => None,
        })
    }

    pub fn get_struct_def(&self, struct_id: StructID) -> Option<&Struct> {
        match self.type_decls.get(&struct_id)? {
            TypeDecl::Reserved |
            TypeDecl::Forward(..) => None,

            TypeDecl::Def(TypeDef::Variant(..)) => None,

            TypeDecl::Def(TypeDef::Struct(s)) => Some(s),
        }
    }

    pub fn get_variant_def(&self, struct_id: StructID) -> Option<&Variant> {
        match self.type_decls.get(&struct_id)? {
            TypeDecl::Reserved |
            TypeDecl::Forward(..) => None,

            TypeDecl::Def(TypeDef::Struct(..)) => None,

            TypeDecl::Def(TypeDef::Variant(v)) => Some(v),
        }
    }

    pub fn get_iface_def(&self, iface_id: InterfaceID) -> Option<&Interface> {
        match self.ifaces.get(&iface_id)? {
            InterfaceDecl::Def(def) => Some(def),
            InterfaceDecl::Forward(..) => None,
        }
    }

    fn next_struct_id(&mut self) -> StructID {
        (0..)
            .map(StructID)
            .find(|id| !self.type_decls.contains_key(id)
                && *id != STRING_ID)
            .unwrap()
    }

    fn next_iface_id(&mut self) -> InterfaceID {
        (0..)
            .map(InterfaceID)
            .find(|id| !self.ifaces.contains_key(id)
                && *id != DISPOSABLE_ID)
            .unwrap()
    }

    fn next_function_id(&mut self) -> FunctionID {
        (0..)
            .map(FunctionID)
            .find(|id| !self.functions.contains_key(id))
            .unwrap()
    }

    pub fn declare_func(
        &mut self,
        func_decl: &pas_ty::ast::FunctionDecl,
        type_args: Option<&pas_ty::TypeList>,
    ) -> FunctionID {
        let global_name = match &func_decl.impl_iface {
            Some(_) => None,
            None => {
                let ns: Vec<_> = func_decl
                    .ident
                    .parent()
                    .expect("func always declared in ns")
                    .iter()
                    .map(syn::Ident::to_string)
                    .collect();
                let name = func_decl.ident.last().to_string();

                let global_name = GlobalName::new(name, ns);

                Some(match type_args {
                    None => global_name,
                    Some(type_args) => global_name.with_ty_args(type_args.clone()),
                })
            }
        };

        self.insert_func(global_name)
    }

    pub fn insert_func(&mut self, global_name: Option<GlobalName>) -> FunctionID {
        let id = self.next_function_id();
        self.functions.insert(id, FunctionDecl { global_name });

        id
    }

    pub fn declare_rc_boilerplate(&mut self, ty: &Type) -> RcBoilerplatePair {
        if self.rc_boilerplate_funcs.contains_key(ty) {
            panic!("duplicate rc boilerplate declaration for type {}", ty);
        }

        let pair = RcBoilerplatePair {
            retain: self.insert_func(None),
            release: self.insert_func(None),
        };

        self.rc_boilerplate_funcs.insert(ty.clone(), pair.clone());

        pair
    }

    pub fn find_rc_boilerplate(&self, ty: &Type) -> Option<RcBoilerplatePair> {
        self.rc_boilerplate_funcs.get(ty).cloned()
    }

    pub fn rc_boilerplate_funcs(&self) -> impl Iterator<Item=(&Type, &RcBoilerplatePair)> {
        self.rc_boilerplate_funcs.iter()
    }

    pub fn find_function(&self, name: &GlobalName) -> Option<FunctionID> {
        self.functions
            .iter()
            .find(|(_id, func)| func.global_name.as_ref() == Some(name))
            .map(|(id, _func)| *id)
    }

    pub fn get_function(&self, id: FunctionID) -> Option<&FunctionDecl> {
        self.functions.get(&id)
    }

    pub fn func_desc(&self, id: FunctionID) -> Option<String> {
        self.functions
            .get(&id)
            .and_then(|decl| decl.global_name.as_ref())
            .map(GlobalName::to_string)
            .or_else(|| {
                self.ifaces().find_map(|(iface_id, iface)| {
                    iface.impls.iter().find_map(|(impl_ty, iface_impl)| {
                        iface_impl.methods.iter().find_map(|(method, impl_id)| {
                            if *impl_id == id {
                                let mut desc = format!("impl of {}.", iface.name);
                                let _ = self.format_method(iface_id, *method, &mut desc);
                                desc.push_str(" for ");
                                let _ = self.format_type(impl_ty, &mut desc);

                                Some(desc)
                            } else {
                                None
                            }
                        })
                    })
                })
            })
    }

    pub fn pretty_ty_name(&self, ty: &Type) -> Cow<str> {
        match ty {
            Type::Struct(id) | Type::Variant(id) => {
                let name = match self.type_decls.get(id) {
                    Some(TypeDecl::Forward(name)) => Some(name),
                    Some(TypeDecl::Def(def)) => Some(def.name()),
                    Some(TypeDecl::Reserved) | None => None,
                };

                match name {
                    None => Cow::Owned(id.to_string()),

                    Some(name) => {
                        Cow::Owned(name.to_pretty_string(|ty| self.pretty_ty_name(ty)))
                    }
                }
            },

            Type::Array { element, dim } => {
                let elem_name = self.pretty_ty_name(element);
                Cow::Owned(format!("array [{}] of {}", dim, elem_name))
            }

            Type::RcPointer(class_id) => {
                let resource_name = match class_id {
                    None => Cow::Borrowed("any"),

                    Some(ClassID::Interface(iface_id)) => {
                        let iface = self.get_iface_def(*iface_id);

                        Cow::Owned(iface
                            .map(|def| {
                                def.name.to_pretty_string(|ty| self.pretty_ty_name(ty))
                            })
                            .unwrap_or_else(|| {
                                format!("<interface {}>", iface_id)
                            }))
                    }

                    Some(ClassID::Class(struct_id)) => {
                        self.pretty_ty_name(&Type::Struct(*struct_id))
                    }
                };

                Cow::Owned(format!("rc {}", resource_name))
            }

            Type::Pointer(ty) => Cow::Owned(format!("^{}", self.pretty_ty_name(ty))),

            ty => Cow::Owned(ty.to_string()),
        }
    }

    pub fn reserve_new_struct(&mut self) -> StructID {
        let id = self.next_struct_id();
        self.type_decls.insert(id, TypeDecl::Reserved);
        id
    }

    pub fn reserve_struct(&mut self, id: StructID) {
        if self.type_decls.contains_key(&id) {
            panic!("reserving existing struct ID {}", id);
        }

        self.type_decls.insert(id, TypeDecl::Reserved);
    }

    // turn a reserved struct ID into a forward decl by name
    pub fn declare_struct(&mut self, id: StructID, name: &NamePath) {
        match &mut self.type_decls[&id] {
            reserved @ TypeDecl::Reserved => {
                *reserved = TypeDecl::Forward(name.clone());
            },

            TypeDecl::Forward(prev_name) => {
                assert_eq!(prev_name, name, "can't declare same struct multiple times with different names");
            }

            TypeDecl::Def(def) => {
                assert_eq!(def.name(), name, "can't declare same struct multiple times with different names");
            }
        }
    }

    pub fn define_struct(&mut self, id: StructID, struct_def: Struct) {
        match &self.type_decls[&id] {
            TypeDecl::Forward(name) => {
                assert_eq!(*name, struct_def.name);

                self.type_decls.insert(id, TypeDecl::Def(TypeDef::Struct(struct_def)));
            }

            _other => {
                panic!("expected named declaration to exist when defining {}", struct_def.name);
            }
        }
    }

    pub fn define_variant(&mut self, id: StructID, variant_def: Variant) {
        match &mut self.type_decls[&id] {
            TypeDecl::Forward(name) => {
                assert_eq!(*name, variant_def.name);

                self.type_decls.insert(id, TypeDecl::Def(TypeDef::Variant(variant_def)));
            }

            _other => {
                panic!("expected named declaration to exist when defining {}", variant_def.name);
            }
        }
    }

    pub fn ifaces(&self) -> impl Iterator<Item = (InterfaceID, &Interface)> {
        self.ifaces
            .iter()
            .filter_map(|(id, iface_decl)| match iface_decl {
                InterfaceDecl::Def(iface_def) => Some((*id, iface_def)),
                InterfaceDecl::Forward(..) => None,
            })
    }

    pub fn declare_iface(&mut self, name: &NamePath) -> InterfaceID {
        // System.Disposable is defined in System.pas but we need to refer to it before processing
        // any units, so it has a fixed IR struct ID
        let disposable_name =
            NamePath::from_parts(vec!["System".to_string(), "Disposable".to_string()]);
        if *name == disposable_name {
            self.ifaces.insert(DISPOSABLE_ID, InterfaceDecl::Forward(disposable_name));
            return DISPOSABLE_ID;
        }

        let existing = self.ifaces.iter().find_map(|(id, decl)| match decl {
            InterfaceDecl::Forward(decl_name) if decl_name == name => Some(*id),
            InterfaceDecl::Def(iface) if iface.name == *name => Some(*id),
            _ => None,
        });

        if let Some(existing) = existing {
            return existing;
        }

        let id = self.next_iface_id();
        self.ifaces.insert(id, InterfaceDecl::Forward(name.clone()));
        id
    }

    pub fn define_iface(&mut self, iface_def: Interface) -> InterfaceID {
        let id = self.declare_iface(&iface_def.name);

        self.ifaces.insert(id, InterfaceDecl::Def(iface_def));

        id
    }

    pub fn find_iface_decl(&self, iface_ident: &IdentPath) -> Option<InterfaceID> {
        let name = NamePath::from_parts(iface_ident.iter().map(Ident::to_string));

        self.ifaces.iter().find_map(|(id, decl)| {
            if *decl.name() == name {
                Some(*id)
            } else {
                None
            }
        })
    }

    pub fn impl_method(
        &mut self,
        iface_id: InterfaceID,
        for_ty: Type,
        method_name: impl Into<String>,
        func_id: FunctionID,
    ) {
        let method_name = method_name.into();

        match self.ifaces.get_mut(&iface_id) {
            Some(InterfaceDecl::Def(iface_def)) => {
                let index = iface_def.method_index(&method_name).unwrap();
                iface_def.add_impl(for_ty, index, func_id);
            }

            Some(InterfaceDecl::Forward(name)) => panic!(
                "trying to impl method {} for interface {} which isn't defined yet",
                method_name, name
            ),

            None => panic!(
                "trying to impl method {} for interface {} which doesn't exist",
                method_name, iface_id
            ),
        }
    }

    pub fn find_impl(
        &self,
        ty: &Type,
        iface_id: InterfaceID,
        method: MethodID,
    ) -> Option<FunctionID> {
        let iface = self.get_iface_def(iface_id).unwrap_or_else(|| {
            panic!(
                "missing metadata definition of iface {} - defined: {:#?}",
                iface_id, self.ifaces
            )
        });

        let ty_impl = iface.impls.get(ty)?;

        ty_impl.methods.get(&method).cloned()
    }

    pub fn impls(&self, ty: &Type) -> Vec<InterfaceID> {
        self.ifaces.iter()
            .filter_map(|(id, _decl)| if self.is_impl(ty, *id) {
                Some(*id)
            } else {
                None
            })
            .collect()
    }

    pub fn is_impl(&self, ty: &Type, iface_id: InterfaceID) -> bool {
        let impls = &self.get_iface_def(iface_id).unwrap().impls;
        impls.contains_key(ty)
    }

    pub fn find_impls(&self, ty: &Type) -> Vec<(InterfaceID, &InterfaceImpl)> {
        self.ifaces()
            .filter_map(|(id, iface)| {
                let impl_for_ty = iface.impls.get(ty)?;
                Some((id, impl_for_ty))
            })
            .collect()
    }

    pub fn find_type(&self, ty: &pas_ty::Type) -> Type {
        match ty {
            pas_ty::Type::Nothing => Type::Nothing,
            pas_ty::Type::Nil => Type::Nothing.ptr(),

            pas_ty::Type::Interface(iface) => {
                let iface_id = match self.find_iface_decl(iface) {
                    Some(id) => id,
                    None => panic!("missing IR definition for interface {}", iface),
                };

                Type::RcPointer(Some(ClassID::Interface(iface_id)))
            }

            pas_ty::Type::Primitive(pas_ty::Primitive::Byte) => Type::U8,
            pas_ty::Type::Primitive(pas_ty::Primitive::Boolean) => Type::Bool,
            pas_ty::Type::Primitive(pas_ty::Primitive::Int32) => Type::I32,
            pas_ty::Type::Primitive(pas_ty::Primitive::Real32) => Type::F32,
            pas_ty::Type::Primitive(pas_ty::Primitive::Pointer) => Type::Nothing.ptr(),

            pas_ty::Type::Pointer(target) => self.find_type(target).ptr(),

            pas_ty::Type::Record(class) | pas_ty::Type::Class(class) => {
                expect_no_generic_args(&class, class.type_args.as_ref());

                let ty_name = NamePath::from_decl(*class.clone(), self);
                let struct_id = match self.find_type_decl(&ty_name) {
                    Some(id) => id,
                    None => panic!("{} was not found in metadata (not instantiated)", class),
                };

                match ty {
                    pas_ty::Type::Class(..) => {
                        let class_id = ClassID::Class(struct_id);
                        Type::RcPointer(Some(class_id))
                    }

                    pas_ty::Type::Record(..) => Type::Struct(struct_id),

                    _ => unreachable!(),
                }
            }

            pas_ty::Type::Array { element, dim } => {
                let element = self.find_type(element.as_ref());
                Type::Array {
                    element: Box::new(element),
                    dim: *dim,
                }
            }

            pas_ty::Type::DynArray { element } => {
                let element = self.find_type(element.as_ref());

                let array_struct = match self.find_dyn_array_struct(&element) {
                    Some(id) => id,
                    None => panic!(
                        "missing dyn array IR struct definition for element type {}",
                        element
                    ),
                };

                Type::RcPointer(Some(ClassID::Class(array_struct)))
            }

            pas_ty::Type::Variant(variant) => {
                expect_no_generic_args(&variant, variant.type_args.as_ref());

                let ty_name = NamePath::from_decl(*variant.clone(), self);

                match self.find_type_decl(&ty_name) {
                    Some(id) => Type::Variant(id),
                    None => panic!("missing IR struct metadata for variant {}", variant),
                }
            }

            pas_ty::Type::MethodSelf => panic!("Self is not a real type in this context"),

            pas_ty::Type::GenericParam(param) => panic!(
                "{} is not a real type in this context: {:?}",
                param,
                pas_common::Backtrace::new()
            ),

            pas_ty::Type::Function(sig) => {
                unimplemented!("No IR type exists to represent function: {}", sig)
            }

            pas_ty::Type::Any => Type::RcPointer(None),
        }
    }

    pub fn find_dyn_array_struct(&self, element: &Type) -> Option<StructID> {
        self.dyn_array_structs.get(element).cloned()
    }

    pub fn define_dyn_array_struct(&mut self, element: Type) -> StructID {
        assert!(
            !self.dyn_array_structs.contains_key(&element),
            "duplicate IR struct definition for dynamic array with element {}",
            element
        );

        let name = NamePath::from_parts(vec![
            format!("array of {}", self.pretty_ty_name(&element))
        ]);

        let mut fields = HashMap::new();
        fields.insert(
            DYNARRAY_LEN_FIELD,
            StructField {
                name: "len".to_string(),
                ty: Type::I32,
                rc: false,
            },
        );
        fields.insert(
            DYNARRAY_PTR_FIELD,
            StructField {
                name: "ptr".to_string(),
                ty: element.clone().ptr(),
                rc: false,
            },
        );

        let struct_id = self.next_struct_id();
        self.type_decls.insert(
            struct_id,
            TypeDecl::Def(TypeDef::Struct(Struct { name, fields })),
        );

        self.dyn_array_structs.insert(element, struct_id);

        // the rc boilerplate impls for a dynarray should be empty
        // dyn array structs are heap-allocated and don't need structural ref-counting
        // (but they do need custom finalization to clean up references they hold)
        self.declare_rc_boilerplate(&Type::Struct(struct_id));

        // we know it will have a disposer impl (and trust that the module
        // will generate the code for it if we give it an ID here)
//        let disposer_id = self.next_function_id();
//        self.functions
//            .insert(disposer_id, FunctionDecl { global_name: None });

//        let array_ref_ty = Type::RcPointer(Some(ClassID::Class(struct_id)));
//        self.impl_method(DISPOSABLE_ID, array_ref_ty, "Dispose", disposer_id);

        struct_id
    }

    pub fn dyn_array_structs(&self) -> &LinkedHashMap<Type, StructID> {
        &self.dyn_array_structs
    }

    pub fn dyn_array_element_ty(&self, array_class_id: StructID) -> Option<&Type> {
        let (el_ty, _) = self.dyn_array_structs().iter()
            .filter(|(_el_ty, struct_id)| array_class_id == **struct_id)
            .next()?;

        Some(el_ty)
    }

    pub fn find_type_decl(&self, name: &NamePath) -> Option<StructID> {
        self.type_decls.iter().find_map(|(id, def)| match def {
            TypeDecl::Def(TypeDef::Struct(struct_def)) if struct_def.name == *name => Some(*id),

            TypeDecl::Def(TypeDef::Variant(variant_def)) if variant_def.name == *name => Some(*id),

            TypeDecl::Forward(forward_name) if *forward_name == *name => Some(*id),

            _ => None,
        })
    }

    // find the declared ID and definition of a struct. if the struct is only forward-declared
    // when this call is made, the definition part of the result will be None
    pub fn find_struct_def(&self, name: &NamePath) -> Option<(StructID, &Struct)> {
        self.type_decls.iter().find_map(|(id, def)| match def {
            TypeDecl::Def(TypeDef::Struct(struct_def)) if struct_def.name == *name => {
                Some((*id, struct_def))
            }

            _ => None,
        })
    }

    pub fn find_variant_def(&self, name: &NamePath) -> Option<(StructID, &Variant)> {
        self.type_decls.iter().find_map(|(id, def)| match def {
            TypeDecl::Def(TypeDef::Variant(variant_def)) if variant_def.name == *name => {
                Some((*id, variant_def))
            }

            _ => None,
        })
    }

    pub fn find_or_insert_string(&mut self, s: &str) -> StringID {
        let existing =
            self.string_literals
                .iter()
                .find_map(|(id, literal)| if literal == s { Some(*id) } else { None });

        match existing {
            Some(id) => id,
            None => {
                let next_id = self
                    .string_literals
                    .keys()
                    .max_by_key(|id| id.0)
                    .map(|id| StringID(id.0 + 1))
                    .unwrap_or(StringID(0));
                self.string_literals.insert(next_id, s.to_string());
                next_id
            }
        }
    }

    pub fn find_string_id(&self, string: &str) -> Option<StringID> {
        self.string_literals.iter().find_map(|(id, string_lit)| {
            if string_lit == string {
                Some(*id)
            } else {
                None
            }
        })
    }

    pub fn get_string(&self, id: StringID) -> Option<&String> {
        self.string_literals.get(&id)
    }

    pub fn strings(&self) -> impl Iterator<Item = (StringID, &str)> + '_ {
        self.string_literals.iter().map(|(id, s)| (*id, s.as_str()))
    }

    // hack: we don't always end up with types properly ordered by structural dependencies
    // as a result of the order we encounter types in, so this gets called to sort them before
    // finishing the module (assuming backends expect the types to be ordered e.g. like in C)
    pub fn sort_type_defs_by_deps(&mut self) {
        let mut unsorted = self.type_decls.clone();

        // remove all defs into a separate collection
        let mut defs = Vec::new();
        let mut decls = LinkedHashMap::new();

        while let Some((id, decl)) = unsorted.pop_front() {
            match decl {
                TypeDecl::Reserved => { decls.insert(id, TypeDecl::Reserved); },
                TypeDecl::Forward(name) => { decls.insert(id, TypeDecl::Forward(name)); },

                TypeDecl::Def(def) => { defs.push((id, def)); }
            }
        }

        let sorted_defs = sort_defs(defs, self);

        self.type_decls = decls;
        for (id, def) in sorted_defs {
            self.type_decls.insert(id, TypeDecl::Def(def));
        }
    }
}

fn expect_no_generic_args<T: fmt::Display>(target: &T, type_args: Option<&pas_ty::TypeList>) {
    if let Some(type_args) = type_args {
        let any_generic_args = type_args.items.iter().any(|arg| arg.is_generic_param());
        assert!(
            !any_generic_args,
            "name of translated variant must not contain unspecialized generics: {}",
            target
        );
    }
}