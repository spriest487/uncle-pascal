use std::{
    collections::hash_map::HashMap,
    fmt,
};

use crate::formatter::{
    InstructionFormatter,
    RawInstructionFormatter,
};
use pas_syn as syn;
use pas_syn::{Path};
use pas_typecheck as pas_ty;

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

pub const DYNARRAY_PTR_FIELD: FieldID = FieldID(0);
pub const DYNARRAY_LEN_FIELD: FieldID = FieldID(1);

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct NamePath {
    pub path: Path<String>,
    pub type_args: Vec<Type>,
}

impl fmt::Display for NamePath {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        RawInstructionFormatter.format_name(self, f)
    }
}

impl NamePath {
    pub fn from_decl(name: pas_ty::QualifiedDeclName, metadata: &Metadata) -> Self {
        let path_parts = name
            .qualified
            .into_parts()
            .into_iter()
            .map(|ident| ident.to_string());

        let type_args = name
            .type_args
            .iter()
            .map(|arg| metadata.translate_type(arg))
            .collect();

        NamePath {
            path: Path::from_parts(path_parts),
            type_args,
        }
    }

    pub fn from_ident_path(
        ident: &syn::IdentPath,
        type_args: Vec<Type>,
    ) -> Self {
        let path = Path::from_parts(ident.iter()
            .map(|ident| ident.to_string()));

        NamePath {
            path,
            type_args,
        }
    }

    pub fn from_parts<Iter: IntoIterator<Item = String>>(iter: Iter) -> Self {
        NamePath {
            path: Path::from_parts(iter),
            type_args: Vec::new(),
        }
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
            .entry(implementor)
            .or_insert_with(|| InterfaceImpl::new(methods_len));
        assert!(!impl_entry.methods.contains_key(&method));

        impl_entry.methods.insert(method, func_id);
    }

    pub fn method_index(&self, name: &str) -> Option<MethodID> {
        self.methods.iter().position(|m| m.name.as_str() == name).map(MethodID)
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
        self.fields
            .iter()
            .find_map(|(id, field)| if field.name.as_str() == name {
                Some(*id)
            } else {
                None
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
    type_args: Vec<pas_ty::Type>,
}

impl GlobalName {
    pub fn new(name: impl Into<String>, ns: impl IntoIterator<Item = impl Into<String>>) -> Self {
        let mut path: Vec<String> = ns.into_iter().map(Into::into).collect();
        path.push(name.into());

        Self { path, type_args: Vec::new() }
    }

    pub fn path(&self) -> impl Iterator<Item = &String> {
        self.path.iter()
    }

    pub fn with_ty_args(self, args: Vec<pas_ty::Type>) -> Self {
        assert_eq!(0, self.type_args.len(), "shouldn't already have type args when building a specialized GlobalName");

        Self {
            type_args: args,
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

        if self.type_args.len() > 0 {
            write!(f, "<")?;
            for (i, arg) in self.type_args.iter().enumerate() {
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

#[derive(Debug, Clone, Default)]
pub struct Metadata {
    type_defs: HashMap<StructID, TypeDef>,
    string_literals: HashMap<StringID, String>,
    ifaces: HashMap<InterfaceID, Interface>,

    dyn_array_structs: HashMap<Type, StructID>,

    functions: HashMap<FunctionID, FunctionDecl>,
}

impl Metadata {
    pub fn new() -> Self {
        Self {
            ..Default::default()
        }
    }

    pub fn extend(&mut self, other: &Metadata) {
        for (id, def) in other.type_defs() {
            if self.type_defs.contains_key(id) {
                let existing = &self.type_defs[id];
                panic!(
                    "duplicate struct ID {} in metadata (new: {}, existing: {})",
                    id,
                    def.name(),
                    existing.name()
                );
            }

            self.type_defs.insert(*id, def.clone());
        }

        for (id, string_lit) in &other.string_literals {
            if self.string_literals.contains_key(id) {
                panic!("duplicate string ID {} in metadata", id);
            }

            self.string_literals.insert(*id, string_lit.clone());
        }

        for (id, iface_def) in &other.ifaces {
            if self.ifaces.contains_key(id) {
                let existing = &self.ifaces[id];
                panic!(
                    "duplicate iface ID {} in metadata (new: {}, existing: {})",
                    id, iface_def.name, existing.name
                );
            }
            self.ifaces.insert(*id, iface_def.clone());
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
    }

    pub fn type_defs(&self) -> &HashMap<StructID, TypeDef> {
        &self.type_defs
    }

    pub fn get_struct(&self, struct_id: StructID) -> Option<&Struct> {
        match self.type_defs.get(&struct_id)? {
            TypeDef::Variant(..) => None,
            TypeDef::Struct(s) => Some(s),
        }
    }

    pub fn get_variant(&self, struct_id: StructID) -> Option<&Variant> {
        match self.type_defs.get(&struct_id)? {
            TypeDef::Struct(..) => None,
            TypeDef::Variant(v) => Some(v),
        }
    }

    fn next_struct_id(&mut self) -> StructID {
        (0..)
            .map(StructID)
            .find(|id| !self.type_defs.contains_key(id))
            .unwrap()
    }

    fn next_iface_id(&mut self) -> InterfaceID {
        (0..)
            .map(InterfaceID)
            .find(|id| !self.ifaces.contains_key(id))
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
        type_args: Vec<pas_ty::Type>,
    ) -> FunctionID {
        let global_name = match &func_decl.impl_iface {
            Some(_) => None,
            None => {
                let ns: Vec<_> = func_decl.ident.parent()
                    .expect("func always declared in ns")
                    .iter()
                    .map(syn::Ident::to_string)
                    .collect();
                let name = func_decl.ident.last().to_string();

                Some(GlobalName::new(name, ns)
                    .with_ty_args(type_args))
            },
        };

        self.insert_func(global_name)
    }

    pub fn insert_func(&mut self, global_name: Option<GlobalName>) -> FunctionID {
        let id = self.next_function_id();
        self.functions.insert(id, FunctionDecl { global_name });

        id
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

    pub fn func_desc(&self, id: FunctionID) -> String {
        self.functions
            .get(&id)
            .and_then(|decl| decl.global_name.as_ref())
            .map(GlobalName::to_string)
            .or_else(|| {
                self.ifaces.values().find_map(|iface| {
                    iface.impls.iter().find_map(|(impl_ty, iface_impl)| {
                        iface_impl.methods.iter().find_map(|(method, impl_id)| {
                            if *impl_id == id {
                                let impl_ty_name = self.pretty_ty_name(impl_ty);
                                Some(format!(
                                    "impl of {}.{} for {}",
                                    iface.name, method.0, impl_ty_name
                                ))
                            } else {
                                None
                            }
                        })
                    })
                })
            })
            .unwrap()
    }

    pub fn pretty_ty_name(&self, ty: &Type) -> String {
        match ty {
            Type::Struct(id) | Type::Variant(id) => self
                .type_defs
                .get(id)
                .map(|def| def.name().to_string())
                .unwrap_or_else(|| id.to_string()),

            Type::Array { element, dim } => {
                let elem_name = self.pretty_ty_name(element);
                format!("array [{}] of {}", dim, elem_name)
            },

            Type::RcPointer(class_id) => {
                let resource_name = match class_id {
                    None => "any".to_string(),

                    Some(ClassID::Interface(iface_id)) => {
                        let iface = self.ifaces.get(iface_id);

                        iface
                            .map(|def| def.name.to_string())
                            .unwrap_or_else(|| format!("<interface {}>", iface_id))
                    },

                    Some(ClassID::Class(struct_id)) => {
                        self.pretty_ty_name(&Type::Struct(*struct_id))
                    },
                };
                format!("rc {}", resource_name)
            },

            Type::Pointer(ty) => format!("^{}", self.pretty_ty_name(ty)),

            ty => ty.to_string(),
        }
    }

    pub fn define_struct(&mut self, struct_def: &pas_ty::ast::Class) -> StructID {
        let name_path = NamePath::from_decl(struct_def.name.clone(), &self);

        let mut fields = HashMap::new();
        for (id, member) in struct_def.members.iter().enumerate() {
            let name = member.ident.to_string();
            let ty = self.translate_type(&member.ty);
            let rc = member.ty.is_rc();

            fields.insert(FieldID(id), StructField { name, ty, rc });
        }

        // System.String is defined in System.pas but we need to refer to it before processing
        // any units, so it has a fixed IR struct ID
        let string_name = NamePath::from_parts(vec!["System".to_string(), "String".to_string()]);
        let id = if name_path == string_name {
            STRING_ID
        } else {
            self.next_struct_id()
        };

        assert!(
            !self
                .type_defs
                .values()
                .any(|def| *def.name() == name_path),
            "duplicate type def: {}",
            name_path
        );

        let struct_def = Struct::new(name_path).with_fields(fields);

        self.type_defs.insert(id, TypeDef::Struct(struct_def));
        id
    }

    pub fn define_variant(&mut self, variant_def: &pas_ty::ast::Variant) -> StructID {
        let name_path = NamePath::from_decl(variant_def.name.clone(), &self);

        let variant_id = self.next_struct_id();

        let mut cases = Vec::new();
        for case in &variant_def.cases {
            let (case_ty, case_rc) = match &case.data_ty {
                Some(data_ty) => (Some(self.translate_type(data_ty)), data_ty.is_rc()),
                None => (None, false),
            };
            cases.push(VariantCase {
                name: case.ident.to_string(),
                ty: case_ty,
                rc: case_rc,
            });
        }

        assert!(
            !self.type_defs.values().any(|def| *def.name() == name_path),
            "duplicate type def: {}",
            name_path
        );

        self.type_defs.insert(
            variant_id,
            TypeDef::Variant(Variant {
                name: name_path,
                cases,
            }),
        );

        variant_id
    }

    pub fn ifaces(&self) -> &HashMap<InterfaceID, Interface> {
        &self.ifaces
    }

    pub fn define_iface(&mut self, iface_def: &pas_ty::ast::Interface) -> InterfaceID {
        // System.Disposable is defined in System.pas but we need to refer to it before processing
        // any units, so it has a fixed IR struct ID
        let name = NamePath::from_decl(iface_def.name.clone(), self);

        let disposable_name =
            NamePath::from_parts(vec!["System".to_string(), "Disposable".to_string()]);
        let id = if name == disposable_name {
            DISPOSABLE_ID
        } else {
            self.next_iface_id()
        };

        let methods: Vec<_> = iface_def
            .methods
            .iter()
            .map(|method| {
                let self_ty = Type::RcPointer(Some(ClassID::Interface(id)));

                Method {
                    name: method.ident.to_string(),
                    return_ty: match &method.return_ty {
                        Some(pas_ty::Type::MethodSelf) => self_ty.clone(),
                        Some(return_ty) => self.translate_type(return_ty),
                        None => Type::Nothing,
                    },
                    params: method
                        .params
                        .iter()
                        .map(|param| match &param.ty {
                            pas_ty::Type::MethodSelf => self_ty.clone(),
                            param_ty => self.translate_type(param_ty),
                        })
                        .collect(),
                }
            })
            .collect();

        self.ifaces.insert(id, Interface::new(name, methods));

        id
    }

    pub fn find_iface(&self, iface_ident: &pas_ty::QualifiedDeclName) -> Option<InterfaceID> {
        let name = NamePath::from_decl(iface_ident.clone(), self);

        self.ifaces
            .iter()
            .find(|(_id, def)| def.name == name)
            .map(|(id, _def)| *id)
    }

    pub fn impl_method(
        &mut self,
        iface_id: InterfaceID,
        for_ty: Type,
        method_name: impl Into<String>,
        func_id: FunctionID,
    ) {
        let method_name = method_name.into();

        let iface = self.ifaces.get_mut(&iface_id).unwrap_or_else(|| {
            panic!(
                "trying to impl method {} for interface {} which doesn't exist",
                method_name, iface_id
            )
        });

        let index = iface.method_index(&method_name).unwrap();
        iface.add_impl(for_ty, index, func_id);
    }

    pub fn find_impl(&self, ty: &Type, iface_id: InterfaceID, method: MethodID) -> Option<FunctionID> {
        let iface = self.ifaces.get(&iface_id)
            .unwrap_or_else(|| panic!("missing metadata definition of iface {} - defined: {:#?}", iface_id, self.ifaces));

        let ty_impl = iface.impls.get(ty)?;

        ty_impl.methods.get(&method).cloned()
    }

    pub fn is_impl(&self, ty: &Type, iface_id: InterfaceID) -> bool {
        let impls = &self.ifaces[&iface_id].impls;
        impls.contains_key(ty)
    }

    pub fn find_impls(&self, ty: &Type) -> Vec<(InterfaceID, &InterfaceImpl)> {
        self.ifaces.iter()
            .filter_map(|(id, iface)| {
                let impl_for_ty = iface.impls.get(ty)?;
                Some((*id, impl_for_ty))
            })
            .collect()
    }

    pub fn translate_type(&self, ty: &pas_ty::Type) -> Type {
        match ty {
            pas_ty::Type::Nothing => Type::Nothing,
            pas_ty::Type::Nil => Type::Nothing.ptr(),

            pas_ty::Type::Interface(iface) => {
                let iface_id = match self.find_iface(&iface.name) {
                    Some(id) => id,
                    None => panic!("missing IR definition for interface {}", iface.name),
                };

                Type::RcPointer(Some(ClassID::Interface(iface_id)))
            },

            pas_ty::Type::Primitive(pas_ty::Primitive::Byte) => Type::U8,
            pas_ty::Type::Primitive(pas_ty::Primitive::Boolean) => Type::Bool,
            pas_ty::Type::Primitive(pas_ty::Primitive::Int32) => Type::I32,
            pas_ty::Type::Primitive(pas_ty::Primitive::Real32) => Type::F32,

            pas_ty::Type::Pointer(target) => self.translate_type(target).ptr(),

            pas_typecheck::Type::Record(class) | pas_typecheck::Type::Class(class) => {
                let ty_name = NamePath::from_decl(class.name.clone(), self);
                let struct_id = match self.find_struct(&ty_name) {
                    Some((id, _def)) => id,
                    None => panic!("{} was not found in metadata (not instantiated)", class.name),
                };

                match class.kind {
                    pas_syn::ast::ClassKind::Record => Type::Struct(struct_id),
                    pas_syn::ast::ClassKind::Object => {
                        let class_id = ClassID::Class(struct_id);
                        Type::RcPointer(Some(class_id))
                    },
                }
            },

            pas_ty::Type::Array { element, dim } => {
                let element = self.translate_type(element.as_ref());
                Type::Array {
                    element: Box::new(element),
                    dim: *dim,
                }
            },

            pas_ty::Type::DynArray { element } => {
                let element = self.translate_type(element.as_ref());

                let array_struct = match self.find_dyn_array_struct(&element) {
                    Some(id) => id,
                    None => panic!("missing dyn array IR struct definition for element type {}", element),
                };

                Type::RcPointer(Some(ClassID::Class(array_struct)))
            },

            pas_ty::Type::Variant(variant) => {
                let ty_name = NamePath::from_decl(variant.name.clone(), self);

                match self.find_variant(&ty_name) {
                    Some((id, _)) => Type::Variant(id),
                    None => panic!("missing IR struct metadata for variant {}", variant.name),
                }
            },

            pas_ty::Type::MethodSelf => {
                panic!("Self is not a real type in this context")
            },

            pas_ty::Type::GenericParam(param) => {
                panic!("{} is not a real type in this context: {:?}", param, pas_common::Backtrace::new())
            },

            pas_ty::Type::Function(sig) => {
                unimplemented!("No IR type exists to represent function: {}", sig)
            },

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

        let name = NamePath::from_parts(vec![format!("{}[]", element)]);

        let mut fields = HashMap::new();
        fields.insert(DYNARRAY_PTR_FIELD, StructField {
            name: "ptr".to_string(),
            ty: element.clone().ptr(),
            rc: false,
        });
        fields.insert(DYNARRAY_LEN_FIELD, StructField {
            name: "len".to_string(),
            ty: Type::I32,
            rc: false,
        });

        let struct_id = self.next_struct_id();
        self.type_defs.insert(struct_id, TypeDef::Struct(Struct { name, fields, }));

        self.dyn_array_structs.insert(element, struct_id);

        // we know it will have a disposer impl (and trust that the module
        // will generate the code for it if we give it an ID here)
        let disposer_id = self.next_function_id();
        self.functions.insert(disposer_id, FunctionDecl { global_name: None });

        let array_ref_ty = Type::RcPointer(Some(ClassID::Class(struct_id)));
        self.impl_method(DISPOSABLE_ID, array_ref_ty, "Dispose", disposer_id);

        struct_id
    }

    pub fn dyn_array_structs(&self) -> &HashMap<Type, StructID> {
        &self.dyn_array_structs
    }

    pub fn find_struct(&self, name: &NamePath) -> Option<(StructID, &Struct)> {
        self.type_defs.iter().find_map(|(id, def)| match def {
            TypeDef::Struct(struct_def) => {
                if struct_def.name == *name {
                    Some((*id, struct_def))
                } else {
                    None
                }
            },

            _ => None,
        })
    }

    pub fn find_variant(&self, name: &NamePath) -> Option<(StructID, &Variant)> {
        self.type_defs.iter().find_map(|(id, def)| match def {
            TypeDef::Variant(variant_def) => {
                if variant_def.name == *name {
                    Some((*id, variant_def))
                } else {
                    None
                }
            },

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
            },
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
}
