use {
    std::{
        collections::hash_map::HashMap,
        fmt,
    },
    pas_typecheck as pas_ty,
};

#[derive(Eq, PartialEq, Hash, Clone, Copy, Debug)]
pub struct StringId(pub usize);

impl fmt::Display for StringId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Eq, PartialEq, Hash, Clone, Copy, Debug)]
pub struct TypeId(pub usize);

impl fmt::Display for TypeId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

pub const DISPOSABLE_ID: TypeId = TypeId(321);

pub const STRING_ID: TypeId = TypeId(573196);
pub const STRING_CHARS_FIELD: usize = 0;
pub const STRING_LEN_FIELD: usize = 1;

pub const RC_ID: TypeId = TypeId(36);
pub const RC_REF_COUNT_FIELD: usize = 0;
pub const RC_VALUE_FIELD: usize = 1;


#[derive(Clone, Debug)]
pub struct Interface {
    pub name: String,
    pub methods: Vec<String>,
    pub impls: HashMap<Type, InterfaceImpl>,
}

impl Interface {
    pub fn new(name: impl Into<String>, methods: impl Into<Vec<String>>) -> Self {
        Self {
            name: name.into(),
            methods: methods.into(),
            impls: HashMap::new(),
        }
    }

    pub fn add_impl(
        &mut self,
        implementor: Type,
        method: impl Into<String>,
        func_name: impl Into<String>)
    {
        let method = method.into();
        assert!(self.methods.contains(&method));

        let methods_len = self.methods.len();
        let impl_entry = self.impls.entry(implementor)
            .or_insert_with(|| InterfaceImpl::new(methods_len));
        assert!(!impl_entry.methods.contains_key(&method));

        impl_entry.methods.insert(method, func_name.into());
    }
}

#[derive(Clone, Debug)]
pub struct InterfaceImpl {
    // method name -> global name of function
    pub methods: HashMap<String, String>,
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
    pub name: String,
    pub fields: HashMap<usize, StructField>,
}

impl Struct {
    pub fn find_field(&self, name: &str) -> Option<usize> {
        self.fields.iter()
            .find_map(|(id, field)| if field.name == name {
                Some(*id)
            } else {
                None
            })
    }

    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            fields: HashMap::new(),
        }
    }

    pub fn with_field(mut self, name: impl Into<String>, ty: Type, rc: bool) -> Self {
        let id = self.fields.keys().max().map(|id| *id + 1).unwrap_or(0);

        self.fields.insert(id, StructField {
            name: name.into(),
            ty,
            rc,
        });

        self
    }

    pub fn with_fields(mut self, fields: HashMap<usize, StructField>) -> Self {
        self.fields.extend(fields);
        self
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum Type {
    Nothing,
    Pointer(Box<Type>),
    Struct(TypeId),
    Rc(Box<Type>),
    Bool,
    U8,
    I32,
    F32,
}

impl Type {
    pub fn ptr(self) -> Self {
        Type::Pointer(Box::new(self))
    }

    pub fn rc(self) -> Self {
        Type::Rc(Box::new(self))
    }

    pub fn is_any_struct(&self) -> bool {
        match self {
            Type::Struct(_) => true,
            _ => false,
        }
    }

    pub fn is_struct(&self, id: TypeId) -> bool {
        match self {
            Type::Struct(ty_id) => *ty_id == id,
            _ => false,
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
            Type::Struct(id) => write!(f, "{{{}}}", id),
            Type::Rc(id) => write!(f, "{{{} rc}}", id),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Metadata {
    structs: HashMap<TypeId, Struct>,
    string_literals: HashMap<StringId, String>,
    ifaces: HashMap<TypeId, Interface>,
}

impl Metadata {
    pub fn system() -> Self {
        let mut metadata = Metadata::new();
        metadata.ifaces.insert(DISPOSABLE_ID, Interface::new("Disposable", vec!["Dispose".to_string()]));

        metadata.structs.insert(RC_ID, Struct::new("Rc")
            .with_field("rc", Type::I32, false)
            .with_field("value", Type::Nothing.ptr(), false));

        metadata.structs.insert(STRING_ID, Struct::new("String")
            .with_field("chars", Type::I32.ptr(), false)
            .with_field("len", Type::I32, false));
        metadata.impl_method(DISPOSABLE_ID, Type::Struct(STRING_ID), "Dispose", "StringDispose");

        metadata
    }

    pub fn new() -> Self {
        Self {
            structs: HashMap::new(),
            string_literals: HashMap::new(),
            ifaces: HashMap::new(),
        }
    }

    pub fn extend(&mut self, other: &Metadata) {
        for (id, struct_def) in &other.structs {
            if self.structs.contains_key(id) {
                let existing = self.structs.get(id).unwrap();
                panic!("duplicate struct ID {} in metadata (new: {}, existing: {})", id, struct_def.name, existing.name);
            }

            self.structs.insert(*id, struct_def.clone());
        }

        for (id, string_lit) in &other.string_literals {
            if self.string_literals.contains_key(id) {
                panic!("duplicate string ID {} in metadata", id);
            }

            self.string_literals.insert(*id, string_lit.clone());
        }
    }

    pub fn structs(&self) -> &HashMap<TypeId, Struct> {
        &self.structs
    }

    fn next_type_id(&mut self) -> TypeId {
        (0..).map(TypeId)
            .filter(|id| !self.structs.contains_key(id) && !self.ifaces.contains_key(id))
            .next()
            .unwrap()
    }

    pub fn define_struct(&mut self, struct_def: &pas_ty::ast::Class) -> TypeId {
        let mut fields = HashMap::new();
        for (id, member) in struct_def.members.iter().enumerate() {
            let name = member.ident.to_string();
            let ty = self.translate_type(&member.ty);
            let rc = member.ty.is_rc();

            fields.insert(id, StructField { name, ty, rc });
        }

        let id = self.next_type_id();

        let struct_name = struct_def.ident.to_string();
        assert!(!self.structs.values().any(|def| def.name == struct_name), "duplicate struct def: {}", struct_name);

        let struct_def = Struct::new(struct_name).with_fields(fields);

        self.structs.insert(id, struct_def);
        id
    }

    pub fn define_iface(&mut self, iface_def: &pas_ty::ast::Interface) -> TypeId {
        let methods: Vec<_> = iface_def.methods.iter()
            .map(|method| method.ident.to_string())
            .collect();

        let id = self.next_type_id();

        self.ifaces.insert(id, Interface::new(iface_def.ident.to_string(), methods));

        id
    }

    pub fn impl_method(&mut self,
                       iface_id: TypeId,
                       for_ty: Type,
                       method: impl Into<String>,
                       func_name: impl Into<String>)
    {
        self.ifaces.get_mut(&iface_id)
            .unwrap_or_else(|| panic!("trying to impl method for interface {} which doesn't exist", iface_id))
            .add_impl(for_ty, method, func_name);
    }

    pub fn find_impl(&self, ty: &Type, iface_id: TypeId, method: &str) -> Option<&str> {
        self.ifaces.get(&iface_id)
            .unwrap_or_else(|| panic!("expected interface {} to exist", iface_id))
            .impls.get(ty)
            .and_then(|ty_impl| ty_impl.methods.get(method))
            .map(|func_name| func_name.as_str())
    }

    pub fn translate_type(&self, ty: &pas_ty::Type) -> Type {
        match ty {
            pas_ty::Type::Nothing => Type::Nothing,
            pas_ty::Type::Nil => Type::Nothing.ptr(),

            pas_ty::Type::Interface(iface) => {
                unimplemented!("translate_type: {}", iface)
            }

            pas_ty::Type::Primitive(pas_ty::Primitive::Byte) => Type::U8,
            pas_ty::Type::Primitive(pas_ty::Primitive::Boolean) => Type::Bool,
            pas_ty::Type::Primitive(pas_ty::Primitive::Int32) => Type::I32,
            pas_ty::Type::Primitive(pas_ty::Primitive::Real32) => Type::F32,

            pas_ty::Type::Pointer(target) => self.translate_type(target)
                .ptr(),

            pas_typecheck::Type::Record(class) => {
                let ty_name = class.ident.to_string();
                let (id, _) = self.find_struct(&ty_name)
                    .unwrap_or_else(|| panic!("structure {} must exist in metadata", ty));
                Type::Struct(id)
            }

            pas_typecheck::Type::Class(class) => {
                let ty_name = class.ident.to_string();
                let (id, _) = self.find_struct(&ty_name)
                    .unwrap_or_else(|| panic!("structure {} must exist in metadata", ty));

                Type::Struct(id).rc()
            }

            pas_typecheck::Type::GenericSelf => unreachable!("Self is not a real type in this context"),

            pas_typecheck::Type::Function(_) => unimplemented!(),
        }
    }

    pub fn find_struct(&self, name: &str) -> Option<(TypeId, &Struct)> {
        self.structs.iter()
            .find_map(|(id, struct_def)| if struct_def.name == name {
                Some((*id, struct_def))
            } else {
                None
            })
    }

    pub fn find_or_insert_string(&mut self, s: &str) -> StringId {
        let existing = self.string_literals.iter()
            .find_map(|(id, literal)| if literal == s {
                Some(*id)
            } else {
                None
            });

        match existing {
            Some(id) => id,
            None => {
                let next_id = self.string_literals.keys()
                    .max_by_key(|id| id.0)
                    .map(|id| StringId(id.0 + 1))
                    .unwrap_or(StringId(0));
                self.string_literals.insert(next_id, s.to_string());
                next_id
            }
        }
    }

    pub fn find_string_id(&self, string: &str) -> Option<StringId> {
        self.string_literals.iter()
            .find_map(|(id, string_lit)| if string_lit == string {
                Some(*id)
            } else {
                None
            })
    }

    pub fn get_string(&self, id: StringId) -> Option<&String> {
        self.string_literals.get(&id)
    }

    pub fn strings(&self) -> impl Iterator<Item=(StringId, &str)> + '_ {
        self.string_literals.iter().map(|(id, s)| (*id, s.as_str()))
    }
}