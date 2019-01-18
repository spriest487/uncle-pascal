use {
    std::{
        collections::hash_map::{
            HashMap,
            Entry,
        },
        fmt,
    },
    pas_typecheck as pas_ty,
};

#[derive(Eq, PartialEq, Hash, Clone, Copy, Debug)]
pub struct StructId(pub usize);

impl fmt::Display for StructId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Clone, Debug)]
pub struct StructField {
    pub name: String,
    pub ty: Type,
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
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum Type {
    Nothing,
    Pointer(Box<Type>),
    Struct(StructId),
    Bool,
    I32,
    F32,
}

impl Type {
    pub fn ptr(self) -> Self {
        Type::Pointer(Box::new(self))
    }

    pub fn is_struct(&self) -> bool {
        match self {
            Type::Struct(_) =>  true,
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
            Type::Pointer(target) => write!(f, "^{}", target),
            Type::Struct(id) => write!(f, "{{{}}}", id),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Metadata {
    structs: HashMap<StructId, Struct>,
}

impl Metadata {
    pub fn new() -> Self {
        Self {
            structs: HashMap::new(),
        }
    }

    pub fn extend(&mut self, other: &Metadata) {
        for (id, struct_def) in &other.structs {
            match self.structs.entry(*id) {
                Entry::Occupied(_) => panic!("duplicate ID {} in metadata", id),
                Entry::Vacant(entry) => { entry.insert(struct_def.clone()); },
            }
        }
    }

    pub fn structs(&self) -> &HashMap<StructId, Struct> {
        &self.structs
    }

    pub fn translate_struct(&mut self, struct_def: &pas_ty::ast::Class) -> StructId {
        let mut fields = HashMap::new();
        for (id, member) in struct_def.members.iter().enumerate() {
            let name = member.ident.to_string();
            let ty = self.translate_type(&member.ty);

            fields.insert(id, StructField { name, ty });
        }

        let id = StructId(self.structs.len());
        self.structs.insert(id, Struct { name: struct_def.ident.to_string(), fields });
        id
    }

    pub fn translate_type(&self, ty: &pas_ty::Type) -> Type {
        match ty {
            pas_typecheck::Type::Nothing => Type::Nothing,
            pas_typecheck::Type::Boolean => Type::Bool,
            pas_typecheck::Type::Integer => Type::I32,
            pas_typecheck::Type::Real32 => Type::F32,

            pas_typecheck::Type::Record(class) => {
                let ty_name = class.ident.to_string();
                let (id, _) = self.find_struct(&ty_name)
                    .unwrap_or_else(|| panic!("structure {} must exist in metadata", ty));;
                Type::Struct(id)
            },

            pas_typecheck::Type::Class(class) => {
                let ty_name = class.ident.to_string();
                let (id, _) = self.find_struct(&ty_name)
                    .unwrap_or_else(|| panic!("class {} must exist in metadata", ty));
                Type::Struct(id).ptr()
            },

            pas_typecheck::Type::Function(_) => unimplemented!(),
        }
    }

    pub fn find_struct(&self, name: &str) -> Option<(StructId, &Struct)> {
        self.structs.iter().find_map(|(id, struct_def)| if struct_def.name == name {
            Some((*id, struct_def))
        } else {
            None
        })
    }
}