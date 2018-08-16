use std::{
    collections::hash_map::*,
    mem::size_of,
    fmt,
};

use types::*;
use node::{
    self,
    Identifier,
    UnitReferenceKind,
    TypeName,
    RecordKind,
};
use semantic::*;

#[derive(Clone, Debug)]
pub enum Named {
    Alias(Type),
    Record(RecordDecl),
    Class(RecordDecl),
    Function(FunctionDecl),
    Symbol(Type),
}

#[derive(Clone)]
pub struct Scope {
    local_name: Option<Identifier>,
    names: HashMap<Identifier, Named>,

    /* map of imported name => global name
     e.g. uses System.* adds String => System.String
     */
    imported_names: HashMap<Identifier, Identifier>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ScopedSymbol {
    /* symbol refers to a name that is in the current scope */
    Local {
        name: Identifier,
        decl_type: Type,
    },

    RecordMember {
        record_id: Identifier,
        record_type: Identifier,
        name: String,
        member_type: Type,
    },
}

impl ScopedSymbol {
    pub fn decl_type(&self) -> &Type {
        match self {
            ScopedSymbol::Local { decl_type, .. } =>
                decl_type,

            ScopedSymbol::RecordMember { member_type, .. } =>
                member_type,
        }
    }
}

impl fmt::Display for ScopedSymbol {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ScopedSymbol::Local { name, decl_type } =>
                write!(f, "{} ({})", name, decl_type),

            ScopedSymbol::RecordMember { record_id, name, member_type, record_type } => {
                write!(f, "{} ({} member of record {})",
                       record_id.child(name),
                       member_type,
                       record_type.name)
            }
        }
    }
}

impl node::Symbol for ScopedSymbol {
    type Type = Type;
}

impl node::ToSource for ScopedSymbol {
    fn to_source(&self) -> String {
        match self {
            ScopedSymbol::Local { name, .. } =>
                name.to_source(),
            ScopedSymbol::RecordMember { record_id, name, .. } =>
                format!("{}.{}", record_id.to_source(), name),
        }
    }
}

impl Default for Scope {
    fn default() -> Self {
        Scope::new().reference(Scope::system(), UnitReferenceKind::Namespaced)
    }
}

impl fmt::Debug for Scope {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "Scope `{}` {{", match self.local_name {
            Some(ref ns) => ns.to_string(),
            None => "(root)".to_owned(),
        })?;

        let types: Vec<_> = self.names.iter()
            .flat_map(|(name, named)| match named {
                &Named::Alias(ref dt) => Some((name, dt)),
                _ => None
            })
            .collect();

        let symbols: Vec<_> = self.names.iter()
            .flat_map(|(name, named)| match named {
                &Named::Symbol(ref dt) => Some((name, dt)),
                _ => None,
            })
            .collect();

        writeln!(f, "\ttypes: [")?;
        for (name, declared_type) in types {
            writeln!(f, "\t\t{}: {}", name, declared_type)?;
        }
        writeln!(f, "\t]")?;

        writeln!(f, "\tsymbols: [")?;
        for (name, sym_type) in symbols {
            writeln!(f, "\t\t{}: {}", name, sym_type)?;
        }
        writeln!(f, "\t]")?;

        writeln!(f, "\timported names: [")?;
        for (name, global_name) in self.imported_names.iter() {
            writeln!(f, "\t\t{}: {}", name, global_name)?;
        }
        writeln!(f, "\t]")?;

        writeln!(f, "}}")
    }
}

impl Scope {
    pub fn new() -> Self {
        Scope {
            local_name: None,
            names: HashMap::new(),
            imported_names: HashMap::new(),
        }
    }

    pub fn local_name(&self) -> Option<&Identifier> {
        self.local_name.as_ref()
    }

    pub fn system() -> Self {
        Scope::new()
            .with_local_namespace("System")
            /* standard primitives */
            .with_alias(Identifier::from("System.Byte"), Type::Byte)
//            .with_alias(Identifier::from("System.Int16"), DeclaredType::Int16)
//            .with_alias(Identifier::from("System.UInt16"), DeclaredType::UInt16)
            .with_alias(Identifier::from("System.Int32"), Type::Int32)
            .with_alias(Identifier::from("System.UInt32"), Type::UInt32)
            .with_alias(Identifier::from("System.Int64"), Type::Int64)
            .with_alias(Identifier::from("System.UInt64"), Type::UInt64)
            .with_alias(Identifier::from("System.NativeInt"), Type::NativeInt)
            .with_alias(Identifier::from("System.NativeUInt"), Type::NativeUInt)
            .with_alias(Identifier::from("System.Pointer"), Type::RawPointer)
            .with_alias(Identifier::from("System.Boolean"), Type::Boolean)
    }

    pub fn qualify_local_name(&self, name: &str) -> Identifier {
        match &self.local_name {
            &Some(ref local_name) => local_name.child(&name),
            &None => Identifier::from(name)
        }
    }

    pub fn with_alias(mut self, name: Identifier, named_type: Type) -> Self {
        self.names.insert(name, Named::Alias(named_type));
        self
    }

    pub fn with_function(mut self, name: Identifier, decl: FunctionDecl) -> Self {
        self.names.insert(name, Named::Function(decl));
        self
    }

    pub fn with_class(mut self, name: Identifier, decl: RecordDecl) -> Self {
        assert_eq!(RecordKind::Class, decl.kind);
        self.names.insert(name, Named::Class(decl));
        self
    }

    pub fn with_record(mut self, name: Identifier, decl: RecordDecl) -> Self {
        assert_eq!(RecordKind::Record, decl.kind);
        self.names.insert(name, Named::Record(decl));
        self
    }

    pub fn with_symbol_local(mut self, name: &str, decl_type: Type) -> Self {
        let name_id = Identifier::from(name);
        assert_eq!(0, name_id.namespace.len(), "names passed to with_symbol_local must be unqualified, but got {}", name);
        self.names.insert(name_id, Named::Symbol(decl_type));
        self
    }

    pub fn with_symbol_absolute<TId>(mut self, name: TId, decl_type: Type) -> Self
        where TId: Into<Identifier>
    {
        self.names.insert(name.into(), Named::Symbol(decl_type));
        self
    }

    pub fn with_local_namespace(mut self, name: &str) -> Self {
        self.local_name = match self.local_name {
            Some(current_name) => Some(current_name.child(name)),
            None => Some(Identifier::from(name)),
        };

        self
    }

    pub fn with_vars_local<'a>(mut self, vars: impl IntoIterator<Item=&'a VarDecl>) -> Self {
        for var in vars {
            assert_eq!(var.name.namespace.len(), 0, "vars passed to with_vars_local should not be namespaced! was {}", var.name);

            self = self.with_symbol_local(&var.name.name, var.decl_type.clone());
        }
        self
    }

    pub fn reference(mut self,
                     other: Scope,
                     ref_kind: UnitReferenceKind) -> Self {
        for (name, named) in other.names {
            match ref_kind {
                UnitReferenceKind::All => {
                    let imported_name = Identifier::from(&name.name);
                    self.imported_names.insert(imported_name, name.clone());
                }

                UnitReferenceKind::Name(ref imported_name) => {
                    if name.name == *imported_name {
                        self.imported_names.insert(Identifier::from(imported_name),
                                                   name.clone());
                    }
                }

                UnitReferenceKind::Namespaced => {
                    //do nothing
                }
            }

            self.names.insert(name, named);
        }
        self
    }

    pub fn reference_all(mut self,
                         others: impl IntoIterator<Item=Scope>)
                         -> Self {
        for scope in others {
            self = self.reference(scope, UnitReferenceKind::Namespaced);
        }
        self
    }

    fn get_symbol_imported(&self, name: &Identifier) -> Option<ScopedSymbol> {
        self.imported_names.get(name)
            .and_then(|global_name| self.get_symbol_global(global_name))
    }

    fn get_symbol_global(&self, name: &Identifier) -> Option<ScopedSymbol> {
        match self.names.get(&name) {
            Some(&Named::Symbol(ref symbol_type)) => {
                let local_sym = ScopedSymbol::Local {
                    name: name.clone(),
                    decl_type: symbol_type.clone(),
                };

                Some(local_sym)
            }
            _ => None
        }
    }

    pub fn get_symbol(&self, name: &Identifier) -> Option<ScopedSymbol> {
        /* todo: can this use find_named? */
        self.local_name.as_ref()
            .and_then(|local_name| {
                let name_in_local_ns = local_name.append(name);

                self.get_symbol_global(&name_in_local_ns)
            })
            .or_else(|| {
                // search local names which refer to records to see if
                // this is the field of a record
                name.parent().and_then(|record_id| {
                    self.find_record_member(&record_id, &name.name)
                })
            })
            .or_else(|| {
                self.get_symbol_imported(name)
            })
            .or_else(|| {
                self.get_symbol_global(name)
            })
    }

    fn find_named(&self, name: &Identifier) -> Option<&Named> {
        self.local_name.as_ref()
            .and_then(|local_name| {
                /* local name? */
                let name_in_local_ns = local_name.append(name);
                self.names.get(&name_in_local_ns)
            })
            .or_else(|| {
                /* name imported from another unit? */
                let global_name = self.imported_names.get(name)?;
                self.names.get(global_name)
            })
            .or_else(|| {
                /* fully-qualified global name? */
                self.names.get(name)
            })
    }
//
//    fn get_type_imported(&self, name: &Identifier) -> Option<Type> {
//        self.imported_names.get(name)
//            .and_then(|global_name| self.get_type_global(global_name))
//    }
//
//    fn find_base_type(&self, name: &Identifier) -> Option<Type> {
//        self.local_name.as_ref()
//            .and_then(|local_name| {
//                let name_in_local_ns = local_name.append(name);
//
//                self.get_type_global(&name_in_local_ns)
//            })
//            .or_else(|| {
//                self.get_type_imported(name)
//            })
//            .or_else(|| {
//                self.get_type_global(name)
//            })
//    }

    pub fn get_type(&self, parsed_type: &TypeName) -> Option<Type> {
        let mut result = match self.find_named(&parsed_type.name)? {
            Named::Class(class) => Type::Class(class.name.clone()),
            Named::Record(record) => Type::Record(record.name.clone()),
            Named::Alias(ty) => ty.clone(),

            Named::Function(_) |
            Named::Symbol(_) => return None,
        };

        for _ in 0..parsed_type.indirection {
            result = result.pointer();
        }

        if parsed_type.array_dimensions.len() > 0 {
            result = Type::Array(ArrayType {
                element: Box::new(result),
                first_dim: parsed_type.array_dimensions[0].clone(),
                rest_dims: parsed_type.array_dimensions[1..].iter().cloned().collect(),
            })
        }

        Some(result)
    }

    pub fn get_function(&self, name: &Identifier) -> Option<&FunctionDecl> {
        match self.find_named(name) {
            Some(Named::Function(decl)) => Some(decl),
            _ => None,
        }
    }

    pub fn get_record(&self, name: &Identifier) -> Option<&RecordDecl> {
        match self.find_named(name) {
            Some(Named::Record(decl)) => {
                assert_eq!(RecordKind::Record, decl.kind);
                Some(decl)
            }
            _ => None,
        }
    }

    fn find_record_member(&self,
                          parent_id: &Identifier,
                          member_name: &str) -> Option<ScopedSymbol> {
        let parent_sym = self.get_symbol(&parent_id)?;

        let record_decl = match parent_sym.decl_type() {
            Type::Record(name) => self.get_record(name),
            Type::Class(name) => self.get_class(name),

            /* records and classes are auto-derefed, so consider pointers to records of any
            indirection level */
            Type::Pointer(ptr) => {
                /* remove remaining levels of indirection */
                match ptr.remove_indirection() {
                    Type::Record(name) => self.get_record(name),
                    Type::Class(name) => self.get_class(name),
                    _ => None,
                }
            }

            _ => None
        }?;

        let member = record_decl.members.iter()
            .find(|m| m.name.to_string() == *member_name)
            .unwrap();

        Some(ScopedSymbol::RecordMember {
            record_id: parent_id.clone(),
            name: member_name.to_owned(),
            record_type: record_decl.name.clone(),
            member_type: member.decl_type.clone(),
        })
    }

    pub fn get_class(&self, name: &Identifier) -> Option<&RecordDecl> {
        match self.names.get(name) {
            Some(Named::Class(decl)) => {
                assert_eq!(RecordKind::Class, decl.kind);
                Some(decl)
            },
            _ => None,
        }
    }

    pub fn align_of(&self, ty: &Type) -> usize {
        const WORD_SIZE: usize = size_of::<usize>();

        let size = self.size_of(ty);
        let mut align = 0;

        while align < size {
            align += WORD_SIZE
        }
        align
    }

    pub fn size_of(&self, ty: &Type) -> usize {
        let record_size = |record: &RecordDecl| {
            record.members.iter()
                .map(|member| self.align_of(&member.decl_type))
                .sum()
        };

        match ty {
            Type::Nil |
            Type::RawPointer |
            Type::Pointer(_) |
            Type::Function(_) |
            Type::NativeInt |
            Type::NativeUInt =>
                size_of::<usize>() as usize,

            Type::Int64 |
            Type::UInt64 =>
                8,

            Type::UInt32 |
            Type::Int32 =>
                4,

            Type::Byte =>
                1,

            Type::Boolean =>
                1,

            Type::Record(name) => {
                let record_decl = self.get_record(name).expect("record type passed to size_of must exist");
                record_size(record_decl)
            }

            Type::Class(name) => {
                let class_decl = self.get_class(name).expect("class type passed to size_of must exist");
                record_size(class_decl)
            }

            Type::Array(array) =>
                array.total_elements() as usize * self.size_of(&array.element),
        }
    }
}
