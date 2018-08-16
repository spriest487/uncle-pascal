use std::{
    collections::hash_map::*,
    mem::size_of,
    iter,
    fmt,
};

use types::*;
use node::{
    self,
    Identifier,
    UnitReferenceKind,
    RecordKind,
    ConstantExpression,
};
use consts::{
    EnumConstant,
    SetConstant,
};
use semantic::*;

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum BindingKind {
    Mutable,
    Uninitialized,
    Immutable,
}

impl fmt::Display for BindingKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            BindingKind::Mutable => write!(f, "mutable"),
            BindingKind::Uninitialized => write!(f, "uninitialized"),
            BindingKind::Immutable => write!(f, "immutable"),
        }
    }
}

impl BindingKind {
    pub fn mutable(&self) -> bool {
        match self {
            | BindingKind::Uninitialized
            | BindingKind::Mutable =>
                true,

            | BindingKind::Immutable =>
                false
        }
    }

    pub fn initialized(&self) -> bool {
        match self {
            | BindingKind::Immutable
            | BindingKind::Mutable =>
                true,

            | BindingKind::Uninitialized =>
                false
        }
    }

    pub fn initialize(self) -> Self {
        match self {
            | BindingKind::Mutable
            | BindingKind::Immutable =>
                self,
            | BindingKind::Uninitialized =>
                BindingKind::Mutable,
        }
    }
}

#[derive(Clone, Debug)]
pub struct SymbolBinding {
    decl_type: Type,
    kind: BindingKind,
}

#[derive(Clone, Debug)]
pub enum Named {
    TypeAlias(Type),
    Record(RecordDecl),
    Class(RecordDecl),
    Function(FunctionDecl),
    Const(ConstantExpression),
    Enumeration(EnumerationDecl),
    Set(SetDecl),
    Symbol(SymbolBinding),
}

#[derive(Clone)]
enum ScopeNamespace {
    /**
        either the root (program/module) or a local scope inside the root.
        we can access anything else declared in the root namespace and public members of
        other namespaces.
        qualifying a name adds no prefix.
    */
    Root,

    /**
        the global scope of a unit.
        we can access anything (public or private) that is also declared in this unit,
        or public members of any other unit.
        qualifying a name adds the namespace of this unit as a prefix.
    */
    Unit(Identifier),

    /**
        a local scope inside a unit - a function body.
        visibility and access from this scope is the same as the Unit namespace.
        qualifying a name adds no prefix.
    */
    Local(Identifier),
}

#[derive(Clone)]
pub struct Scope {
    namespace: ScopeNamespace,
    names: HashMap<Identifier, Named>,

    /* map of imported name => global name
     e.g. uses System.* adds String => System.String
     */
    imported_names: HashMap<Identifier, Identifier>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ScopedSymbol {
    /* symbol refers to a name that is in the current scope */
    Local {
        name: Identifier,
        decl_type: Type,
        binding_kind: BindingKind,
    },

    RecordMember {
        record_id: Identifier,
        record_type: Identifier,
        binding_kind: BindingKind,
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

    pub fn name(&self) -> Identifier {
        match self {
            ScopedSymbol::Local { name, .. } =>
                name.clone(),
            ScopedSymbol::RecordMember { record_id, name, .. } =>
                record_id.child(name),
        }
    }

    pub fn initialized(&self) -> bool {
        match self {
            | ScopedSymbol::RecordMember { binding_kind, .. }
            | ScopedSymbol::Local { binding_kind, .. } =>
                binding_kind.initialized(),
        }
    }

    pub fn mutable(&self, from_ns: Option<&Identifier>) -> bool {
        match self {
            ScopedSymbol::Local { binding_kind, .. } => binding_kind.mutable(),
            ScopedSymbol::RecordMember { record_id, binding_kind, .. } => {
                let same_ns = record_id.parent().as_ref() == from_ns;
                same_ns && binding_kind.mutable()
            }
        }
    }

    pub fn binding_kind(&self) -> BindingKind {
        match self {
            ScopedSymbol::Local { binding_kind, .. } => *binding_kind,
            ScopedSymbol::RecordMember { binding_kind, .. } => *binding_kind,
        }
    }
}

impl fmt::Display for ScopedSymbol {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ScopedSymbol::Local { name, decl_type, binding_kind } =>
                write!(f, "{} ({} {})", name, binding_kind, decl_type),

            ScopedSymbol::RecordMember {
                record_id,
                name,
                member_type,
                record_type,
                binding_kind
            } => {
                write!(f, "{} ({} member of {} record {})",
                       record_id.child(name),
                       member_type,
                       binding_kind,
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

impl fmt::Debug for Scope {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "{} {{", self.namespace_description())?;

        let mut types = Vec::new();
        let mut symbols = Vec::new();
        let mut functions = Vec::new();
        let mut classes = Vec::new();
        let mut records = Vec::new();
        let mut consts = Vec::new();
        let mut enums = Vec::new();
        let mut sets = Vec::new();

        for (name, named) in self.names.iter() {
            match named {
                Named::TypeAlias(ty) => types.push((name, ty)),
                Named::Symbol(sym) => symbols.push((name, sym)),
                Named::Function(func) => functions.push((name, func)),
                Named::Class(decl) => classes.push((name, decl)),
                Named::Record(decl) => records.push((name, decl)),
                Named::Const(val) => consts.push((name, val)),
                Named::Enumeration(decl) => enums.push((name, decl)),
                Named::Set(decl) => sets.push((name, decl)),
            }
        }

        writeln!(f, "\ttypes: [")?;
        for (name, declared_type) in types {
            writeln!(f, "\t\t{}: {}", name, declared_type)?;
        }
        writeln!(f, "\t]")?;

        writeln!(f, "\tsymbols: [")?;
        for (name, binding) in symbols {
            writeln!(f, "\t\t{}: {} ({})", name, binding.decl_type, binding.kind)?;
        }
        writeln!(f, "\t]")?;

        writeln!(f, "\tfunctions: [")?;
        for (name, func) in functions {
            writeln!(f, "\t\t{}: {}", name, func.name)?;
        }
        writeln!(f, "\t]")?;

        writeln!(f, "\tclasses: [")?;
        for (name, class) in classes {
            writeln!(f, "\t\t{}: {}", name, class.name)?;
        }
        writeln!(f, "\t]")?;

        writeln!(f, "\trecords: [")?;
        for (name, record) in records {
            writeln!(f, "\t\t{}: {}", name, record.name)?;
        }
        writeln!(f, "\t]")?;

        writeln!(f, "\tenumerations: [")?;
        for (name, enum_decl) in enums {
            writeln!(f, "\t\t{}: {}", name, enum_decl.name)?;
        }
        writeln!(f, "\t]")?;

        writeln!(f, "\tsets: [")?;
        for (name, set_decl) in sets {
            writeln!(f, "\t\t{}: {}", name, set_decl.name)?;
        }
        writeln!(f, "\t]")?;

        writeln!(f, "\tconsts: [")?;
        for (name, val) in consts {
            writeln!(f, "\t\t{}: {}", name, val.to_source())?;
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
    pub fn new_root() -> Self {
        let root = Scope {
            names: HashMap::new(),
            imported_names: HashMap::new(),
            namespace: ScopeNamespace::Root,
        };

        root.reference(&Scope::system(), UnitReferenceKind::Namespaced)
    }

    pub fn system() -> Self {
        let system = Scope {
            names: HashMap::new(),
            imported_names: HashMap::new(),
            namespace: ScopeNamespace::Unit(Identifier::from("System")),
        };

        system
            /* standard primitives */
            .with_type_alias("Byte", Type::Byte)
//            .with_alias("System.Int16", DeclaredType::Int16)
//            .with_alias("System.UInt16", DeclaredType::UInt16)
            .with_type_alias("Int32", Type::Int32)
            .with_type_alias("UInt32", Type::UInt32)
            .with_type_alias("Int64", Type::Int64)
            .with_type_alias("UInt64", Type::UInt64)
            .with_type_alias("Float64", Type::Float64)
            .with_type_alias("NativeInt", Type::NativeInt)
            .with_type_alias("NativeUInt", Type::NativeUInt)
            .with_type_alias("Pointer", Type::RawPointer)
            .with_type_alias("Boolean", Type::Boolean)
    }

    pub fn new_unit(namespace: impl Into<Identifier>) -> Self {
        let scope = Scope {
            namespace: ScopeNamespace::Unit(namespace.into()),
            names: HashMap::new(),
            imported_names: HashMap::new(),
        };

        scope.reference(&Scope::system(), UnitReferenceKind::Namespaced)
    }

    pub fn new_local(parent: &Self) -> Self {
        let parent_ns = match &parent.namespace {
            | ScopeNamespace::Local(ns)
            | ScopeNamespace::Unit(ns) => {
                ScopeNamespace::Local(ns.clone())
            }

            | ScopeNamespace::Root =>
                ScopeNamespace::Root,
        };

        let child = Scope {
            namespace: parent_ns,
            names: HashMap::new(),
            imported_names: HashMap::new(),
        };

        child.reference(parent, UnitReferenceKind::All)
    }

    pub fn namespace_description(&self) -> String {
        match &self.namespace {
            ScopeNamespace::Unit(ns) => format!("Unit namespace `{}`", ns),
            ScopeNamespace::Local(ns) => format!("Local namespace (in `{}`)", ns),
            ScopeNamespace::Root => "Root namespace".to_string(),
        }
    }

    pub fn namespace_qualify(&self, name: &str) -> Identifier {
        match &self.namespace {
            | ScopeNamespace::Local(_)
            | ScopeNamespace::Root
            => Identifier::from(name),

            | ScopeNamespace::Unit(ns)
            => ns.child(name),
        }
    }

    pub fn with_type_alias(mut self, name: &str, named_type: Type) -> Self {
        let full_name = self.namespace_qualify(name);
        self.names.insert(full_name, Named::TypeAlias(named_type));
        self
    }

    pub fn with_function(mut self, decl: FunctionDecl) -> Self {
        let name = self.namespace_qualify(&decl.name);
        self.names.insert(name, Named::Function(decl));
        self
    }

    pub fn with_class(mut self, decl: RecordDecl) -> Self {
        let name = self.namespace_qualify(&decl.name);
        assert_eq!(RecordKind::Class, decl.kind);
        self.names.insert(name, Named::Class(decl));
        self
    }

    pub fn with_enumeration(mut self, decl: EnumerationDecl) -> Self {
        for (ord, name) in decl.names.iter().enumerate() {
            let const_name = self.namespace_qualify(&decl.name);
            let enum_const = EnumConstant::new(ord as u64, name, const_name);
            let val = ConstantExpression::Enum(enum_const);
            self.names.insert(Identifier::from(name), Named::Const(val));
        }

        let qualified_name = self.namespace_qualify(&decl.name);
        self.names.insert(qualified_name, Named::Enumeration(decl));
        self
    }

    pub fn with_set(mut self, decl: SetDecl) -> Self {
        let qualified_name = self.namespace_qualify(&decl.name);

        /* it would make more sense to add the values first so we didn't
        have to copy the set decl, but in the weird case that a set constant
        has the same name as the set, we want the error to be raised on the
        inner value, not the set decl */
        self.names.insert(qualified_name.clone(), Named::Set(decl.clone()));

        if let node::SetEnumeration::Inline(names) = &decl.enumeration {
            for name in names.iter() {
                let name_qualified = self.namespace_qualify(name);
                let name_const = SetConstant::new(iter::once(name), qualified_name.clone());
                let name_expr = ConstantExpression::Set(name_const);

                self.names.insert(name_qualified, Named::Const(name_expr));
            }
        }

        self
    }

    pub fn with_record(mut self, decl: RecordDecl) -> Self {
        let name = self.namespace_qualify(&decl.name);
        assert_eq!(RecordKind::Record, decl.kind);
        self.names.insert(name, Named::Record(decl));
        self
    }

    pub fn with_binding(mut self, name: &str, decl_type: Type, kind: BindingKind) -> Self {
        let binding = SymbolBinding {
            decl_type,
            kind,
        };

        let full_name = self.namespace_qualify(name);

        self.names.insert(full_name, Named::Symbol(binding));
        self
    }

    pub fn initialize_symbol(mut self, name: &Identifier) -> Self {
        let find_symbol = self.find_named(name)
            .map(|(full_name, named)| (full_name, named.clone()));

        match find_symbol {
            Some((full_name, Named::Symbol(mut binding))) => {
                binding.kind = binding.kind.initialize();
                self.names.insert(full_name.clone(), Named::Symbol(binding));
            }

            _ =>
                panic!("called initialize_symbol() on something that wasn't a symbol: `{}`", name)
        }

        self
    }

    pub fn with_const(mut self, name: &str, val: ConstantExpression) -> Self {
        let name = self.namespace_qualify(name);
        self.names.insert(name, Named::Const(val));
        self
    }

    pub fn reference(mut self,
                     other: &Scope,
                     ref_kind: UnitReferenceKind) -> Self {
        for (name, named) in other.names.iter() {
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

            self.names.insert(name.clone(), named.clone());
        }
        self
    }

    pub fn reference_all(mut self,
                         others: impl IntoIterator<Item=Scope>)
                         -> Self {
        for scope in others {
            self = self.reference(&scope, UnitReferenceKind::Namespaced);
        }
        self
    }

    fn get_symbol_imported(&self, name: &Identifier) -> Option<ScopedSymbol> {
        self.imported_names.get(name)
            .and_then(|global_name| self.get_symbol_global(global_name))
    }

    fn get_symbol_global(&self, name: &Identifier) -> Option<ScopedSymbol> {
        match self.names.get(&name) {
            Some(Named::Symbol(binding)) => {
                Some(ScopedSymbol::Local {
                    name: name.clone(),
                    decl_type: binding.decl_type.clone(),
                    binding_kind: binding.kind,
                })
            }

            Some(Named::Function(func)) => {
                Some(ScopedSymbol::Local {
                    name: name.clone(),
                    decl_type: Type::Function(Box::new(func.signature())),
                    binding_kind: BindingKind::Immutable,
                })
            }
            _ => None
        }
    }

    pub fn unit_namespace(&self) -> Option<&Identifier> {
        match &self.namespace {
            | ScopeNamespace::Unit(ns)
            | ScopeNamespace::Local(ns)
            => Some(ns),

            | ScopeNamespace::Root
            => None
        }
    }

    pub fn get_symbol(&self, name: &Identifier) -> Option<ScopedSymbol> {
        /* todo: can this use find_named? */
        self.unit_namespace().as_ref()
            .and_then(|local_namespace| {
                let name_in_local_ns = local_namespace.append(name);
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
            .or_else(|| {
                let (func_id, func) = self.get_function(name)?;

                Some(ScopedSymbol::Local {
                    name: func_id,
                    decl_type: Type::Function(Box::from(func.signature())),
                    binding_kind: BindingKind::Immutable,
                })
            })
    }

    /*
        diff of symbols uninitialized in a previous scope -> symbols uninitialized in this scope, to
        see which ones have been initialized since then
    */
    pub fn initialized_since<'a>(&self, other: &'a Scope) -> Vec<&'a Identifier> {
        let still_uninitialized: Vec<_> = self.uninitialized_symbols().collect();

        other.uninitialized_symbols()
            .filter(|name| !still_uninitialized.contains(name))
            .collect()
    }

    pub fn uninitialized_symbols(&self) -> impl Iterator<Item=&Identifier> {
        self.names.iter()
            .filter_map(|(name, named)| match named {
                Named::Symbol(binding) => {
                    match binding.kind.initialized() {
                        true => None,
                        false => Some(name)
                    }
                }

                _ => None,
            })
    }

    fn find_named(&self, name: &Identifier) -> Option<(Identifier, &Named)> {
        self.unit_namespace().as_ref()
            .and_then(|local_name| {
                /* local name? */
                let name_in_local_ns = local_name.append(name);
                self.names.get(&name_in_local_ns).map(|named| (name_in_local_ns, named))
            })
            .or_else(|| {
                /* name imported from another unit? */
                let global_name = self.imported_names.get(name)?;
                self.names.get(global_name).map(|named| (global_name.clone(), named))
            })
            .or_else(|| {
                /* fully-qualified global name? */
                self.names.get(name).map(|named| (name.clone(), named))
            })
    }

    pub fn get_type_alias(&self, alias: &Identifier) -> Option<Type> {
        let result = match self.find_named(alias) {
            Some((class_name, Named::Class(_))) =>
                Type::Class(class_name),
            Some((record_name, Named::Record(_))) =>
                Type::Record(record_name),
            Some((enumeration_name, Named::Enumeration(_))) =>
                Type::Enumeration(enumeration_name),
            Some((set_name, Named::Set(_))) =>
                Type::Set(set_name),
            Some((_, Named::TypeAlias(ty))) =>
                ty.clone(),

            None |
            Some((_, Named::Const(_))) |
            Some((_, Named::Function(_))) |
            Some((_, Named::Symbol(_))) =>
                return None,
        };

        Some(result)
    }

    pub fn get_enumeration(&self, name: &Identifier) -> Option<(Identifier, &EnumerationDecl)> {
        match self.find_named(name) {
            Some((ref id, Named::Enumeration(decl))) => Some((id.clone(), decl)),
            _ => None,
        }
    }

    pub fn get_set(&self, name: &Identifier) -> Option<(Identifier, &SetDecl)> {
        match self.find_named(name) {
            Some((ref id, Named::Set(decl))) => Some((id.clone(), decl)),
            _ => None,
        }
    }

    pub fn get_set_enumeration(&self, name: &Identifier) -> Option<Vec<Identifier>> {
        let (set_id, set) = self.get_set(name)?;
        match &set.enumeration {
            node::SetEnumeration::Inline(names) => {
                let set_ns = set_id.parent();

                Some(names.iter()
                    .map(|name| Identifier::child_of_namespace(set_ns.as_ref(), name))
                    .collect())
            }

            node::SetEnumeration::Named(enum_name) => {
                let (enum_name, named_enum) = self.get_enumeration(enum_name)?;
                let enum_ns = enum_name.parent();

                Some(named_enum.names.iter()
                    .map(|name| Identifier::child_of_namespace(enum_ns.as_ref(), name))
                    .collect())
            }
        }
    }

    pub fn get_function(&self, name: &Identifier) -> Option<(Identifier, &FunctionDecl)> {
        match self.find_named(name) {
            Some((ref id, Named::Function(decl))) => Some((id.clone(), decl)),
            _ => None,
        }
    }

    pub fn get_record(&self, name: &Identifier) -> Option<(Identifier, &RecordDecl)> {
        match self.find_named(name) {
            Some((ref id, Named::Record(decl))) => {
                assert_eq!(RecordKind::Record, decl.kind);
                Some((id.clone(), decl))
            }
            _ => None,
        }
    }

    pub fn get_const(&self, name: &Identifier) -> Option<(Identifier, &ConstantExpression)> {
        match self.find_named(name) {
            Some((ref id, Named::Const(const_expr))) => {
                Some((id.clone(), const_expr))
            }
            _ => None,
        }
    }

    fn find_record_member(&self,
                          parent_id: &Identifier,
                          member_name: &str) -> Option<ScopedSymbol> {
        let parent_sym = self.get_symbol(&parent_id)?;

        let (record_id, record_decl) = match parent_sym.decl_type() {
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

        let member = record_decl.get_member(member_name)?;

        Some(ScopedSymbol::RecordMember {
            record_id: parent_sym.name(),
            name: member_name.to_owned(),
            record_type: record_id,
            member_type: member.decl_type.clone(),
            binding_kind: parent_sym.binding_kind(),
        })
    }

    pub fn get_class(&self, name: &Identifier) -> Option<(Identifier, &RecordDecl)> {
        match self.find_named(name)? {
            (ref id, Named::Class(decl)) => {
                assert_eq!(RecordKind::Class, decl.kind);
                Some((id.clone(), decl))
            }
            _ => None,
        }
    }

    /* given the fully qualified name of a class, find the destructor
    function for that class. None if the class doesn't exist or doesn't have
    a destructor */
    pub fn get_destructor(&self, name: &Identifier) -> Option<(&Identifier, &FunctionDecl)> {
        let (_, class) = self.get_class(name)?;

        self.names.iter()
            .filter_map(|(name, named)| match named {
                Named::Function(func) => {
                    match func.is_destructor_of(class) {
                        true => Some((name, func)),
                        false => None,
                    }
                }
                _ => None
            })
            .next()
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

    pub fn size_of_record(&self, record: &RecordDecl) -> usize {
        record.members.iter()
            .map(|member| self.align_of(&member.decl_type))
            .sum()
    }

    pub fn size_of(&self, ty: &Type) -> usize {
        let enum_size = |_enum_decl: &EnumerationDecl| {
            /* todo: all enums are u64s */
            8
        };

        let set_size = |_enum_decl: &SetDecl| {
            /* todo: all sets are u64s */
            8
        };

        match ty {
            Type::Nil |
            Type::RawPointer |
            Type::Pointer(_) |
            Type::Function(_) |
            Type::NativeInt |
            Type::UntypedRef |
            Type::NativeUInt =>
                size_of::<usize>(),

            Type::Int64 |
            Type::UInt64 |
            Type::Float64 =>
                8,

            Type::Set(id) => {
                let (_, set_decl) = self.get_set(id)
                    .expect("set type passed to size_of must exist");
                set_size(set_decl)
            }

            Type::Enumeration(id) => {
                let (_, enum_decl) = self.get_enumeration(id)
                    .expect("enum type passed to size_of must exist");
                enum_size(enum_decl)
            }

            Type::UInt32 |
            Type::Int32 =>
                4,

            Type::Byte =>
                1,

            Type::Boolean =>
                1,

            Type::Record(name) => {
                let (_, record_decl) = self.get_record(name).expect("record type passed to size_of must exist");
                self.size_of_record(record_decl)
            }

            Type::Class(name) => {
                let (_, class_decl) = self.get_class(name).expect("class type passed to size_of must exist");
                self.size_of_record(class_decl)
            }

            Type::DynamicArray(_array) => {
                /* dynamic arrays are heap-allocated and so they *should* just
                be a single pointer... currently this is probably not true */
                size_of::<usize>()
            }

            Type::Array(array) =>
                array.total_elements() as usize * self.size_of(&array.element),
        }
    }
}

#[cfg(test)]
mod test {
    use std::rc::Rc;
    use super::*;
    use source;
    use tokens;
    use keywords;
    use consts::IntConstant;

    fn fake_context() -> SemanticContext {
        let location = source::Location::new("test", 0, 0);
        let token = tokens::Keyword(keywords::Program);
        SemanticContext {
            token: source::Token::new(token, location),
            scope: Rc::new(Scope::new_root()),
        }
    }

    #[test]
    fn resolves_consts_in_referenced_units() {
        let const_val = ConstantExpression::Integer(IntConstant::from(3));

        let imported = Scope::new_unit("NS1")
            .with_const("CONST1", const_val.clone());

        let scope = Scope::new_unit("NS2")
            .reference(&imported, UnitReferenceKind::Namespaced);

        let expected_id = Identifier::from("NS1.CONST1");
        match scope.get_const(&expected_id) {
            Some((result_id, result_val)) => {
                assert_eq!(expected_id, result_id);
                assert_eq!(const_val, *result_val);
            }
            None => panic!("name {} must be found in scope {:?}", expected_id, scope)
        }
    }

    #[test]
    fn add_record_adds_record_in_local_ns() {
        let record_decl = RecordDecl {
            context: fake_context(),
            members: vec![],
            name: "World".to_string(),
            kind: RecordKind::Record,
            variant_part: None,
        };

        let scope = Scope::new_unit("Hello")
            .with_record(record_decl);

        let (result_id, result) = scope.get_record(&Identifier::from("Hello.World"))
            .unwrap_or_else(|| {
                panic!("get_record should return a result in scope {:?}", scope)
            });
        assert_eq!("World", &result.name);
        assert_eq!(Identifier::from("Hello.World"), result_id);
    }
}