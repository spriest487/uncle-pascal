mod bindings;
mod scoped_symbol;
mod interface;

#[cfg(test)]
mod test;

pub use self::bindings::{
    BindingKind,
    SymbolBinding,
};
pub use self::scoped_symbol::{
    ScopedSymbol
};
use self::interface::{
    Interface,
};

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
    ConstExpression,
};
use consts::{
    EnumConstant,
    SetConstant,
};
use semantic::*;

#[derive(Clone, Debug)]
pub struct NamedFunction {
    decl: FunctionDecl,
    defined: bool,
}

fn expect_overload_ok(new: &NamedFunction,
                      previous: &NamedFunction,
                      interface: Option<&Identifier>)
                      -> SemanticResult<()> {
    /* can't have two different declarations with different signatures */
    if new.decl.signature() != previous.decl.signature() {
        return Err(SemanticError::name_in_use(&new.decl.name, new.decl.context.clone()));
    }

    /* we can forward-declare implementations as many times as we like, but we can
        never define them multiple times */
    if new.defined && previous.defined {
        return Err(SemanticError::multiple_function_def(
            new.decl.clone(),
            previous.decl.clone(),
            interface.cloned(),
        ));
    }

    Ok(())
}

#[derive(Clone, Debug)]
enum Named {
    TypeAlias(Type),
    Record(RecordDecl),
    Class(RecordDecl),
    Function(NamedFunction),
    Const(ConstExpression, Type),
    Enumeration(EnumerationDecl),
    Interface(Interface),
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
            ScopeNamespace::Unit(ns) => format!("unit namespace `{}`", ns),
            ScopeNamespace::Local(ns) => format!("local namespace (in `{}`)", ns),
            ScopeNamespace::Root => "root namespace".to_string(),
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

    pub fn with_function_def(self, func: Function) -> SemanticResult<Self> {
        self.with_function(func.decl, true)
    }

    pub fn with_function_decl(self, func: FunctionDecl) -> SemanticResult<Self> {
        self.with_function(func, false)
    }

    fn with_function(mut self, decl: FunctionDecl, defined: bool) -> SemanticResult<Self> {
        let new_func = NamedFunction {
            decl,
            defined,
        };

        match new_func.decl.implements.clone() {
            Some(implements) => {
                let implemented_for = self.full_type_name(&implements.for_type)
                    .expect("typechecker should reject invalid impl types");

                self.names.get_mut(&implements.interface)
                    /* the interface we are implementing must have already been declared somewhere */
                    .and_then(|named| match named {
                        Named::Interface(iface) => Some(iface),
                        _ => None
                    })
                    .ok_or_else(|| SemanticError::unknown_symbol(
                        implements.interface.clone(),
                        new_func.decl.context.clone(),
                    ))
                    .and_then(|interface| {
                        interface.add_impl(implemented_for, new_func.clone())
                    })?;


                Ok(self)
            }

            /* normal function decl or definition */
            None => {
                let name = self.namespace_qualify(&new_func.decl.name);

                /* it's fine to declare a function with the same signature many times, but we can't
                define it more than once, and we can't declare it with a different signature */
                match self.names.entry(name.clone()) {
                    /* this is the first decl */
                    Entry::Vacant(slot) => {
                        slot.insert(Named::Function(new_func));
                    }

                    Entry::Occupied(mut slot) => {
                        let previous_decl = match slot.get_mut() {
                            Named::Function(previous) => previous,
                            _ => return Err(SemanticError::name_in_use(name, new_func.decl.context)),
                        };
                        expect_overload_ok(&new_func, previous_decl, None)?;

                        previous_decl.defined = previous_decl.defined || defined;
                    }
                }
                Ok(self)
            }
        }
    }

    pub fn with_class(mut self, decl: RecordDecl) -> Self {
        let name = self.namespace_qualify(&decl.name);
        assert_eq!(RecordKind::Class, decl.kind);
        self.names.insert(name, Named::Class(decl));
        self
    }

    pub fn with_interface(mut self, decl: InterfaceDecl) -> SemanticResult<Self> {
        let name = self.namespace_qualify(&decl.name);

        match self.names.entry(name.clone()) {
            Entry::Vacant(slot) => {
                slot.insert(Named::Interface(Interface::new(decl)));
            }

            Entry::Occupied(_) => return Err(SemanticError::name_in_use(name, decl.context)),
        }

        Ok(self)
    }

    pub fn with_enumeration(mut self, decl: EnumerationDecl) -> Self {
        let qualified_name = self.namespace_qualify(&decl.name);
        let enum_type = Type::Enumeration(qualified_name.clone());

        for (ord, name) in decl.names.iter().enumerate() {
            let const_name = self.namespace_qualify(&decl.name);
            let enum_const = EnumConstant::new(ord as u64, name, const_name);
            let val = node::ConstExpression::Enum(enum_const);
            self.names.insert(Identifier::from(name), Named::Const(val, enum_type.clone()));
        }

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
            let set_type = Type::Set(qualified_name.clone());

            for name in names.iter() {
                let name_qualified = self.namespace_qualify(name);
                let name_const = SetConstant::new(iter::once(name), qualified_name.clone());
                let name_expr = node::ConstExpression::Set(name_const);

                self.names.insert(name_qualified, Named::Const(name_expr, set_type.clone()));
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
            .map(|(full_name, named)| (full_name.clone(), named.clone()));

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

    pub fn with_const(mut self, name: &str, val: ConstExpression, as_type: Option<Type>) -> Self {
        let name = self.namespace_qualify(name);
        let val_type = as_type.unwrap_or_else(|| val.value_type());

        self.names.insert(name, Named::Const(val, val_type));
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
                    decl_type: Type::Function(Box::new(func.decl.signature())),
                    binding_kind: BindingKind::Function,
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
                    name: func_id.clone(),
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

    /* workaround until HashMap::get_key_value is stable */
    fn named_key_value(&self, name: &Identifier) -> Option<(&Identifier, &Named)> {
        let named = self.names.get(name)?;
        let key = self.names.keys().find(|k| *k == name).unwrap();
        Some((key, named))
    }

    fn find_named(&self, name: &Identifier) -> Option<(&Identifier, &Named)> {
        self.unit_namespace().as_ref()
            .and_then(|local_name| {
                /* local name? */
                let name_in_local_ns = local_name.append(name);
                self.named_key_value(&name_in_local_ns)
            })
            .or_else(|| {
                /* name imported from another unit? */
                let global_name = self.imported_names.get(name)?;
                self.named_key_value(&global_name)
            })
            .or_else(|| {
                /* fully-qualified global name? */
                self.named_key_value(name)
            })
    }

    pub fn get_type_alias(&self, alias: &Identifier) -> Option<Type> {
        let result = match self.find_named(alias) {
            | Some((class_name, Named::Class(_)))
            => Type::Class(class_name.clone()),

            | Some((record_name, Named::Record(_)))
            => Type::Record(record_name.clone()),

            | Some((interface_name, Named::Interface(_)))
            => Type::AnyImplementation(interface_name.clone()),

            | Some((enumeration_name, Named::Enumeration(_)))
            => Type::Enumeration(enumeration_name.clone()),

            | Some((set_name, Named::Set(_)))
            => Type::Set(set_name.clone()),

            | Some((_, Named::TypeAlias(ty)))
            => ty.clone(),

            | None
            | Some((_, Named::Const(_, _)))
            | Some((_, Named::Function(_)))
            | Some((_, Named::Symbol(_)))
            => return None,
        };

        Some(result)
    }

    pub fn get_enumeration(&self, name: &Identifier) -> Option<(&Identifier, &EnumerationDecl)> {
        match self.find_named(name) {
            Some((id, Named::Enumeration(decl))) => Some((id, decl)),
            _ => None,
        }
    }

    pub fn get_set(&self, name: &Identifier) -> Option<(&Identifier, &SetDecl)> {
        match self.find_named(name) {
            Some((id, Named::Set(decl))) => Some((id, decl)),
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

    pub fn get_function(&self, name: &Identifier) -> Option<(&Identifier, &FunctionDecl)> {
        match self.find_named(name) {
            Some((id, Named::Function(named_func))) => Some((id, &named_func.decl)),
            _ => None,
        }
    }

    pub fn get_record(&self, name: &Identifier) -> Option<(&Identifier, &RecordDecl)> {
        match self.find_named(name) {
            Some((id, Named::Record(decl))) => {
                assert_eq!(RecordKind::Record, decl.kind);
                Some((id, decl))
            }
            _ => None,
        }
    }

    pub fn get_const(&self, name: &Identifier) -> Option<(&Identifier, &ConstExpression, &Type)> {
        match self.find_named(name) {
            Some((id, Named::Const(const_expr, val_type))) => {
                Some((id, const_expr, val_type))
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
            record_type: record_id.clone(),
            member_type: member.decl_type.clone(),
            binding_kind: parent_sym.binding_kind(),
        })
    }

    pub fn get_class(&self, name: &Identifier) -> Option<(&Identifier, &RecordDecl)> {
        match self.find_named(name)? {
            (id, Named::Class(decl)) => {
                assert_eq!(RecordKind::Class, decl.kind);
                Some((id, decl))
            }
            _ => None,
        }
    }

    /**
        for a local interface name, impl type and func name, get the qualified interface name and
        func decl matching those parameters if it exists
    */
    pub fn get_interface_impl(&self,
                              of_type: &Type,
                              interface: &Identifier,
                              function: &str)
                              -> Option<(&Identifier, &FunctionDecl)> {
        let (interface_id, interface) = self.get_interface(interface)?;
        let of_type = self.full_type_name(of_type)?;

        let method_decl = interface.get_impl(&of_type, function)?;

        Some((interface_id, method_decl))
    }

    pub fn get_interface(&self, name: &Identifier) -> Option<(&Identifier, &Interface)> {
        match self.find_named(name) {
            | Some((name, Named::Interface(iface_decl))) => Some((name, iface_decl)),
            | _ => None,
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
            | Type::AnyImplementation(_)
            | Type::Nil
            | Type::RawPointer
            | Type::Pointer(_)
            | Type::Function(_)
            | Type::NativeInt
            | Type::UntypedRef
            | Type::NativeUInt
            => size_of::<usize>(),

            | Type::Int64
            | Type::UInt64
            | Type::Float64
            => 8,

            | Type::Set(id) => {
                let (_, set_decl) = self.get_set(id)
                    .expect("set type passed to size_of must exist");
                set_size(set_decl)
            }

            | Type::Enumeration(id) => {
                let (_, enum_decl) = self.get_enumeration(id)
                    .expect("enum type passed to size_of must exist");
                enum_size(enum_decl)
            }

            | Type::UInt32
            | Type::Int32
            => 4,

            | Type::Byte => 1,

            | Type::Boolean => 1,

            | Type::Record(name) => {
                let (_, record_decl) = self.get_record(name).expect("record type passed to size_of must exist");
                self.size_of_record(record_decl)
            }

            | Type::Class(name) => {
                let (_, class_decl) = self.get_class(name).expect("class type passed to size_of must exist");
                self.size_of_record(class_decl)
            }

            | Type::DynamicArray(_array) => {
                /* dynamic arrays are heap-allocated and so they *should* just
                be a single pointer... currently this is probably not true */
                size_of::<usize>()
            }

            | Type::Array(array) =>
                array.total_elements() as usize * self.size_of(&array.element),
        }
    }

    /**
        for a given type, return pairs of (interface ID, method) for all interface method
        implementations that exist for the type
    */
    pub fn get_interface_impls(&self, ty: &Type) -> Vec<(&Identifier, &FunctionDecl)> {
        let type_name = match self.full_type_name(ty) {
            Some(name) => name,
            None => return vec![],
        };

        self.names.iter()
            .filter_map(|(name, named)| {
                match named {
                    Named::Interface(iface) => Some((name, iface)),
                    _ => None
                }
            })
            .flat_map(|(iface_name, iface)| {
                iface.impls_for_type(&type_name)
                    .into_iter()
                    .map(move |impl_fn| (iface_name, impl_fn))
            })
            .collect()
    }

    pub fn type_implements(&self, ty: &Type, interface_id: &Identifier) -> bool {
        if let Type::AnyImplementation(ty_interface) = ty {
            return interface_id == ty_interface;
        }

        self.full_type_name(ty)
            .and_then(|type_name| {
                let (_, iface) = self.get_interface(interface_id)?;

                /*
                   checking that all members are implemented should be done separately,
                   so just check if we have any at all
                */
                let implemented = iface.impls_for_type(&type_name).len() > 0;
                Some(implemented)
            })
            .unwrap_or(false)
    }

    pub fn full_type_name(&self, ty: &Type) -> Option<Identifier> {
        match ty {
            | Type::Class(name) => Some(self.get_class(&name)?.0.clone()),
            | Type::Record(name) => Some(self.get_record(&name)?.0.clone()),
            | Type::Set(name) => Some(self.get_set(&name)?.0.clone()),
            | Type::Enumeration(name) => Some(self.get_enumeration(&name)?.0.clone()),

            | Type::Boolean
            | Type::Byte
            | Type::Int32
            | Type::UInt32
            | Type::Int64
            | Type::UInt64
            | Type::Float64 =>
                return Some(Identifier::from(&Type::name(Some(ty)))),

            _ =>
                return None,
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
        let mut interfaces = Vec::new();
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
                Named::Const(val, _) => consts.push((name, val)),
                Named::Enumeration(decl) => enums.push((name, decl)),
                Named::Set(decl) => sets.push((name, decl)),
                Named::Interface(iface) => interfaces.push((name, iface)),
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
            let defined = if func.defined { "defined" } else { "undefined" };
            writeln!(f, "\t\t{}: {} ({})", name, func.decl.name, defined)?;
        }
        writeln!(f, "\t]")?;

        writeln!(f, "\tclasses: [")?;
        for (name, class) in classes {
            writeln!(f, "\t\t{}: {}", name, class.name)?;
        }
        writeln!(f, "\t]")?;

        writeln!(f, "\tinterfaces: [")?;
        for (name, iface) in interfaces {
            writeln!(f, "\t\t{}: {}", name, iface.decl.name)?;

            for (method_name, method) in iface.methods.iter() {
                for (impl_ty, method_func) in method.impls_by_type.iter() {
                    writeln!(
                        f,
                        "\t\t\t* `{}` implemented for {} in {} ({})",
                        method_name,
                        impl_ty,
                        method_func.decl.scope().namespace_description(),
                        if method_func.defined { "defined" } else { "undefined" },
                    )?;
                }
            }
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
            writeln!(f, "\t\t{}: {}", name, val)?;
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