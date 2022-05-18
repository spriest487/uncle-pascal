pub mod name_path;
pub mod symbol;
pub mod ty;

use crate::dep_sort::sort_defs;
use linked_hash_map::LinkedHashMap;
use pas_syn::{
    self as syn,
    Ident,
    IdentPath
};
use pas_typecheck as pas_ty;
use std::{
    borrow::Cow,
    collections::hash_map::HashMap,
    fmt,
    rc::Rc
};
pub use name_path::*;
pub use symbol::*;
pub use ty::*;
use crate::InstructionFormatter;

#[derive(Eq, PartialEq, Hash, Clone, Copy, Debug, Ord, PartialOrd)]
pub struct StringID(pub usize);

impl fmt::Display for StringID {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "string literal #{}", self.0)
    }
}

#[derive(Eq, PartialEq, Hash, Clone, Copy, Debug, Ord, PartialOrd)]
pub struct TypeDefID(pub usize);

impl fmt::Display for TypeDefID {
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

pub const STRING_ID: TypeDefID = TypeDefID(1);
pub const STRING_VTYPE_ID: VirtualTypeID = VirtualTypeID::Class(STRING_ID);
pub const STRING_CHARS_FIELD: FieldID = FieldID(0);
pub const STRING_LEN_FIELD: FieldID = FieldID(1);

pub const STRING_TYPE: Type = Type::RcPointer(STRING_VTYPE_ID);

pub const DYNARRAY_LEN_FIELD: FieldID = FieldID(0);
pub const DYNARRAY_PTR_FIELD: FieldID = FieldID(1);

pub const CLOSURE_PTR_FIELD: FieldID = FieldID(0);

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub struct FunctionID(pub usize);

impl fmt::Display for FunctionID {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "function {}", self.0)
    }
}

#[derive(Debug, Clone)]
pub struct FunctionDecl {
    pub global_name: Option<Symbol>,
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
pub struct RuntimeType {
    pub release: FunctionID,
    pub retain: FunctionID,
}

#[derive(Clone, Debug)]
pub struct DynArrayRuntimeType {
    pub alloc: FunctionID,
    pub length: FunctionID,
}

#[derive(Debug, Clone, Default)]
pub struct Metadata {
    type_decls: LinkedHashMap<TypeDefID, TypeDecl>,
    string_literals: LinkedHashMap<StringID, String>,
    ifaces: LinkedHashMap<InterfaceID, InterfaceDecl>,

    dyn_array_structs: LinkedHashMap<Type, TypeDefID>,

    functions: LinkedHashMap<FunctionID, Rc<FunctionDecl>>,

    closures: Vec<TypeDefID>,

    function_types_by_sig: HashMap<pas_ty::FunctionSig, TypeDefID>,

    runtime_types: HashMap<Type, RuntimeType>,
    dyn_array_runtime_types: HashMap<Type, DynArrayRuntimeType>,
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
                    id, decl, conflict,
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
                    .map(Symbol::to_string)
                    .unwrap_or_else(|| "<unnamed>".to_string());
                let existing_name = existing
                    .global_name
                    .as_ref()
                    .map(Symbol::to_string)
                    .unwrap_or_else(|| "<unnamed>".to_string());

                panic!(
                    "duplicate function ID {} in metadata (new: {}, existing: {})",
                    id, name, existing_name
                );
            }
            self.functions.insert(*id, func_decl.clone());
        }

        for (ty, funcs) in &other.runtime_types {
            if self.runtime_types.contains_key(ty) {
                panic!("duplicate rc boilerplate definitions for type {}", ty);
            }

            self.runtime_types.insert(ty.clone(), funcs.clone());
        }

        for (sig, ty_id) in &other.function_types_by_sig {
            if !self.function_types_by_sig.contains_key(sig) {
                self.function_types_by_sig.insert(sig.clone(), *ty_id);
            }
        }

        for (el_ty, struct_id) in &other.dyn_array_structs {
            if !self.dyn_array_structs.contains_key(el_ty) {
                self.dyn_array_structs.insert(el_ty.clone(), *struct_id);
            }
        }
        for (el_ty, runtime_ty) in &other.dyn_array_runtime_types {
            if !self.dyn_array_runtime_types.contains_key(el_ty) {
                self.dyn_array_runtime_types.insert(el_ty.clone(), runtime_ty.clone());
            }
        }
    }

    pub fn type_defs(&self) -> impl Iterator<Item = (TypeDefID, &TypeDef)> {
        self.type_decls.iter().filter_map(|(id, decl)| match decl {
            TypeDecl::Def(def) => Some((*id, def)),

            TypeDecl::Reserved | TypeDecl::Forward(..) => None,
        })
    }

    pub fn get_struct_def(&self, struct_id: TypeDefID) -> Option<&Struct> {
        match self.type_decls.get(&struct_id)? {
            TypeDecl::Reserved | TypeDecl::Forward(..) => None,

            TypeDecl::Def(TypeDef::Struct(s)) => Some(s),

            TypeDecl::Def(..) => None,
        }
    }

    pub fn get_variant_def(&self, struct_id: TypeDefID) -> Option<&VariantDef> {
        match self.type_decls.get(&struct_id)? {
            TypeDecl::Reserved | TypeDecl::Forward(..) => None,

            TypeDecl::Def(TypeDef::Variant(v)) => Some(v),

            TypeDecl::Def(..) => None,
        }
    }

    pub fn get_iface_def(&self, iface_id: InterfaceID) -> Option<&Interface> {
        match self.ifaces.get(&iface_id)? {
            InterfaceDecl::Def(def) => Some(def),
            InterfaceDecl::Forward(..) => None,
        }
    }

    fn next_type_def_id(&mut self) -> TypeDefID {
        (0..)
            .map(TypeDefID)
            .find(|id| !self.type_decls.contains_key(id) && *id != STRING_ID)
            .unwrap()
    }

    fn next_iface_id(&mut self) -> InterfaceID {
        (0..)
            .map(InterfaceID)
            .find(|id| !self.ifaces.contains_key(id) && *id != DISPOSABLE_ID)
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

                let global_name = Symbol::new(name, ns);

                Some(match type_args {
                    None => global_name,
                    Some(type_args) => global_name.with_ty_args(type_args.clone()),
                })
            },
        };

        self.insert_func(global_name)
    }

    pub fn insert_func(&mut self, global_name: Option<Symbol>) -> FunctionID {
        let id = self.next_function_id();

        let decl = FunctionDecl { global_name };
        self.functions.insert(id, Rc::new(decl));

        id
    }

    pub fn declare_runtime_type(&mut self, ty: &Type) -> RuntimeType {
        if self.runtime_types.contains_key(ty) {
            panic!("duplicate rc boilerplate declaration for type {}", ty);
        }

        let pair = RuntimeType {
            retain: self.insert_func(None),
            release: self.insert_func(None),
        };

        self.runtime_types.insert(ty.clone(), pair.clone());

        pair
    }

    pub fn declare_dynarray_runtime_type(&mut self, element_ty: &Type) -> DynArrayRuntimeType {
        if self.dyn_array_runtime_types.contains_key(element_ty) {
            panic!("duplicate rc boilerplate declaration for type {}", element_ty);
        }

        let runtime_type = DynArrayRuntimeType {
            alloc: self.insert_func(None),
            length: self.insert_func(None),
        };

        self.dyn_array_runtime_types.insert(element_ty.clone(), runtime_type.clone());
        runtime_type
    }

    pub fn get_runtime_type(&self, ty: &Type) -> Option<RuntimeType> {
        self.runtime_types.get(ty).cloned()
    }

    pub fn get_dynarray_runtime_type(&self, elem_ty: &Type) -> Option<DynArrayRuntimeType> {
        self.dyn_array_runtime_types.get(elem_ty).cloned()
    }

    pub fn runtime_types(&self) -> impl Iterator<Item = (&Type, &RuntimeType)> {
        self.runtime_types.iter()
    }

    pub fn find_function(&self, name: &Symbol) -> Option<FunctionID> {
        self.functions
            .iter()
            .find(|(_id, func)| func.global_name.as_ref() == Some(name))
            .map(|(id, _func)| *id)
    }

    pub fn get_function(&self, id: FunctionID) -> Option<&Rc<FunctionDecl>> {
        self.functions.get(&id)
    }

    pub fn func_desc(&self, id: FunctionID) -> Option<String> {
        self.functions
            .get(&id)
            .and_then(|decl| decl.global_name.as_ref())
            .map(Symbol::to_string)
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
            Type::Struct(id) | Type::Variant(id) => match self.type_decls.get(id) {
                Some(TypeDecl::Forward(name)) => {
                    let pretty_name = name.to_pretty_string(|ty| self.pretty_ty_name(ty));
                    Cow::Owned(pretty_name)
                },
                Some(TypeDecl::Def(def)) => {
                    let pretty_name = def.to_pretty_str(|ty| self.pretty_ty_name(ty));
                    Cow::Owned(pretty_name)
                },
                Some(TypeDecl::Reserved) | None => Cow::Owned(id.to_string()),
            },

            Type::Array { element, dim } => {
                let elem_name = self.pretty_ty_name(element);
                Cow::Owned(format!("array [{}] of {}", dim, elem_name))
            },

            Type::RcPointer(class_id) => {
                let resource_name = match class_id {
                    VirtualTypeID::Any => Cow::Borrowed("any"),

                    VirtualTypeID::Interface(iface_id) => {
                        let iface = self.get_iface_def(*iface_id);

                        Cow::Owned(
                            iface
                                .map(|def| def.name.to_pretty_string(|ty| self.pretty_ty_name(ty)))
                                .unwrap_or_else(|| format!("<interface {}>", iface_id)),
                        )
                    },

                    VirtualTypeID::Closure(func_ty_id) => {
                        Cow::Owned(match self.get_func_ptr_ty(*func_ty_id) {
                            Some(sig) => format!("closure of {}", sig),
                            None => format!("closure of {}", *func_ty_id),
                        })
                    }

                    VirtualTypeID::Class(struct_id) => {
                        self.pretty_ty_name(&Type::Struct(*struct_id))
                    },
                };

                Cow::Owned(format!("rc[{}]", resource_name))
            },

            Type::Function(func_ty_id) => {
                Cow::Owned(match self.get_func_ptr_ty(*func_ty_id) {
                    Some(sig) => format!("{}", sig),
                    None => format!("function {}", *func_ty_id),
                })
            }

            Type::Pointer(ty) => Cow::Owned(format!("^{}", self.pretty_ty_name(ty))),

            ty => Cow::Owned(ty.to_string()),
        }
    }

    pub fn reserve_new_struct(&mut self) -> TypeDefID {
        let id = self.next_type_def_id();
        self.type_decls.insert(id, TypeDecl::Reserved);
        id
    }

    pub fn reserve_struct(&mut self, id: TypeDefID) {
        if self.type_decls.contains_key(&id) {
            panic!("reserving existing struct ID {}", id);
        }

        self.type_decls.insert(id, TypeDecl::Reserved);
    }

    // turn a reserved struct ID into a forward decl by name
    pub fn declare_struct(&mut self, id: TypeDefID, name: &NamePath) {
        match &mut self.type_decls[&id] {
            reserved @ TypeDecl::Reserved => {
                *reserved = TypeDecl::Forward(name.clone());
            },

            TypeDecl::Forward(prev_name) => {
                assert_eq!(
                    prev_name, name,
                    "can't declare same struct multiple times with different names"
                );
            },

            TypeDecl::Def(def) => {
                assert_eq!(
                    def.name(),
                    Some(name),
                    "can't declare same struct multiple times with different names"
                );
            },
        }
    }

    pub fn define_struct(&mut self, id: TypeDefID, struct_def: Struct) {
        match &self.type_decls[&id] {
            TypeDecl::Forward(name) => {
                assert_eq!(Some(name), struct_def.name());
                let type_def = TypeDecl::Def(TypeDef::Struct(struct_def));
                self.type_decls.insert(id, type_def);
            },

            TypeDecl::Reserved => {
                let type_def = TypeDecl::Def(TypeDef::Struct(struct_def));
                self.type_decls.insert(id, type_def);
            }

            _other => {
                panic!("expected named declaration to exist when defining {}", struct_def);
            },
        }
    }

    pub fn define_variant(&mut self, id: TypeDefID, variant_def: VariantDef) {
        match &mut self.type_decls[&id] {
            TypeDecl::Forward(name) => {
                assert_eq!(*name, variant_def.name);

                self.type_decls
                    .insert(id, TypeDecl::Def(TypeDef::Variant(variant_def)));
            },

            _other => {
                panic!(
                    "expected named declaration to exist when defining {}",
                    variant_def.name
                );
            },
        }
    }

    pub fn define_closure_ty(&mut self, id: TypeDefID, closure_def: Struct) {
        self.define_struct(id, closure_def);
        self.closures.push(id);
    }

    pub fn get_func_ptr_ty(&self, id: TypeDefID) -> Option<&FunctionSig> {
        self.type_decls.get(&id).and_then(|decl| match decl {
            TypeDecl::Def(TypeDef::Function(ptr_def)) => Some(ptr_def),
            _ => None,
        })
    }

    pub fn find_func_ty(&self, sig: &pas_ty::FunctionSig) -> Option<TypeDefID> {
        self.function_types_by_sig.get(&sig).cloned()
    }

    pub fn define_func_ty(
        &mut self,
        sig: pas_ty::FunctionSig,
        func_ty: FunctionSig,
    ) -> TypeDefID {
        assert!(!self.function_types_by_sig.contains_key(&sig));

        let id = self.next_type_def_id();
        self.function_types_by_sig.insert(sig, id);

        let replaced = self
            .type_decls
            .insert(id, TypeDecl::Def(TypeDef::Function(func_ty)));
        assert!(replaced.is_none());

        id
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
            self.ifaces
                .insert(DISPOSABLE_ID, InterfaceDecl::Forward(disposable_name));
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
            },

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
        let iface = self.get_iface_def(iface_id)?;
        let ty_impl = iface.impls.get(ty)?;

        ty_impl.methods.get(&method).cloned()
    }

    pub fn impls(&self, ty: &Type) -> Vec<InterfaceID> {
        self.ifaces
            .iter()
            .filter_map(|(id, _decl)| {
                if self.is_impl(ty, *id) {
                    Some(*id)
                } else {
                    None
                }
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

                Type::RcPointer(VirtualTypeID::Interface(iface_id))
            },

            pas_ty::Type::Primitive(pas_ty::Primitive::Boolean) => Type::Bool,

            pas_ty::Type::Primitive(pas_ty::Primitive::Int8) => Type::I8,
            pas_ty::Type::Primitive(pas_ty::Primitive::UInt8) => Type::U8,
            pas_ty::Type::Primitive(pas_ty::Primitive::Int16) => Type::I16,
            pas_ty::Type::Primitive(pas_ty::Primitive::UInt16) => Type::U16,
            pas_ty::Type::Primitive(pas_ty::Primitive::Int32) => Type::I32,
            pas_ty::Type::Primitive(pas_ty::Primitive::UInt32) => Type::U32,
            pas_ty::Type::Primitive(pas_ty::Primitive::Int64) => Type::I64,
            pas_ty::Type::Primitive(pas_ty::Primitive::UInt64) => Type::U64,
            pas_ty::Type::Primitive(pas_ty::Primitive::NativeInt) => Type::ISize,
            pas_ty::Type::Primitive(pas_ty::Primitive::NativeUInt) => Type::USize,

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
                        let class_id = VirtualTypeID::Class(struct_id);
                        Type::RcPointer(class_id)
                    },

                    pas_ty::Type::Record(..) => Type::Struct(struct_id),

                    _ => unreachable!(),
                }
            },

            pas_ty::Type::Array(array_ty) => {
                let element = self.find_type(&array_ty.element_ty);
                Type::Array {
                    element: Rc::new(element),
                    dim: array_ty.dim,
                }
            },

            pas_ty::Type::DynArray { element } => {
                let element = self.find_type(element.as_ref());

                let array_struct = match self.find_dyn_array_struct(&element) {
                    Some(id) => id,
                    None => panic!(
                        "missing dyn array IR struct definition for element type {}",
                        element
                    ),
                };

                Type::RcPointer(VirtualTypeID::Class(array_struct))
            },

            pas_ty::Type::Variant(variant) => {
                expect_no_generic_args(&variant, variant.type_args.as_ref());

                let ty_name = NamePath::from_decl(*variant.clone(), self);

                match self.find_type_decl(&ty_name) {
                    Some(id) => Type::Variant(id),
                    None => panic!("missing IR struct metadata for variant {}", variant),
                }
            },

            pas_ty::Type::MethodSelf => panic!("Self is not a real type in this context"),

            pas_ty::Type::GenericParam(param) => panic!(
                "{} is not a real type in this context: {:?}",
                param,
                pas_common::Backtrace::new()
            ),

            pas_ty::Type::Function(sig) => match self.function_types_by_sig.get(sig) {
                Some(id) => Type::Function(*id),
                None => panic!("no type definition for function with sig {}", sig),
            },

            pas_ty::Type::Any => Type::RcPointer(VirtualTypeID::Any),
        }
    }

    pub fn find_dyn_array_struct(&self, element: &Type) -> Option<TypeDefID> {
        self.dyn_array_structs.get(element).cloned()
    }

    pub fn define_dyn_array_struct(&mut self, element: Type) -> TypeDefID {
        assert!(
            !self.dyn_array_structs.contains_key(&element),
            "duplicate IR struct definition for dynamic array with element {}",
            element
        );

        let mut fields = LinkedHashMap::new();
        fields.insert(
            DYNARRAY_LEN_FIELD,
            StructFieldDef {
                name: Some("len".to_string()),
                ty: Type::I32,
                rc: false,
            },
        );
        fields.insert(
            DYNARRAY_PTR_FIELD,
            StructFieldDef {
                name: Some("ptr".to_string()),
                ty: element.clone().ptr(),
                rc: false,
            },
        );

        let struct_id = self.next_type_def_id();
        self.type_decls.insert(
            struct_id,
            TypeDecl::Def(TypeDef::Struct(Struct {
                identity: StructIdentity::DynArray(element.clone()),
                fields,
                src_span: None,
            })),
        );

        self.dyn_array_structs.insert(element.clone(), struct_id);

        // the rc boilerplate impls for a dynarray should be empty
        // dyn array structs are heap-allocated and don't need structural ref-counting
        // (but they do need custom finalization to clean up references they hold)
        self.declare_runtime_type(&Type::Struct(struct_id));
        self.declare_dynarray_runtime_type(&element);

        // we know it will have a disposer impl (and trust that the module
        // will generate the code for it if we give it an ID here)
        //        let disposer_id = self.next_function_id();
        //        self.functions
        //            .insert(disposer_id, FunctionDecl { global_name: None });

        //        let array_ref_ty = Type::RcPointer(Some(ClassID::Class(struct_id)));
        //        self.impl_method(DISPOSABLE_ID, array_ref_ty, "Dispose", disposer_id);

        struct_id
    }

    pub fn dyn_array_structs(&self) -> &LinkedHashMap<Type, TypeDefID> {
        &self.dyn_array_structs
    }

    pub fn dyn_array_element_ty(&self, array_class_id: TypeDefID) -> Option<&Type> {
        let (el_ty, _) = self
            .dyn_array_structs()
            .iter()
            .filter(|(_el_ty, struct_id)| array_class_id == **struct_id)
            .next()?;

        Some(el_ty)
    }

    pub fn closures(&self) -> &[TypeDefID] {
        &self.closures
    }

    pub fn find_type_decl(&self, name: &NamePath) -> Option<TypeDefID> {
        self.type_decls.iter().find_map(|(id, def)| match def {
            TypeDecl::Def(TypeDef::Struct(struct_def)) if struct_def.name() == Some(name) =>
            {
                Some(*id)
            },

            TypeDecl::Def(TypeDef::Variant(variant_def)) if variant_def.name == *name => Some(*id),

            TypeDecl::Forward(forward_name) if *forward_name == *name => Some(*id),

            _ => None,
        })
    }

    // find the declared ID and definition of a struct. if the struct is only forward-declared
    // when this call is made, the definition part of the result will be None
    pub fn find_struct_def(&self, name: &NamePath) -> Option<(TypeDefID, &Struct)> {
        self.type_decls.iter().find_map(|(id, def)| match def {
            TypeDecl::Def(TypeDef::Struct(struct_def)) if struct_def.name() == Some(name) =>
            {
                Some((*id, struct_def))
            },

            _ => None,
        })
    }

    pub fn find_variant_def(&self, name: &NamePath) -> Option<(TypeDefID, &VariantDef)> {
        self.type_decls.iter().find_map(|(id, def)| match def {
            TypeDecl::Def(TypeDef::Variant(variant_def)) if variant_def.name == *name => {
                Some((*id, variant_def))
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
                TypeDecl::Reserved => {
                    decls.insert(id, TypeDecl::Reserved);
                },
                TypeDecl::Forward(name) => {
                    decls.insert(id, TypeDecl::Forward(name));
                },

                TypeDecl::Def(def) => {
                    defs.push((id, def));
                },
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
