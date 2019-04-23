pub mod ns;
pub mod result;

use crate::{
    ast::{FunctionDecl, FunctionDef, Interface},
    context::NamespaceStack,
    FunctionSig, Primitive, Type,
};
use pas_common::span::*;
use pas_syn::{
    ast::{self, TypeName},
    ident::*,
};
use std::{
    borrow::Borrow,
    collections::{
        hash_map::{Entry, HashMap},
        HashSet,
    },
    fmt,
    hash::Hash,
    rc::Rc,
};

pub use self::{ns::*, result::*};

#[derive(Clone, Debug, PartialEq, Copy, Eq, Hash)]
pub enum ValueKind {
    /// local value in an immutable location
    Immutable,

    /// local value in mutable location
    Mutable,

    /// rvalue, e.g. value returned from function, result of operation,
    /// with no binding
    Temporary,

    /// reference to a mutable location somewhere else
    Ref,
}

impl fmt::Display for ValueKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ValueKind::Immutable => write!(f, "Immutable binding"),
            ValueKind::Mutable => write!(f, "Mutable binding"),
            ValueKind::Temporary => write!(f, "Temporary value"),
            ValueKind::Ref => write!(f, "Reference"),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Binding {
    pub ty: Type,
    pub kind: ValueKind,
    pub def: Option<Span>,
}

#[derive(Copy, Clone, Debug)]
pub enum InstanceMember<'a> {
    Data {
        ty: &'a Type,
    },
    Method {
        iface_ty: &'a Type,
        decl: &'a FunctionDecl,
    },
}

#[derive(Copy, Clone, Debug)]
pub enum TypeMember<'a> {
    Method { decl: &'a FunctionDecl },
}

#[derive(Clone, Debug, PartialEq)]
pub enum Decl {
    Type(Type),
    BoundValue(Binding),
    Function(Rc<FunctionSig>),
    Alias(IdentPath),
}

impl fmt::Display for Decl {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Decl::Type(ty) => write!(f, "type `{}`", ty),
            Decl::BoundValue(binding) => write!(f, "{} of `{}`", binding.kind, binding.ty),
            Decl::Function(sig) => write!(f, "{}", sig),
            Decl::Alias(aliased) => write!(f, "{}", aliased),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
struct MethodImpl {
    def: bool,
}

#[derive(Clone, Debug, PartialEq)]
struct InterfaceImpl {
    methods: HashMap<Ident, MethodImpl>,
}

impl InterfaceImpl {
    fn new() -> Self {
        Self {
            methods: HashMap::new(),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Ord, PartialOrd, Hash, Copy)]
pub struct ScopeID(usize);

#[derive(Clone, Debug, PartialEq)]
pub struct Scope {
    id: ScopeID,
    ident: Option<Ident>,
    decls: HashMap<Ident, Member<Scope>>,
    uninit: HashSet<Ident>,
}

impl Scope {
    fn new(id: ScopeID, ident: Option<Ident>) -> Self {
        Self {
            id,
            ident,
            decls: HashMap::new(),
            uninit: HashSet::new(),
        }
    }
}

impl Namespace for Scope {
    type Key = Ident;
    type Value = Decl;

    fn key(&self) -> Option<&Self::Key> {
        self.ident.as_ref()
    }

    fn keys(&self) -> Vec<Ident> {
        self.decls.keys().cloned().collect()
    }

    fn get_member<Q>(&self, member_key: &Q) -> Option<(&Ident, &Member<Self>)>
    where
        Ident: Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.decls
            .iter()
            .find(|(k, _v)| (*k).borrow() == member_key)
    }

    fn insert_member(&mut self, key: Ident, member_val: Member<Self>) -> Result<(), Ident> {
        match self.decls.entry(key.clone()) {
            Entry::Occupied(entry) => Err(entry.key().clone()),
            Entry::Vacant(entry) => {
                entry.insert(member_val);
                Ok(())
            }
        }
    }

    fn replace_member(&mut self, key: Ident, member_val: Member<Self>) {
        self.decls.insert(key, member_val);
    }
}

#[derive(Clone, Debug)]
pub struct Context {
    next_id: ScopeID,
    scopes: NamespaceStack<Scope>,

    /// iface ident -> self ty -> impl details
    iface_impls: HashMap<IdentPath, HashMap<Type, InterfaceImpl>>,

    /// decl ident -> definition location
    defs: HashMap<IdentPath, Span>,
}

pub fn builtin_span() -> Span {
    Span {
        file: Rc::new("<builtin>".into()),
        start: Location { line: 0, col: 0 },
        end: Location { line: 0, col: 0 },
    }
}

impl Context {
    pub fn root() -> Self {
        let builtin_span = builtin_span();

        let mut root_ctx = Self {
            scopes: NamespaceStack::new(Scope::new(ScopeID(0), None)),
            next_id: ScopeID(1),

            defs: HashMap::new(),
            iface_impls: HashMap::new(),
        };

        let nothing_ident = Ident::new("Nothing", builtin_span.clone());
        root_ctx.declare_type(nothing_ident, Type::Nothing).unwrap();

        let bool_ident = Ident::new(Primitive::Boolean.name(), builtin_span.clone());
        root_ctx
            .declare_type(bool_ident, Primitive::Boolean)
            .unwrap();

        let byte_ident = Ident::new(Primitive::Byte.name(), builtin_span.clone());
        root_ctx.declare_type(byte_ident, Primitive::Byte).unwrap();

        let int_ident = Ident::new(Primitive::Int32.name(), builtin_span.clone());
        root_ctx.declare_type(int_ident, Primitive::Int32).unwrap();

        let single_ident = Ident::new(Primitive::Real32.name(), builtin_span.clone());
        root_ctx
            .declare_type(single_ident, Primitive::Real32)
            .unwrap();

        // builtins are in scope 0, unit is scope 1
        root_ctx.push_scope(None);
        root_ctx
    }

    pub fn push_scope(&mut self, ns: Option<Ident>) -> ScopeID {
        let new_id = self.next_id;
        self.next_id = ScopeID(self.next_id.0 + 1);

        self.scopes.push(Scope::new(new_id, ns));
        new_id
    }

    pub fn pop_scope(&mut self, id: ScopeID) -> NamingResult<()> {
        assert_ne!(ScopeID(0), id, "can't pop the root scope");

        loop {
            let popped_id = self.scopes.current_path().top().id;

            self.scopes.pop();

            if popped_id == id {
                break Ok(());
            }
        }
    }

    pub fn find<'a>(&'a self, name: &Ident) -> Option<MemberRef<'a, Scope>> {
        match self.scopes.current_path().find(name) {
            Some(MemberRef::Value {
                value: Decl::Alias(aliased),
                ..
            }) => self.resolve(aliased),
            result => result,
        }
    }

    pub fn resolve<'a>(&'a self, path: &IdentPath) -> Option<MemberRef<'a, Scope>> {
        match self.scopes.resolve(path.as_slice()) {
            Some(MemberRef::Value {
                value: Decl::Alias(aliased),
                ..
            }) => self.resolve(aliased),
            result => result,
        }
    }

    fn declare(&mut self, name: Ident, decl: Decl) -> NamingResult<()> {
        match self.find(&name) {
            Some(old_ref) => {
                let old_ident = match old_ref {
                    MemberRef::Value {
                        key, parent_path, ..
                    } => Path::new(key.clone(), parent_path.keys().cloned()),
                    MemberRef::Namespace { path } => Path::from_parts(path.keys().cloned()),
                };
                Err(NameError::AlreadyDeclared {
                    new: name.clone(),
                    existing: old_ident,
                })
            }

            None => self
                .scopes
                .insert(name.clone(), decl)
                .map_err(|AlreadyDeclared(existing)| NameError::AlreadyDeclared {
                    existing: Path::from_parts(existing),
                    new: name,
                }),
        }
    }

    pub fn declare_binding(&mut self, name: Ident, binding: Binding) -> NamingResult<()> {
        let uninit = binding.kind == ValueKind::Mutable;

        if uninit {
            self.declare(name.clone(), Decl::BoundValue(binding))?;
            self.scopes.current_mut().uninit.insert(name);
        } else {
            self.declare(name, Decl::BoundValue(binding))?;
        }

        Ok(())
    }

    pub fn declare_type(&mut self, name: Ident, ty: impl Into<Type>) -> NamingResult<()> {
        self.declare(name, Decl::Type(ty.into()))
    }

    pub fn declare_function(&mut self, name: Ident, decl: &FunctionDecl) -> NamingResult<()> {
        let def = if decl.external_src().is_some() {
            Some(decl.span().clone())
        } else {
            None
        };

        self.declare_function_and_def(name, FunctionSig::of_decl(decl), def)
    }

    pub fn declare_alias(&mut self, name: Ident, aliased: IdentPath) -> NamingResult<()> {
        self.declare(name, Decl::Alias(aliased))
    }

    pub fn namespace_names(&self, ns_path: &IdentPath) -> NamingResult<Vec<Ident>> {
        match self.resolve(ns_path) {
            Some(MemberRef::Namespace { path }) => Ok(path.top().keys()),

            Some(MemberRef::Value { value: decl, .. }) => {
                let unexpected = UnexpectedValue::Decl(decl.clone());
                Err(NameError::ExpectedNamespace(ns_path.clone(), unexpected))
            }

            None => Err(NameError::NotFound(ns_path.last().clone())),
        }
    }

    fn method_impl_entry(
        &mut self,
        iface_ident: IdentPath,
        self_ty: Type,
        method: Ident,
    ) -> NamingResult<Entry<Ident, MethodImpl>> {
        // check the method exists
        let (_iface_path, iface) = self.find_iface(&iface_ident)?;
        if iface.get_method(&method).is_none() {
            return Err(NameError::MemberNotFound {
                span: method.span.clone(),
                base: Type::Interface(Rc::new(iface.clone())),
                member: method,
            });
        }

        let ty_impls = self
            .iface_impls
            .entry(iface_ident)
            .or_insert_with(|| HashMap::new());
        let impl_for_ty = ty_impls
            .entry(self_ty)
            .or_insert_with(|| InterfaceImpl::new());

        Ok(impl_for_ty.methods.entry(method))
    }

    pub fn declare_method_impl(
        &mut self,
        iface_ident: IdentPath,
        self_ty: Type,
        method: Ident,
    ) -> NamingResult<()> {
        self.method_impl_entry(iface_ident, self_ty, method)?
            .or_insert_with(|| MethodImpl { def: false });
        Ok(())
    }

    pub fn define_method_impl(
        &mut self,
        iface_ident: IdentPath,
        self_ty: Type,
        method: Ident,
    ) -> NamingResult<()> {
        match self.method_impl_entry(iface_ident.clone(), self_ty.clone(), method.clone())? {
            Entry::Occupied(mut entry) => {
                if entry.get().def {
                    return Err(NameError::AlreadyImplemented {
                        method,
                        for_ty: self_ty,
                        iface: iface_ident,
                        existing: entry.key().span.clone(),
                    });
                } else {
                    entry.get_mut().def = true;
                }
            }

            Entry::Vacant(entry) => {
                entry.insert(MethodImpl { def: true });
            }
        }

        Ok(())
    }

    pub fn qualify_name(&self, name: Ident) -> IdentPath {
        let parts: Vec<_> = self.scopes.current_path().keys().cloned().collect();

        if parts.is_empty() {
            IdentPath::from(name)
        } else {
            IdentPath::from_parts(parts).child(name)
        }
    }

    fn declare_function_and_def(
        &mut self,
        name: Ident,
        sig: FunctionSig,
        def: Option<Span>,
    ) -> NamingResult<()> {
        self.declare(name.clone(), Decl::Function(Rc::new(sig)))?;

        if let Some(def) = def {
            let decl_ident = self.qualify_name(name);
            self.defs.insert(decl_ident, def);
        }

        Ok(())
    }

    pub fn define_function(
        &mut self,
        name: Ident,
        sig: FunctionSig,
        def: &FunctionDef,
    ) -> NamingResult<()> {
        let decl = self.scopes.current_path().find(&name);

        match decl {
            Some(MemberRef::Value {
                value,
                key,
                parent_path,
            }) => {
                let path = Path::new(key.clone(), parent_path.keys().cloned());

                match value {
                    // a function with this name was declared but not yet defined, so we need to update
                    // the existing declaration
                    Decl::Function(old_sig) => {
                        // sig must match
                        if sig != *old_sig.as_ref() {
                            return Err(NameError::AlreadyDeclared {
                                new: name.clone(),
                                existing: path,
                            });
                        }

                        match self.defs.entry(path.clone()) {
                            // a function with this name was already declared, and it has a definition,
                            // so it's an error to redefine it
                            Entry::Occupied(entry) => Err(NameError::AlreadyDefined {
                                ident: path,
                                existing: entry.get().clone(),
                            }),

                            Entry::Vacant(entry) => {
                                entry.insert(def.span().clone());
                                Ok(())
                            }
                        }
                    }

                    other => {
                        let path = Path::new(key.clone(), parent_path.keys().cloned());
                        return Err(NameError::ExpectedFunction(path, other.clone().into()));
                    }
                }
            }

            Some(MemberRef::Namespace { path }) => {
                return Err(NameError::AlreadyDeclared {
                    new: name,
                    existing: IdentPath::from_parts(path.keys().cloned()),
                });
            }

            None => {
                // it wasn't already declared, so we need to declare AND define it. if it
                // has an external modifier, it's defined externally and so it has two definitions
                if def.decl.external_src().is_some() {
                    return Err(NameError::AlreadyDefined {
                        existing: def.decl.span().clone(),
                        ident: self.qualify_name(def.decl.ident.clone()),
                    });
                }

                self.declare_function_and_def(name, sig, Some(def.span().clone()))
            }
        }
    }

    pub fn find_type(&self, ty: &ast::TypeName) -> NamingResult<Type> {
        match ty {
            ast::TypeName::Ident { ident, indirection } => match self.resolve(ident) {
                Some(MemberRef::Value {
                    value: Decl::Type(ty),
                    ..
                }) => {
                    let ty = ty.clone().indirect_by(*indirection);
                    Ok(ty)
                }

                Some(MemberRef::Value {
                    value: unexpected, ..
                }) => Err(NameError::ExpectedType(
                    ident.clone(),
                    unexpected.clone().into(),
                )),

                Some(MemberRef::Namespace { path }) => {
                    let ns_ident = path.top().key().unwrap().clone();
                    let unexpected = UnexpectedValue::Namespace(ns_ident);
                    Err(NameError::ExpectedType(ident.clone(), unexpected))
                }

                None => Err(NameError::NotFound(ident.last().clone())),
            },

            ast::TypeName::Array { element, dim, .. } => {
                let element = self.find_type(element.as_ref())?;

                Ok(Type::Array {
                    element: Box::new(element),
                    dim: *dim,
                })
            }

            ast::TypeName::Unknown(_) => unreachable!("trying to resolve unknown type"),
        }
    }

    pub fn find_iface(&self, name: &IdentPath) -> NamingResult<(IdentPath, &Interface)> {
        match self.resolve(name) {
            Some(MemberRef::Value {
                value: Decl::Type(Type::Interface(iface)),
                key,
                ref parent_path,
                ..
            }) => {
                let parent_path = Path::new(key.clone(), parent_path.keys().cloned());
                Ok((parent_path, iface.as_ref()))
            }
            Some(MemberRef::Value { value: other, .. }) => Err(NameError::ExpectedInterface(
                name.clone(),
                other.clone().into(),
            )),
            Some(MemberRef::Namespace { path }) => {
                let unexpected = UnexpectedValue::Namespace(path.top().ident.clone().unwrap());
                Err(NameError::ExpectedInterface(name.clone(), unexpected))
            }
            None => Err(NameError::NotFound(name.last().clone())),
        }
    }

    pub fn is_iface_impl(&self, ty: &Type, iface_name: &IdentPath) -> bool {
        match self.iface_impls.get(iface_name) {
            None => false,
            Some(impls) => impls.contains_key(ty),
        }
    }

    pub fn find_function(&self, name: &IdentPath) -> NamingResult<(IdentPath, Rc<FunctionSig>)> {
        match self.resolve(name) {
            Some(MemberRef::Value {
                value: Decl::Function(sig),
                key,
                ref parent_path,
                ..
            }) => {
                let func_path = Path::new(key.clone(), parent_path.keys().cloned());
                Ok((func_path, sig.clone()))
            }
            Some(MemberRef::Value { value: other, .. }) => Err(NameError::ExpectedFunction(
                name.clone(),
                other.clone().into(),
            )),
            Some(MemberRef::Namespace { path }) => {
                let unexpected = UnexpectedValue::Namespace(path.top().ident.clone().unwrap());
                Err(NameError::ExpectedFunction(name.clone(), unexpected))
            }
            None => Err(NameError::NotFound(name.last().clone())),
        }
    }

    /// an instance method is an interface impl method for `ty` that takes Self as the first argument
    /// TODO: or any function taking `ty` as its first argument
    fn instance_methods_of<'ctx>(
        &'ctx self,
        ty: &'ctx Type,
    ) -> Vec<(&'ctx Type, &'ctx FunctionDecl)> {
        match ty {
            Type::Interface(iface) => iface
                .methods
                .iter()
                .map(|method_decl| (ty, method_decl))
                .collect(),

            _ => {
                let mut methods = Vec::new();

                for (iface_ident, iface_impls) in &self.iface_impls {
                    let iface_decl = self
                        .resolve(iface_ident)
                        .and_then(|member| member.as_value())
                        .unwrap();

                    let (iface_ty, iface) = match iface_decl {
                        Decl::Type(iface_ty @ Type::Interface(_)) => match iface_ty {
                            Type::Interface(iface) => (iface_ty, iface),
                            _ => unreachable!(),
                        },

                        _ => panic!("invalid kind of decl referenced in iface impl"),
                    };

                    if iface_impls.contains_key(ty) {
                        let iface_instance_methods = iface.methods.iter().filter(|method_decl| {
                            method_decl
                                .params
                                .get(0)
                                .map(|arg_0| arg_0.ty == Type::GenericSelf)
                                .unwrap_or(false)
                        });

                        // add all the methods, we don't need to check if they're actually defined
                        // or implemented - we should check that elsewhere
                        for method in iface_instance_methods {
                            methods.push((iface_ty, method))
                        }
                    }
                }

                methods
            }
        }
    }

    pub fn find_instance_member<'ty, 'ctx: 'ty>(
        &'ctx self,
        of_ty: &'ty Type,
        member: &Ident,
    ) -> NamingResult<InstanceMember<'ty>> {
        let data_member = of_ty.find_member(member);

        let methods = self.instance_methods_of(of_ty);
        let matching_methods: Vec<_> = methods
            .iter()
            .filter(|(_of_ty, m)| m.ident == *member)
            .map(|(of_ty, m)| (*of_ty, *m))
            .collect();

        fn ambig_paths<'a>(
            options: impl IntoIterator<Item = (&'a Type, &'a Ident)>,
        ) -> Vec<IdentPath> {
            options
                .into_iter()
                .map(|(of_ty, ident)| match of_ty.full_path() {
                    Some(base) => base.child(ident.clone()),
                    None => Path::new(ident.clone(), Vec::new()),
                })
                .collect()
        }

        match (data_member, matching_methods.len()) {
            (Some(data_member), 0) => Ok(InstanceMember::Data {
                ty: &data_member.ty,
            }),

            // unambiguous method
            (None, 1) => {
                let (iface_ty, method) = matching_methods[0];

                Ok(InstanceMember::Method {
                    iface_ty,
                    decl: method,
                })
            }

            (None, 0) => Err(NameError::MemberNotFound {
                span: member.span.clone(),
                member: member.clone(),
                base: of_ty.clone(),
            }),

            // there's a data member and 1+ methods
            (Some(data_member), _) => Err(NameError::Ambiguous {
                ident: member.clone(),
                options: ambig_paths(
                    matching_methods
                        .into_iter()
                        .map(|(of_ty, method_decl)| (of_ty, &method_decl.ident))
                        .chain(vec![(of_ty, data_member.ident)]),
                ),
            }),

            (None, _) => Err(NameError::Ambiguous {
                ident: member.clone(),
                options: ambig_paths(
                    matching_methods
                        .into_iter()
                        .map(|(of_ty, method_decl)| (of_ty, &method_decl.ident)),
                ),
            }),
        }
    }

    pub fn find_type_member<'ty>(
        &self,
        ty: &'ty Type,
        member_ident: &Ident,
    ) -> NamingResult<TypeMember<'ty>> {
        match ty {
            Type::Interface(iface) => {
                let method_decl =
                    iface
                        .get_method(member_ident)
                        .ok_or_else(|| NameError::MemberNotFound {
                            member: member_ident.clone(),
                            base: ty.clone(),
                            span: member_ident.span.clone(),
                        })?;

                Ok(TypeMember::Method { decl: method_decl })
            }

            _ => Err(NameError::MemberNotFound {
                base: ty.clone(),
                span: member_ident.span.clone(),
                member: member_ident.clone(),
            }),
        }
    }

    pub fn string_type(&self) -> NamingResult<Type> {
        let ns = IdentPath::from(Ident::new("System", builtin_span()));
        let str_class_name = TypeName::Ident {
            ident: ns.child(Ident::new("String", builtin_span())),
            indirection: 0,
        };

        self.find_type(&str_class_name)
    }

    pub fn undefined_syms(&self) -> Vec<Ident> {
        let mut syms = Vec::new();
        for scope in self.scopes.current_path().as_slice().iter().rev() {
            for (ident, decl) in scope.decls.iter() {
                // only functions can possibly be undefined
                if let Member::Value(Decl::Function(_)) = decl {
                    let decl_path = IdentPath::from_parts(scope.key().cloned())
                        .clone()
                        .child(ident.clone());

                    if self.defs.get(&decl_path).is_none() {
                        syms.push(ident.clone());
                    }
                }
            }
        }

        syms
    }

    /// Check if a local decl is marked as initialized.
    /// Panics if the decl doesn't exist or isn't a kind of decl which can be initialized.
    pub fn initialized(&self, local_id: &Ident) -> bool {
        let current_ns = self.scopes.current_path();
        let scope = current_ns.top();
        check_initialize_allowed(scope, local_id);

        !scope.uninit.contains(local_id)
    }

    /// Mark a local decl as initialized.
    /// No effect if the decl exists and is already initialized.
    /// Panics if the decl doesn't exist or isn't a kind of decl which can be initialized.
    pub fn initialize(&mut self, local_id: &Ident) {
        let scope = self.scopes.current_mut();
        check_initialize_allowed(scope, local_id);

        scope.uninit.remove(local_id);
    }

    pub fn is_local(&self, id: &Ident) -> bool {
        let current = self.scopes.current_path();
        current.top().decls.contains_key(id)
    }

    pub fn consolidate_branches(&mut self, contexts: &[Self]) {
        let scope = self.scopes.current_path();

        let mut init_in_all = Vec::new();
        for uninit_name in &scope.top().uninit {
            if contexts.iter().all(|ctx| ctx.initialized(uninit_name)) {
                init_in_all.push(uninit_name.clone());
            }
        }

        for name in init_in_all {
            self.initialize(&name);
        }
    }
}

fn check_initialize_allowed(scope: &Scope, ident: &Ident) {
    match scope.decls.get(ident) {
        Some(Member::Value(decl)) => match decl {
            Decl::BoundValue(Binding {
                kind: ValueKind::Mutable,
                ..
            }) => {
                // ok
            }

            Decl::BoundValue(Binding { kind, .. }) => {
                panic!(
                    "`{}` cannot be initialized: not mutable (was: {})",
                    ident, kind
                );
            }

            other => {
                panic!(
                    "`{}` cannot be initialized: not a binding (was: {:?})",
                    ident, other
                );
            }
        },

        Some(other) => {
            panic!(
                "`{}` cannot be initialized: not a decl (was: {:?})",
                ident, other
            );
        }

        None => {
            panic!("`{}` cannot be initialized: not found in this scope", ident);
        }
    }
}
