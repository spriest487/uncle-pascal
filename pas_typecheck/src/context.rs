pub mod ns;
pub mod result;

use crate::{
    ast::{
        FunctionDecl,
        FunctionDef,
        Interface,
    },
    context::NamespaceStack,
    FunctionSig,
    Primitive,
    QualifiedDeclName,
    Type,
};
use pas_common::span::*;
use pas_syn::{
    ast::{self, Visibility},
    ident::*,
};
use std::{
    borrow::Borrow,
    collections::hash_map::{
        Entry,
        HashMap,
    },
    fmt,
    hash::Hash,
    rc::Rc,
};

pub use self::{
    ns::*,
    result::*,
};

#[derive(Clone, Debug, PartialEq, Copy, Eq, Hash)]
pub enum ValueKind {
    /// local value in an immutable location
    Immutable,

    /// uninitialized mutable location
    Uninitialized,

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
            ValueKind::Uninitialized => write!(f, "Uninitialized binding"),
            ValueKind::Immutable => write!(f, "Immutable binding"),
            ValueKind::Mutable => write!(f, "Mutable binding"),
            ValueKind::Temporary => write!(f, "Temporary value"),
            ValueKind::Ref => write!(f, "Reference"),
        }
    }
}

impl ValueKind {
    pub fn mutable(self) -> bool {
        match self {
            ValueKind::Mutable | ValueKind::Uninitialized => true,
            _ => false,
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
    Type {
        ty: Type,
        visibility: Visibility,
    },
    BoundValue(Binding),
    Function {
        sig: Rc<FunctionSig>,
        visibility: Visibility,
    },
    Alias(IdentPath),
}

impl fmt::Display for Decl {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Decl::Type { ty, .. } => write!(f, "type `{}`", ty),
            Decl::BoundValue(binding) => write!(f, "{} of `{}`", binding.kind, binding.ty),
            Decl::Function { sig, .. } => write!(f, "{}", sig),
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
}

impl Scope {
    fn new(id: ScopeID, ident: Option<Ident>) -> Self {
        Self {
            id,
            ident,
            decls: HashMap::new(),
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
            },
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

    loop_stack: Vec<Span>,
    /* /// current type decl, if we're inside one
     * decl_scope: Option<TypeDeclName>, */
}

pub fn builtin_span() -> Span {
    Span {
        file: Rc::new("<builtin>".into()),
        start: Location { line: 0, col: 0 },
        end: Location { line: 0, col: 0 },
    }
}

impl Context {
    pub fn root(no_stdlib: bool) -> Self {
        let builtin_span = builtin_span();

        let mut root_ctx = Self {
            scopes: NamespaceStack::new(Scope::new(ScopeID(0), None)),
            next_id: ScopeID(1),

            defs: HashMap::new(),
            iface_impls: HashMap::new(),

            loop_stack: Vec::new(),
            //            decl_scope: None,
        };

        let declare_builtin = |ctx: &mut Self, name: &str, ty: Type| {
            let ident = Ident::new(name, builtin_span.clone());
            ctx.declare_type(ident, ty, Visibility::Exported)
                .expect("builtin type decl must not fail");
        };

        declare_builtin(&mut root_ctx, "Nothing", Type::Nothing);
        declare_builtin(&mut root_ctx, "Any", Type::Any);

        let primitives = [
            Primitive::Boolean,
            Primitive::Byte,
            Primitive::Int32,
            Primitive::Real32,
        ];
        for primitive in &primitives {
            declare_builtin(&mut root_ctx, primitive.name(), Type::Primitive(*primitive));
        }

        // builtins are in scope 0, unit is scope 1
        root_ctx.push_scope(None);

        if no_stdlib {
            // the declaration of Disposable needs to be present even for --no-stdlib builds
            // or destructors won't work

            let system_ident = Ident::new("System", builtin_span.clone());
            let disposable_ident = Ident::new("Disposable", builtin_span.clone());

            let disposable_name = QualifiedDeclName {
                qualified: root_ctx.qualify_name(disposable_ident.clone()),
                decl_name: ast::TypeDeclName {
                    ident: disposable_ident.clone(),
                    span: builtin_span.clone(),
                    type_params: Vec::new(),
                },
                type_args: Vec::new(),
            };

            let disposable_iface = Interface {
                name: disposable_name,
                methods: vec![FunctionDecl {
                    ident: Ident::new("Dispose", builtin_span.clone()).into(),
                    return_ty: None,
                    impl_iface: None,
                    mods: Vec::new(),
                    params: vec![ast::FunctionParam {
                        ident: Ident::new("self", builtin_span.clone()),
                        ty: Type::GenericSelf,
                        modifier: None,
                        span: builtin_span.clone(),
                    }],
                    span: builtin_span.clone(),
                }],
                span: builtin_span,
            };
            let disposable_ty = Type::Interface(Rc::new(disposable_iface));

            let system_scope = root_ctx.push_scope(Some(system_ident));

            root_ctx
                .declare_type(disposable_ident, disposable_ty, Visibility::Exported)
                .expect("builtin System.Disposable type decl must not fail");

            root_ctx.pop_scope(system_scope);
        }

        root_ctx
    }

    pub fn push_scope(&mut self, ns: Option<Ident>) -> ScopeID {
        let new_id = self.next_id;
        self.next_id = ScopeID(self.next_id.0 + 1);

        self.scopes.push(Scope::new(new_id, ns));
        new_id
    }

    pub fn pop_scope(&mut self, id: ScopeID) {
        assert_ne!(ScopeID(0), id, "can't pop the root scope");

        loop {
            let popped_id = self.scopes.current_path().top().id;

            self.scopes.pop();

            if popped_id == id {
                break;
            }
        }
    }

    pub fn push_loop(&mut self, at: Span) {
        self.loop_stack.push(at);
    }

    pub fn pop_loop(&mut self) -> Span {
        self.loop_stack
            .pop()
            .expect("can't pop loop stack when not in a loop")
    }

    pub fn in_loop(&self) -> Option<&Span> {
        self.loop_stack.last()
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

    pub fn find_decl(&self, name: &Ident) -> Option<&Ident> {
        match self.find(name)? {
            MemberRef::Value { key, .. } => Some(key),
            _ => None,
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
            },

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
        self.declare(name, Decl::BoundValue(binding))?;
        Ok(())
    }

    pub fn declare_type(
        &mut self,
        name: Ident,
        ty: Type,
        visibility: Visibility
    ) -> NamingResult<()> {
        self.declare(name, Decl::Type { ty, visibility })?;
        Ok(())
    }

    pub fn declare_function(
        &mut self,name: Ident,
        decl: &FunctionDecl,
        visibility: Visibility
    ) -> NamingResult<()> {
        let def = if decl.external_src().is_some() {
            Some(decl.span().clone())
        } else {
            None
        };

        self.declare_function_and_def(name, FunctionSig::of_decl(decl), def, visibility)
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
            },

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
            .or_insert_with(HashMap::new);
        let impl_for_ty = ty_impls.entry(self_ty).or_insert_with(InterfaceImpl::new);

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
        iface: Rc<Interface>,
        self_ty: Type,
        method: Ident,
    ) -> NamingResult<()> {
        match self.method_impl_entry(
            iface.name.qualified.clone(),
            self_ty.clone(),
            method.clone(),
        )? {
            Entry::Occupied(mut entry) => {
                if entry.get().def {
                    return Err(NameError::AlreadyImplemented {
                        method,
                        for_ty: self_ty,
                        iface: Box::new(iface.as_ref().clone()),
                        existing: entry.key().span.clone(),
                    });
                } else {
                    entry.get_mut().def = true;
                }
            },

            Entry::Vacant(entry) => {
                entry.insert(MethodImpl { def: true });
            },
        }

        Ok(())
    }

    pub fn namespace(&self) -> IdentPath {
        IdentPath::from_parts(self.scopes.current_path().keys().cloned())
    }

    pub fn qualify_name(&self, name: Ident) -> IdentPath {
        self.namespace().child(name)
    }

    fn declare_function_and_def(
        &mut self,
        name: Ident,
        sig: FunctionSig,
        def: Option<Span>,
        visibility: Visibility,
    ) -> NamingResult<()> {
        self.declare(name.clone(), Decl::Function {
            sig: Rc::new(sig),
            visibility,
        })?;

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
        visibility: Visibility,
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
                    Decl::Function { sig: old_sig, .. } => {
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
                                entry.insert(def.decl.span().clone());
                                Ok(())
                            },
                        }
                    },

                    other => {
                        let path = Path::new(key.clone(), parent_path.keys().cloned());
                        Err(NameError::ExpectedFunction(path, other.clone().into()))
                    },
                }
            },

            Some(MemberRef::Namespace { path }) => Err(NameError::AlreadyDeclared {
                new: name,
                existing: IdentPath::from_parts(path.keys().cloned()),
            }),

            None => {
                // it wasn't already declared, so we need to declare AND define it. if it
                // has an external modifier, it's defined externally and so it has two definitions
                if def.decl.external_src().is_some() {
                    Err(NameError::AlreadyDefined {
                        existing: def.decl.span().clone(),
                        ident: def.decl.ident.clone(),
                    })
                } else {
                    self.declare_function_and_def(
                        name,
                        sig,
                        Some(def.decl.span().clone()),
                        visibility
                    )
                }
            },
        }
    }

    pub fn find_iface(&self, name: &IdentPath) -> NamingResult<(IdentPath, &Interface)> {
        match self.resolve(name) {
            Some(MemberRef::Value {
                value: Decl::Type { ty: Type::Interface(iface), .. },
                key,
                ref parent_path,
                ..
            }) => {
                let parent_path = Path::new(key.clone(), parent_path.keys().cloned());
                Ok((parent_path, iface.as_ref()))
            },

            Some(MemberRef::Value { value: other, .. }) => Err(NameError::ExpectedInterface(
                name.clone(),
                other.clone().into(),
            )),

            Some(MemberRef::Namespace { path }) => {
                let unexpected = UnexpectedValue::Namespace(path.top().ident.clone().unwrap());
                Err(NameError::ExpectedInterface(name.clone(), unexpected))
            },

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
                value: Decl::Function { sig, .. },
                key,
                ref parent_path,
                ..
            }) => {
                let func_path = Path::new(key.clone(), parent_path.keys().cloned());
                Ok((func_path, sig.clone()))
            },
            Some(MemberRef::Value { value: other, .. }) => Err(NameError::ExpectedFunction(
                name.clone(),
                other.clone().into(),
            )),
            Some(MemberRef::Namespace { path }) => {
                let unexpected = UnexpectedValue::Namespace(path.top().ident.clone().unwrap());
                Err(NameError::ExpectedFunction(name.clone(), unexpected))
            },
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
                        Decl::Type { ty: iface_ty @ Type::Interface(..), .. } => match iface_ty {
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
            },
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
            .filter(|(_of_ty, m)| *m.ident.single() == *member)
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
            },

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
                        .map(|(of_ty, method_decl)| (of_ty, method_decl.ident.single()))
                        .chain(vec![(of_ty, data_member.ident)]),
                ),
            }),

            (None, _) => Err(NameError::Ambiguous {
                ident: member.clone(),
                options: ambig_paths(
                    matching_methods
                        .into_iter()
                        .map(|(of_ty, method_decl)| (of_ty, method_decl.ident.single())),
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
            },

            _ => Err(NameError::MemberNotFound {
                base: ty.clone(),
                span: member_ident.span.clone(),
                member: member_ident.clone(),
            }),
        }
    }

    pub fn undefined_syms(&self) -> Vec<Ident> {
        let mut syms = Vec::new();
        for scope in self.scopes.current_path().as_slice().iter().rev() {
            for (ident, decl) in scope.decls.iter() {
                // only functions can possibly be undefined
                if let Member::Value(Decl::Function { .. }) = decl {
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

    /// Mark a local decl as initialized.
    /// No effect if the decl exists and is already initialized.
    /// Panics if the decl doesn't exist or isn't a kind of decl which can be initialized.
    pub fn initialize(&mut self, local_id: &Ident) {
        let scope = self.scopes.current_mut();
        check_initialize_allowed(scope, local_id);

        match scope.decls.get_mut(local_id) {
            Some(Member::Value(Decl::BoundValue(Binding { kind, .. }))) => {
                if *kind == ValueKind::Uninitialized || *kind == ValueKind::Mutable {
                    *kind = ValueKind::Mutable;
                } else {
                    panic!("{} does not refer to a mutable binding", local_id);
                }
            },
            _ => panic!("{} does not refer to a mutable binding", local_id),
        }
    }

    pub fn is_local(&self, id: &Ident) -> bool {
        let current = self.scopes.current_path();
        current.top().decls.contains_key(id)
    }

    pub fn consolidate_branches(&mut self, branch_contexts: &[Self]) {
        let scope = self.scopes.current_path();

        let uninit_names: Vec<_> = scope
            .top()
            .decls
            .iter()
            .filter_map(|(ident, decl)| match decl {
                Member::Value(Decl::BoundValue(Binding {
                    kind: ValueKind::Uninitialized,
                    ..
                })) => Some(ident),
                _ => None,
            })
            .collect();
        let this_depth = scope.as_slice().len();

        // names initialized in all branches
        let mut all_init = Vec::new();
        for uninit_name in uninit_names {
            let is_init_in_all =
                branch_contexts
                    .iter()
                    .all(|ctx| match ctx.find(uninit_name).unwrap() {
                        MemberRef::Value {
                            value, parent_path, ..
                        } => match value {
                            Decl::BoundValue(binding) => {
                                parent_path.as_slice().len() == this_depth
                                    && binding.kind == ValueKind::Mutable
                            },

                            _ => false,
                        },

                        MemberRef::Namespace { .. } => false,
                    });

            if is_init_in_all {
                all_init.push(uninit_name.clone());
            }
        }

        for name in all_init {
            self.initialize(&name);
        }
    }

    pub fn is_accessible(&self, name: &IdentPath) -> bool {
        match self.resolve(name) {
            Some(MemberRef::Value { parent_path, value, .. }) => {
                match value {
                    Decl::Type { visibility, .. } | Decl::Function { visibility, .. } => {
                        match visibility {
                            Visibility::Exported => true,
                            Visibility::Private => {
                                let decl_unit_ns = IdentPath::from_parts(parent_path.keys().cloned());
                                self.namespace().is_parent_of(&decl_unit_ns)
                            },
                        }
                    },

                    _ => true,
                }
            },

            _ => true,
        }
    }

    pub fn is_constructor_accessible(&self, ty: &Type) -> bool {
        match ty {
            Type::Class(class) => self.namespace().is_parent_of(&class.name.qualified),
            _ => true,
        }
    }
}

fn check_initialize_allowed(scope: &Scope, ident: &Ident) {
    match scope.decls.get(ident) {
        Some(Member::Value(decl)) => match decl {
            Decl::BoundValue(Binding { kind, .. }) => {
                if !kind.mutable() {
                    panic!(
                        "`{}` cannot be initialized: not mutable (was: {})",
                        ident, kind
                    );
                }
            },

            other => {
                panic!(
                    "`{}` cannot be initialized: not a binding (was: {:?})",
                    ident, other
                );
            },
        },

        Some(other) => {
            panic!(
                "`{}` cannot be initialized: not a decl (was: {:?})",
                ident, other
            );
        },

        None => {
            panic!("`{}` cannot be initialized: not found in this scope", ident);
        },
    }
}
