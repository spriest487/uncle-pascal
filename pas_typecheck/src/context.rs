use std::{
    borrow::Borrow,
    collections::hash_map::{Entry, HashMap},
    fmt,
    hash::Hash,
    rc::Rc,
};

use pas_common::span::*;
use pas_syn::{ast::Visibility, ident::*};

use crate::{
    ast::{Class, FunctionDecl, FunctionDef, Interface, OverloadCandidate, Variant},
    context::NamespaceStack,
    specialize_class_def, specialize_generic_variant, FunctionSig, Primitive, Symbol, Type,
    TypeParamList, TypeParamType,
};

pub use self::{builtin::*, ns::*, result::*, ufcs::InstanceMethod};

pub mod builtin;
pub mod ns;
pub mod result;

mod ufcs;

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

#[derive(Clone, Debug)]
pub enum InstanceMember {
    Data {
        ty: Type,
    },
    Method {
        iface_ty: Type,
        method: Ident,
    },
    UFCSCall {
        func_name: IdentPath,
        sig: Rc<FunctionSig>,
    },
    Overloaded {
        candidates: Vec<OverloadCandidate>,
    },
}

#[derive(Clone, Debug)]
pub enum TypeMember {
    Method { decl: FunctionDecl },
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
struct InterfaceImpl {
    methods: HashMap<Ident, Option<FunctionDef>>,
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
    env: Environment,
    decls: HashMap<Ident, Member<Scope>>,
}

impl Scope {
    fn new(id: ScopeID, env: Environment) -> Self {
        Self {
            id,
            env,
            decls: HashMap::new(),
        }
    }
}

impl Namespace for Scope {
    type Key = Ident;
    type Value = Decl;

    fn key(&self) -> Option<&Self::Key> {
        match &self.env {
            Environment::Module { namespace } => Some(namespace),
            _ => None,
        }
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
pub enum Def {
    External(FunctionDecl),
    Function(FunctionDef),
    Class(Rc<Class>),
    Interface(Rc<Interface>),
    Variant(Rc<Variant>),
}

impl Def {
    fn ident(&self) -> &Ident {
        match self {
            Def::External(func_decl) => func_decl.ident.last(),
            Def::Function(func_def) => func_def.decl.ident.last(),
            Def::Class(class) => &class.name.decl_name.ident,
            Def::Interface(iface) => &iface.name.decl_name.ident,
            Def::Variant(variant) => &variant.name.decl_name.ident,
        }
    }
}

// result of comparing a defined name with its previous decl
enum DefDeclMatch {
    // the def matches the decl
    Match,

    // the def is the right kind but doesn't match, e.g. function with wrong signature
    Mismatch,

    // the decl with the same name as this def is the wrong kind
    WrongKind,
}

impl DefDeclMatch {
    pub fn always_match(_: &Decl) -> DefDeclMatch {
        DefDeclMatch::Match
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Environment {
    Global,
    Module { namespace: Ident },
    TypeDecl,
    FunctionDecl,
    FunctionBody { result_ty: Type },
    Block,
}

impl Environment {
    pub fn namespace(&self) -> Option<&Ident> {
        match self {
            Environment::Module { namespace } => Some(namespace),
            _ => None,
        }
    }
}

#[derive(Clone, Debug)]
pub struct Context {
    next_id: ScopeID,
    scopes: NamespaceStack<Scope>,

    /// iface ident -> self ty -> impl details
    iface_impls: HashMap<IdentPath, HashMap<Type, InterfaceImpl>>,

    /// decl ident -> definition location
    defs: HashMap<IdentPath, Def>,

    loop_stack: Vec<Span>,
}

impl Context {
    pub fn root(no_stdlib: bool) -> Self {
        let builtin_span = builtin_span();

        let mut root_ctx = Self {
            scopes: NamespaceStack::new(Scope::new(ScopeID(0), Environment::Global)),
            next_id: ScopeID(1),

            defs: Default::default(),

            iface_impls: Default::default(),

            loop_stack: Default::default(),
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
            Primitive::Pointer,
        ];
        for primitive in &primitives {
            declare_builtin(&mut root_ctx, primitive.name(), Type::Primitive(*primitive));
        }

        // builtins are in scope 0, unit is scope 1
        root_ctx.push_scope(Environment::Global);

        if no_stdlib {
            // declare things normally declared in System.pas that the compiler needs to function
            let disposable_ty = Type::Interface(builtin_disposable_name().qualified);
            let disposable_name = disposable_ty.full_path().unwrap();

            let string_ty = Type::Class(Box::new(builtin_string_name()));
            let string_name = string_ty.full_path().unwrap();

            let unit_env = Environment::Module {
                namespace: Ident::new("System", builtin_span),
            };
            let system_scope = root_ctx.push_scope(unit_env);

            root_ctx
                .declare_type(
                    disposable_name.last().clone(),
                    disposable_ty,
                    Visibility::Exported,
                )
                .expect("builtin System.Disposable type decl must not fail");

            root_ctx
                .declare_type(string_name.last().clone(), string_ty, Visibility::Exported)
                .expect("builtin System.String type decl must not fail");

            root_ctx.pop_scope(system_scope);
        }

        root_ctx
    }

    pub fn push_scope(&mut self, env: Environment) -> ScopeID {
        let new_id = self.next_id;
        self.next_id = ScopeID(self.next_id.0 + 1);

        self.scopes.push(Scope::new(new_id, env));
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

    pub fn find(&self, name: &Ident) -> Option<MemberRef<Scope>> {
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

    pub fn resolve(&self, path: &IdentPath) -> Option<MemberRef<Scope>> {
        match self.scopes.resolve(path.as_slice()) {
            Some(MemberRef::Value {
                value: Decl::Alias(aliased),
                ..
            }) => self.resolve(aliased),
            result => result,
        }
    }

    pub fn current_func_return_ty(&self) -> Option<&Type> {
        for scope in self.scopes.iter_up() {
            if let Environment::FunctionBody { result_ty } = &scope.env {
                return Some(result_ty);
            }
        }

        None
    }

    fn declare(&mut self, name: Ident, decl: Decl) -> NamingResult<()> {
        match self.find(&name) {
            Some(MemberRef::Value {
                value: Decl::Alias(aliased),
                key,
                ref parent_path,
            }) => {
                match &decl {
                    // new decl aliases the same type and is OK
                    Decl::Alias(new_aliased) if *new_aliased == *aliased => {
                        // don't need to redeclare it, it's already the same alias
                        Ok(())
                    }

                    // new name replaces an existing alias and doesn't match the old alias
                    _ => {
                        let old_ident =
                            IdentPath::from_parts(parent_path.keys().cloned()).child(key.clone());

                        Err(NameError::AlreadyDeclared {
                            new: name.clone(),
                            existing: old_ident,
                        })
                    }
                }
            }

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
        self.declare(name, Decl::BoundValue(binding))?;
        Ok(())
    }

    pub fn declare_iface(
        &mut self,
        iface: Rc<Interface>,
        visibility: Visibility,
    ) -> NamingResult<()> {
        let name = iface.name.decl_name.ident.clone();
        let iface_ty = Type::Interface(iface.name.qualified.clone());
        self.declare_type(name.clone(), iface_ty, visibility)?;

        let map_unexpected = |_, _| unreachable!();
        self.define(
            name,
            Def::Interface(iface.clone()),
            DefDeclMatch::always_match,
            map_unexpected,
        )?;

        Ok(())
    }

    pub fn declare_variant(
        &mut self,
        variant: Rc<Variant>,
        visibility: Visibility,
    ) -> NamingResult<()> {
        let name = variant.name.decl_name.ident.clone();

        let variant_ty = Type::Variant(Box::new(variant.name.clone()));
        self.declare_type(name.clone(), variant_ty, visibility)?;

        let map_unexpected = |_, _| unreachable!();
        self.define(
            name,
            Def::Variant(variant.clone()),
            DefDeclMatch::always_match,
            map_unexpected,
        )?;

        Ok(())
    }

    pub fn declare_class(&mut self, class: Rc<Class>, visibility: Visibility) -> NamingResult<()> {
        let name = class.name.decl_name.ident.clone();

        let class_ty = match class.kind {
            pas_syn::ast::ClassKind::Object => Type::Class(Box::new(class.name.clone())),
            pas_syn::ast::ClassKind::Record => Type::Record(Box::new(class.name.clone())),
        };

        self.declare_type(name.clone(), class_ty, visibility)?;

        let map_unexpected = |_, _| unreachable!();
        self.define(
            name,
            Def::Class(class.clone()),
            DefDeclMatch::always_match,
            map_unexpected,
        )?;

        Ok(())
    }

    /// declare the type params of a function in the local scope
    pub fn declare_type_params(&mut self, names: &TypeParamList) -> NamingResult<()> {
        for (pos, param) in names.items.iter().enumerate() {
            let is_iface = param
                .constraint
                .as_ref()
                .map(|c| c.is_ty.clone())
                .map(Box::new);

            self.declare_type(
                param.ident.clone(),
                Type::GenericParam(Box::new(TypeParamType {
                    name: param.ident.clone(),
                    is_iface,
                    pos,
                })),
                Visibility::Private,
            )?;
        }

        Ok(())
    }

    pub fn declare_self_ty(&mut self, ty: Type, span: Span) -> NamingResult<()> {
        let self_ident = Ident::new("Self", span);
        self.declare_type(self_ident, ty, Visibility::Private)
    }

    pub fn declare_type(
        &mut self,
        name: Ident,
        ty: Type,
        visibility: Visibility,
    ) -> NamingResult<()> {
        self.declare(name, Decl::Type { ty, visibility })?;
        Ok(())
    }

    pub fn declare_function(
        &mut self,
        name: Ident,
        func_decl: &FunctionDecl,
        visibility: Visibility,
    ) -> NamingResult<()> {
        let decl = Decl::Function {
            sig: FunctionSig::of_decl(func_decl).into(),
            visibility,
        };

        self.declare(name.clone(), decl)?;

        if func_decl.external_src().is_some() {
            let def = Def::External(func_decl.clone());
            self.define(name, def, |_| DefDeclMatch::Match, |_, _| unreachable!())?;
        }

        Ok(())
    }

    pub fn declare_alias(&mut self, name: Ident, aliased: IdentPath) -> NamingResult<()> {
        self.declare(name, Decl::Alias(aliased))
    }

    pub fn resolve_alias(&self, path: &IdentPath) -> Option<IdentPath> {
        let member = self.resolve(path)?;

        match member {
            MemberRef::Value {
                value,
                parent_path,
                key,
            } => match value {
                Decl::Alias(aliased) => self.resolve_alias(aliased),
                _ => Some(IdentPath::new(key.clone(), parent_path.keys().cloned())),
            },

            _ => None,
        }
    }

    pub fn namespace_names(&self, ns_path: &IdentPath) -> NamingResult<Vec<Ident>> {
        match self.resolve(ns_path) {
            Some(MemberRef::Namespace { path }) => Ok(path.top().keys()),

            Some(MemberRef::Value { value: decl, .. }) => {
                let unexpected = UnexpectedValue::Decl(decl.clone());
                Err(NameError::Unexpected {
                    ident: ns_path.clone(),
                    expected: ExpectedKind::Namespace,
                    actual: unexpected,
                })
            }

            None => Err(NameError::NotFound(ns_path.last().clone())),
        }
    }

    fn method_impl_entry(
        &mut self,
        iface_ident: IdentPath,
        self_ty: Type,
        method: Ident,
    ) -> NamingResult<Entry<Ident, Option<FunctionDef>>> {
        // check the method exists
        let iface = self.find_iface_def(&iface_ident)?;
        if iface.get_method(&method).is_none() {
            return Err(NameError::MemberNotFound {
                span: method.span.clone(),
                base: Type::Interface(iface.name.qualified.clone()),
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
            .or_insert_with(|| None);
        Ok(())
    }

    pub fn define_method_impl(
        &mut self,
        iface: &IdentPath,
        self_ty: Type,
        method_def: FunctionDef,
    ) -> NamingResult<()> {
        match self.method_impl_entry(
            iface.clone(),
            self_ty.clone(),
            method_def.decl.ident.last().clone(),
        )? {
            Entry::Occupied(mut entry) => {
                if entry.get().is_some() {
                    return Err(NameError::AlreadyImplemented {
                        method: method_def.decl.ident.last().clone(),
                        for_ty: self_ty,
                        iface: iface.clone(),
                        existing: entry.key().span.clone(),
                    });
                } else {
                    *entry.get_mut() = Some(method_def);
                }
            }

            Entry::Vacant(entry) => {
                entry.insert(Some(method_def));
            }
        }

        Ok(())
    }

    pub fn find_method_impl_def(
        &self,
        iface: &IdentPath,
        for_ty: &Type,
        method: &Ident,
    ) -> Option<&FunctionDef> {
        let impls = self.iface_impls.get(iface)?;

        let impls_for_ty = impls.get(for_ty)?;

        let method = impls_for_ty.methods.get(method)?.as_ref()?;

        Some(&method)
    }

    pub fn namespace(&self) -> IdentPath {
        IdentPath::from_parts(self.scopes.current_path().keys().cloned())
    }

    pub fn qualify_name(&self, name: Ident) -> IdentPath {
        self.namespace().child(name)
    }

    fn define<DeclPred, MapUnexpected>(
        &mut self,
        name: Ident,
        def: Def,
        decl_predicate: DeclPred,
        map_unexpected: MapUnexpected,
    ) -> NamingResult<()>
    where
        DeclPred: Fn(&Decl) -> DefDeclMatch,
        MapUnexpected: Fn(IdentPath, UnexpectedValue) -> NameError,
    {
        let full_name = IdentPath::new(name.clone(), self.scopes.current_path().keys().cloned());

        if let Some(existing) = self.defs.get(&full_name) {
            return Err(NameError::AlreadyDefined {
                ident: full_name,
                existing: existing.ident().span().clone(),
            });
        }

        match self.scopes.current_path().find(&name) {
            None => {
                return Err(NameError::NotFound(name.clone()));
            }

            Some(MemberRef::Value {
                value,
                parent_path: _,
                key,
            }) => {
                match decl_predicate(value) {
                    DefDeclMatch::Match => {
                        // ok
                    }

                    DefDeclMatch::Mismatch => {
                        return Err(NameError::DefDeclMismatch {
                            decl: key.span().clone(),
                            def: def.ident().span().clone(),
                            ident: full_name,
                        });
                    }

                    DefDeclMatch::WrongKind => {
                        let unexpected = UnexpectedValue::Decl(value.clone());
                        return Err(map_unexpected(full_name, unexpected));
                    }
                }
            }

            Some(MemberRef::Namespace { path }) => {
                let name = path.keys().last().unwrap().clone();
                return Err(map_unexpected(full_name, UnexpectedValue::Namespace(name)));
            }
        }

        self.defs.insert(full_name, def);

        Ok(())
    }

    pub fn define_function(
        &mut self,
        name: Ident,
        def: FunctionDef,
        visibility: Visibility,
    ) -> NamingResult<()> {
        let sig = FunctionSig::of_decl(&def.decl);

        let is_func_decl = |decl: &Decl| match decl {
            Decl::Function {
                sig: existing_sig,
                visibility: existing_vis,
            } => {
                if sig == **existing_sig && visibility == *existing_vis {
                    DefDeclMatch::Match
                } else {
                    DefDeclMatch::Mismatch
                }
            }
            _ => DefDeclMatch::WrongKind,
        };

        let expected_func_err = |ident, unexpected| NameError::Unexpected {
            ident,
            expected: ExpectedKind::Function,
            actual: unexpected,
        };

        self.define(name, Def::Function(def), is_func_decl, expected_func_err)
    }

    pub fn find_type(&self, name: &IdentPath) -> NamingResult<(IdentPath, &Type)> {
        match self.resolve(name) {
            Some(MemberRef::Value {
                value: Decl::Type { ty, .. },
                key,
                ref parent_path,
                ..
            }) => {
                let parent_path = Path::new(key.clone(), parent_path.keys().cloned());
                Ok((parent_path, ty))
            }

            Some(MemberRef::Value { value: other, .. }) => Err(NameError::Unexpected {
                ident: name.clone(),
                expected: ExpectedKind::AnyType,
                actual: other.clone().into(),
            }),

            Some(MemberRef::Namespace { path }) => Err(NameError::Unexpected {
                ident: name.clone(),
                actual: UnexpectedValue::Namespace(path.top().env.namespace().unwrap().clone()),
                expected: ExpectedKind::AnyType,
            }),

            None => Err(NameError::NotFound(name.last().clone())),
        }
    }

    pub fn find_def(&self, name: &IdentPath) -> Option<&Def> {
        self.defs.get(name)
    }

    pub fn find_class_def(&self, name: &IdentPath) -> NamingResult<Rc<Class>> {
        match self.defs.get(name) {
            Some(Def::Class(class_def)) => Ok(class_def.clone()),

            Some(..) => {
                let decl = self.resolve(&name);
                let unexpected = UnexpectedValue::Decl(
                    decl.expect("found def so decl must exist")
                        .as_value()
                        .expect("if def exists it must be a value not a namespace")
                        .clone(),
                );

                Err(NameError::Unexpected {
                    ident: name.clone(),
                    actual: unexpected,
                    expected: ExpectedKind::Class,
                })
            }

            None => Err(NameError::NotFound(name.last().clone())),
        }
    }

    pub fn instantiate_class(&self, name: &Symbol) -> NamingResult<Rc<Class>> {
        name.expect_not_unspecialized()?;

        let base_def = self.find_class_def(&name.qualified)?;

        let instance_def = match &name.type_args {
            Some(type_args) => {
                let instance_def = specialize_class_def(base_def.as_ref(), type_args, name.span())?;
                Rc::new(instance_def)
            }
            None => base_def,
        };

        Ok(instance_def)
    }

    pub fn find_variant_def(&self, name: &IdentPath) -> NamingResult<Rc<Variant>> {
        match self.defs.get(name) {
            Some(Def::Variant(variant_def)) => Ok(variant_def.clone()),

            Some(..) => {
                let decl = self.resolve(&name);
                let unexpected = UnexpectedValue::Decl(
                    decl.expect("found def so decl must exist")
                        .as_value()
                        .expect("if def exists it must be a value not a namespace")
                        .clone(),
                );

                Err(NameError::Unexpected {
                    ident: name.clone(),
                    actual: unexpected,
                    expected: ExpectedKind::Variant,
                })
            }

            None => Err(NameError::NotFound(name.last().clone())),
        }
    }

    pub fn instantiate_variant(&self, name: &Symbol) -> NamingResult<Rc<Variant>> {
        name.expect_not_unspecialized()?;

        let base_def = self.find_variant_def(&name.qualified)?;

        let instance_def = match &name.type_args {
            Some(type_args) => {
                let instance_def =
                    specialize_generic_variant(base_def.as_ref(), type_args, name.span())?;
                Rc::new(instance_def)
            }
            None => base_def,
        };

        Ok(instance_def)
    }

    pub fn find_iface(&self, name: &IdentPath) -> NamingResult<IdentPath> {
        match self.resolve(name) {
            Some(MemberRef::Value {
                value:
                    Decl::Type {
                        ty: Type::Interface(..),
                        ..
                    },
                key,
                ref parent_path,
                ..
            }) => {
                let parent_path = Path::new(key.clone(), parent_path.keys().cloned());

                Ok(parent_path)
            }

            Some(MemberRef::Value { value: other, .. }) => Err(NameError::Unexpected {
                ident: name.clone(),
                actual: other.clone().into(),
                expected: ExpectedKind::Interface,
            }),

            Some(MemberRef::Namespace { path }) => {
                let unexpected =
                    UnexpectedValue::Namespace(path.top().env.namespace().unwrap().clone());
                Err(NameError::Unexpected {
                    ident: name.clone(),
                    actual: unexpected,
                    expected: ExpectedKind::Interface,
                })
            }

            None => Err(NameError::NotFound(name.last().clone())),
        }
    }

    pub fn find_iface_def(&self, name: &IdentPath) -> NamingResult<Rc<Interface>> {
        match self.defs.get(name) {
            Some(Def::Interface(iface_def)) => Ok(iface_def.clone()),

            Some(..) => {
                let decl = self.resolve(&name);
                let unexpected = UnexpectedValue::Decl(
                    decl.expect("found def so decl must exist")
                        .as_value()
                        .expect("if def exists it must be a value not a namespace")
                        .clone(),
                );

                Err(NameError::Unexpected {
                    ident: name.clone(),
                    actual: unexpected,
                    expected: ExpectedKind::Interface,
                })
            }

            None => Err(NameError::NotFound(name.last().clone())),
        }
    }

    pub fn is_iface_impl(&self, self_ty: &Type, iface_name: &IdentPath) -> bool {
        match self_ty {
            Type::GenericParam(param_ty) => match &param_ty.is_iface {
                Some(as_iface) => as_iface.as_iface() == Ok(iface_name),
                None => false,
            },

            _ => match self.iface_impls.get(iface_name) {
                None => false,
                Some(impls) => impls.contains_key(self_ty),
            },
        }
    }

    pub fn implemented_ifaces(&self, self_ty: &Type) -> Vec<IdentPath> {
        match self_ty {
            Type::GenericParam(param_ty) => match &param_ty.is_iface {
                Some(as_iface) => {
                    let iface_path = as_iface
                        .as_iface()
                        .expect("is-constraint can only refer to interface")
                        .clone();
                    vec![iface_path]
                }
                None => Vec::new(),
            },

            _ => {
                let mut result = Vec::new();
                for (iface_id, iface_impl) in &self.iface_impls {
                    if iface_impl.contains_key(self_ty) {
                        result.push(iface_id.clone());
                    }
                }

                result
            }
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
            }

            Some(MemberRef::Value { value: other, .. }) => Err(NameError::Unexpected {
                ident: name.clone(),
                actual: other.clone().into(),
                expected: ExpectedKind::Function,
            }),

            Some(MemberRef::Namespace { path }) => {
                let unexpected =
                    UnexpectedValue::Namespace(path.top().env.namespace().unwrap().clone());
                Err(NameError::Unexpected {
                    ident: name.clone(),
                    actual: unexpected,
                    expected: ExpectedKind::Function,
                })
            }

            None => Err(NameError::NotFound(name.last().clone())),
        }
    }

    pub fn find_instance_member<'ty, 'ctx: 'ty>(
        &'ctx self,
        of_ty: &'ty Type,
        member: &Ident,
    ) -> NamingResult<InstanceMember> {
        let data_member = of_ty.find_data_member(member, self)?;

        let methods = ufcs::instance_methods_of(of_ty, self)?;
        let matching_methods: Vec<_> = methods.iter().filter(|m| *m.ident() == *member).collect();

        fn ambig_paths<'a>(options: impl IntoIterator<Item = (Type, Ident)>) -> Vec<IdentPath> {
            options
                .into_iter()
                .map(|(of_ty, ident)| match of_ty.full_path() {
                    Some(base) => base.child(ident.clone()),
                    None => Path::new(ident.clone(), Vec::new()),
                })
                .collect()
        }

        fn ambig_matching_methods(methods: &[&ufcs::InstanceMethod]) -> Vec<(Type, Ident)> {
            methods
                .iter()
                .map(|im| match im {
                    ufcs::InstanceMethod::Method { iface_ty, decl } => {
                        (iface_ty.clone(), decl.ident.last().clone())
                    }

                    ufcs::InstanceMethod::FreeFunction { sig, func_name, .. } => {
                        let of_ty = sig.params.first().unwrap().ty.clone();
                        (of_ty.clone(), func_name.last().clone())
                    }
                })
                .collect()
        }

        match (data_member, matching_methods.len()) {
            (Some(data_member), 0) => Ok(InstanceMember::Data { ty: data_member.ty }),

            // unambiguous method
            (None, 1) => match matching_methods.last().unwrap() {
                ufcs::InstanceMethod::Method { iface_ty, decl } => Ok(InstanceMember::Method {
                    iface_ty: iface_ty.clone(),
                    method: decl.ident.last().clone(),
                }),

                ufcs::InstanceMethod::FreeFunction { func_name, sig } => {
                    Ok(InstanceMember::UFCSCall {
                        func_name: func_name.clone(),
                        sig: sig.clone(),
                    })
                }
            },

            // no data member, no methods
            (None, 0) => Err(NameError::MemberNotFound {
                span: member.span.clone(),
                member: member.clone(),
                base: of_ty.clone(),
            }),

            // no data member, multiple methods - we can use overloading to determine which
            (None, _) => {
                let candidates: Vec<_> = matching_methods
                    .iter()
                    .map(|m| OverloadCandidate::from_instance_method((**m).clone()))
                    .collect();

                Ok(InstanceMember::Overloaded { candidates })
            }

            // there's a data member AND 1+ methods
            (Some(data_member), _) => Err(NameError::Ambiguous {
                ident: member.clone(),
                options: ambig_paths(
                    ambig_matching_methods(&matching_methods)
                        .into_iter()
                        .chain(vec![(of_ty.clone(), data_member.ident)]),
                ),
            }),
        }
    }

    pub fn is_unsized_ty(&self, ty: &Type) -> NamingResult<bool> {
        match ty {
            Type::Nothing | Type::MethodSelf => Ok(true),

            Type::Class(class) | Type::Record(class) => {
                match self.find_class_def(&class.qualified) {
                    Ok(..) => Ok(false),
                    Err(NameError::NotFound(..)) => Ok(true),
                    Err(err) => Err(err.into()),
                }
            }

            Type::Variant(variant) => match self.find_variant_def(&variant.qualified) {
                Ok(..) => Ok(false),
                Err(NameError::NotFound(..)) => Ok(true),
                Err(err) => Err(err.into()),
            },

            Type::Array { element, .. } => self.is_unsized_ty(element),

            Type::Any
            | Type::GenericParam(_)
            | Type::Interface(..)
            | Type::Pointer(..)
            | Type::Primitive(..)
            | Type::Nil
            | Type::Function(..)
            | Type::DynArray { .. } => Ok(false),
        }
    }

    pub fn find_type_member(&self, ty: &Type, member_ident: &Ident) -> NamingResult<TypeMember> {
        match ty {
            Type::Interface(iface) => {
                let iface_def = self.find_iface_def(iface)?;
                let method_decl = iface_def.get_method(member_ident).ok_or_else(|| {
                    NameError::MemberNotFound {
                        member: member_ident.clone(),
                        base: ty.clone(),
                        span: member_ident.span.clone(),
                    }
                })?;

                Ok(TypeMember::Method {
                    decl: method_decl.clone(),
                })
            }

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
            }
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
                            }

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
            Some(MemberRef::Value {
                parent_path, value, ..
            }) => match value {
                Decl::Type { visibility, .. } | Decl::Function { visibility, .. } => {
                    match visibility {
                        Visibility::Exported => true,
                        Visibility::Private => {
                            let decl_unit_ns = IdentPath::from_parts(parent_path.keys().cloned());
                            let current_ns = self.namespace();

                            current_ns == decl_unit_ns || current_ns.is_parent_of(&decl_unit_ns)
                        }
                    }
                }

                Decl::Alias(..) => false,

                _ => true,
            },

            _ => true,
        }
    }

    pub fn is_constructor_accessible(&self, ty: &Type) -> bool {
        match ty {
            Type::Class(class) => self.namespace().is_parent_of(&class.qualified),
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
