pub mod builtin;
pub mod result;
pub mod scope;
pub mod value_kind;

mod ufcs;
mod decl;
mod def;

pub use self::{
    builtin::*, decl::*, def::*, result::*, scope::*, ufcs::InstanceMethod, value_kind::*,
};
use crate::ast::Literal;
use crate::{
    ast::{Class, FunctionDecl, FunctionDef, Interface, OverloadCandidate, Variant},
    specialize_class_def, specialize_generic_variant, FunctionSig, Primitive, Symbol, Type,
    TypeParamList, TypeParamType,
};
use pas_common::span::*;
use pas_syn::{ast::Visibility, ident::*};
use std::{
    collections::hash_map::{Entry, HashMap},
    rc::Rc,
};

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

impl DefDeclMatch {
    pub fn always_match(_: &Decl) -> DefDeclMatch {
        DefDeclMatch::Match
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Environment {
    Global,
    Namespace { namespace: IdentPath },
    TypeDecl,
    FunctionDecl,
    FunctionBody { result_ty: Type },
    Block { allow_unsafe: bool },
}

impl Environment {
    pub fn namespace(&self) -> Option<&IdentPath> {
        match self {
            Environment::Namespace { namespace } => Some(namespace),
            _ => None,
        }
    }
}

#[derive(Clone, Debug)]
pub struct Context {
    // span for the whole module, should be position 0 of the primary source file
    module_span: Span,

    next_scope_id: ScopeID,
    scopes: NamespaceStack,

    /// iface ident -> self ty -> impl details
    iface_impls: HashMap<IdentPath, HashMap<Type, InterfaceImpl>>,

    /// decl ident -> definition location
    defs: HashMap<IdentPath, Def>,

    loop_stack: Vec<Span>,
}

impl Context {
    pub fn root(no_stdlib: bool, module_span: Span) -> Self {
        let mut root_ctx = Self {
            module_span: module_span.clone(),

            scopes: NamespaceStack::new(Scope::new(ScopeID(0), Environment::Global)),
            next_scope_id: ScopeID(1),

            defs: Default::default(),

            iface_impls: Default::default(),

            loop_stack: Default::default(),
        };

        let declare_builtin = |ctx: &mut Self, name: &str, ty: Type| {
            let ident = Ident::new(name, module_span.clone());
            ctx.declare_type(ident, ty, Visibility::Interface)
                .expect("builtin type decl must not fail");
        };

        declare_builtin(&mut root_ctx, NOTHING_TYPE_NAME, Type::Nothing);
        declare_builtin(&mut root_ctx, ANY_TYPE_NAME, Type::Any);

        for primitive in &Primitive::ALL {
            declare_builtin(&mut root_ctx, primitive.name(), Type::Primitive(*primitive));
        }

        if no_stdlib {
            root_ctx.def_no_stdlib_types(module_span);
        }

        root_ctx
    }

    fn def_no_stdlib_types(&mut self, module_span: Span) {
        let system_scope = self.push_scope(Environment::Namespace {
            namespace: IdentPath::new(Ident::new(SYSTEM_UNIT_NAME, module_span), vec![]),
        });

        // declare things normally declared in System.pas that the compiler needs to function
        self.declare_class(Rc::new(builtin_string_class()), Visibility::Interface)
            .expect("builtin System.String definition must not fail");

        self.declare_iface(Rc::new(builtin_disposable_iface()), Visibility::Interface)
            .expect("builtin System.Disposable definition must not fail");

        self.pop_scope(system_scope);
    }

    pub fn module_span(&self) -> &Span {
        &self.module_span
    }

    pub fn push_scope(&mut self, env: Environment) -> ScopeID {
        let new_id = self.next_scope_id;
        self.next_scope_id = ScopeID(self.next_scope_id.0 + 1);

        // self.scopes.current_mut().insert_member()

        self.scopes.push(Scope::new(new_id, env));
        new_id
    }

    pub fn pop_scope(&mut self, id: ScopeID) {
        assert_ne!(ScopeID(0), id, "can't pop the root scope");

        loop {
            let popped_id = self.scopes.current_path().top().id();

            self.scopes.pop();

            if popped_id == id {
                break;
            }
        }
    }

    pub fn unit_scope<T, E, F>(&mut self, unit_path: IdentPath, f: F) -> Result<T, E>
        where F: FnOnce(&mut Context) -> Result<T, E>,
        E: From<NameError>
    {
        let path_len = unit_path.as_slice().len();
        let mut path_parts = unit_path.into_parts();

        let mut unit_scopes = Vec::with_capacity(path_len);
        let mut part_path = Vec::with_capacity(path_len);

        path_parts.reverse();

        for _ in 0..path_len {
            let part = path_parts.pop().unwrap();
            part_path.push(part.clone());

            let part_ns = IdentPath::from_parts(part_path.clone());

            let current_scope = self.scopes.current_mut();
            match current_scope.remove_member(part_ns.last()) {
                None => {
                    // this part of the namespace is new, add a new scope for it
                    let scope = self.push_scope(Environment::Namespace { namespace: part_ns });
                    unit_scopes.push(scope);
                }

                Some(Member::Namespace(existing_ns)) => {
                    // this is a previously declared namespace e.g. we are trying to define unit
                    // A.B.C and A.B has been previously declared - we take B out of the scope
                    // temporarily and make it active again. it'll be returned to its parent
                    // scope as normal when we pop it
                    unit_scopes.push(existing_ns.id());
                    self.scopes.push(existing_ns);
                }

                Some(Member::Value(decl)) => {
                    // we are trying to declare namespace A.B.C but A.B refers to something else that
                    // isn't a namespace
                    let err = NameError::Unexpected {
                        ident: part_ns.clone(),
                        actual: Named::from(decl.clone()),
                        expected: ExpectedKind::Namespace,
                    }.into();

                    // restore the previous state
                    current_scope.insert_member(part, Member::Value(decl)).unwrap();
                    for unit_scope in unit_scopes.into_iter().rev() {
                        self.pop_scope(unit_scope);
                    }

                    return Err(err);
                }
            }
        }

        let result = f(self);

        for unit_scope in unit_scopes.into_iter().rev() {
            self.pop_scope(unit_scope);
        }

        result
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

    pub fn use_unit(&mut self, unit: IdentPath) {
        let mut current_path = self.scopes.current_path_mut();
        current_path.top().add_use_unit(unit);
    }

    pub fn find_decl(&self, name: &Ident) -> Option<&Ident> {
        match self.find(name)? {
            MemberRef::Value { key, .. } => Some(key),
            _ => None,
        }
    }

    pub fn find(&self, name: &Ident) -> Option<MemberRef> {
        self.find_path_slice(&[name.clone()])
    }

    pub fn find_path(&self, path: &IdentPath) -> Option<MemberRef> {
        self.find_path_slice(path.as_slice())
    }

    fn find_path_slice(&self, path: &[Ident]) -> Option<MemberRef> {
        match self.scopes.resolve_path(path) {
            Some(MemberRef::Value {
                     value: Decl::Alias(aliased),
                     ..
                 }) => self.find_path(aliased),

            Some(member_ref) => Some(member_ref),

            None if path.len() == 1 => {
                // try it as a qualified name in a used namespaces
                let current_use_units = self.scopes.current_used_units();

                // there can be multiple used units that declare the same name - if there's one
                // result, we use that, otherwise it's ambiguous
                let results: Vec<_> = current_use_units.into_iter()
                    .filter_map(|use_unit| {
                        let mut path_in_unit = use_unit;
                        path_in_unit.extend(path.iter().cloned());

                        self.find_path_slice(path_in_unit.as_slice())
                    })
                    .collect();

                match results.len() {
                    0 => None,
                    1 => Some(results.into_iter().next().unwrap()),
                    _ => {
                        // todo: error for ambiguous resolve
                        None
                    }
                }
            }

            None => None,
        }
    }

    pub fn current_func_return_ty(&self) -> Option<&Type> {
        for scope in self.scopes.iter().rev() {
            if let Environment::FunctionBody { result_ty } = scope.env() {
                return Some(result_ty);
            }
        }

        None
    }

    pub fn allow_unsafe(&self) -> bool {
        for scope in self.scopes.iter().rev() {
            if let Environment::Block { allow_unsafe: true } = scope.env() {
                return true;
            }
        }

        false
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
                        let old_ns = IdentPath::from_parts(parent_path.keys().cloned());
                        let old_ident = old_ns.child(key.clone());

                        Err(NameError::AlreadyDeclared {
                            new: name.clone(),
                            existing_kind: NameKind::Name,
                            existing: old_ident,
                        })
                    }
                }
            }

            Some(old_ref) => {
                let old_kind = old_ref.kind();
                let old_ident = match old_ref {
                    MemberRef::Value {
                        key, parent_path, ..
                    } => Path::new(key.clone(), parent_path.keys().cloned()),
                    MemberRef::Namespace { path } => Path::from_parts(path.keys().cloned()),
                };

                Err(NameError::AlreadyDeclared {
                    new: name.clone(),
                    existing_kind: old_kind,
                    existing: old_ident,
                })
            }

            None => {
                self.scopes.insert(name.clone(), decl)
            }
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
        let iface_ty = Type::Interface(Box::new(iface.name.qualified.clone()));
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
                Visibility::Implementation,
            )?;
        }

        Ok(())
    }

    pub fn declare_self_ty(&mut self, ty: Type, span: Span) -> NamingResult<()> {
        let self_ident = Ident::new("Self", span);
        self.declare_type(self_ident, ty, Visibility::Implementation)
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

    pub fn declare_const(
        &mut self,
        name: Ident,
        val: Literal,
        ty: Type,
        visibility: Visibility,
        span: Span,
    ) -> NamingResult<()> {
        self.declare(
            name,
            Decl::Const {
                visibility,
                val,
                ty,
                span,
            },
        )
    }

    pub fn resolve_alias(&self, path: &IdentPath) -> Option<IdentPath> {
        let member = self.find_path(path)?;

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
        match self.find_path(ns_path) {
            Some(MemberRef::Namespace { path }) => Ok(path.top().keys()),

            Some(MemberRef::Value { value: decl, .. }) => {
                let unexpected = Named::Decl(decl.clone());
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
            let base_ty = Type::Interface(Box::new(iface.name.qualified.clone()));
            return Err(NameError::MemberNotFound {
                span: method.span.clone(),
                base: NameContainer::Type(base_ty),
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
        MapUnexpected: Fn(IdentPath, Named) -> NameError,
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
                        let unexpected = Named::Decl(value.clone());
                        return Err(map_unexpected(full_name, unexpected));
                    }
                }
            }

            Some(MemberRef::Namespace { path }) => {
                let path = IdentPath::from_parts(path.keys().cloned());
                return Err(map_unexpected(full_name, Named::Namespace(path)));
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
        match self.find_path(name) {
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
                actual: Named::Namespace(IdentPath::from_parts(path.keys().cloned())),
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
                let decl = self.find_path(&name);
                let unexpected = Named::Decl(
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
                let decl = self.find_path(&name);
                let unexpected = Named::Decl(
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
        match self.find_path(name) {
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
                Err(NameError::Unexpected {
                    ident: name.clone(),
                    actual: Named::Namespace(path.to_namespace()),
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
                let decl = self.find_path(&name);
                let unexpected = Named::Decl(
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
        match self.find_path(name) {
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
                Err(NameError::Unexpected {
                    ident: name.clone(),
                    actual: Named::Namespace(path.to_namespace()),
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
                base: NameContainer::Type(of_ty.clone()),
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

            Type::Array(array_ty) => self.is_unsized_ty(&array_ty.element_ty),

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
                        base: NameContainer::Type(ty.clone()),
                        span: member_ident.span.clone(),
                    }
                })?;

                Ok(TypeMember::Method {
                    decl: method_decl.clone(),
                })
            }

            _ => Err(NameError::MemberNotFound {
                base: NameContainer::Type(ty.clone()),
                span: member_ident.span.clone(),
                member: member_ident.clone(),
            }),
        }
    }

    pub fn undefined_syms(&self) -> Vec<Ident> {
        let mut syms = Vec::new();

        let current_path = self.scopes.current_path();
        let current_scopes = current_path.as_slice();

        for i in (0..current_scopes.len()).rev() {
            let scope = current_scopes[i];

            let current_scope_ns = IdentPath::from_parts(current_scopes[0..=i].iter()
                .flat_map(|s| s.key())
                .cloned());

            for (ident, decl) in scope.members() {
                // only functions can possibly be undefined
                if let Member::Value(Decl::Function { .. }) = decl {
                    let decl_path = current_scope_ns.clone()
                        .child(ident.clone());

                    if self.defs.get(&decl_path).is_none() {
                        eprintln!("undefined: {}", decl_path);
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
        for scope in self.scopes.iter_mut().rev() {
            if let Some(member) = scope.get_decl_mut(local_id) {
                if let Member::Value(Decl::BoundValue(Binding { kind, .. })) = member {
                    if *kind == ValueKind::Uninitialized || *kind == ValueKind::Mutable {
                        *kind = ValueKind::Mutable;

                        return;
                    } else {
                        panic!("{} does not refer to a mutable binding", local_id);
                    }
                }
            }
        }

        panic!("called initialize() on an id which isn't an initializable binding in this scope: {}", local_id)
    }

    pub fn root_scope(&self) -> &Scope {
        self.scopes.iter().nth(0).unwrap()
    }

    pub fn get_decl_scope(&self, id: &Ident) -> Option<&Scope> {
        for scope in self.scopes.iter().rev() {
            if scope.get_decl(id).is_some() {
                return Some(scope);
            }
        }

        None
    }

    pub fn consolidate_branches(&mut self, branch_contexts: &[Self]) {
        let scope = self.scopes.current_path();

        let uninit_names: Vec<_> = scope
            .top()
            .members()
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
        self.scopes.is_accessible(name)
    }

    pub fn is_constructor_accessible(&self, ty: &Type) -> bool {
        match ty {
            Type::Class(class) => self.namespace().is_parent_of(&class.qualified),
            _ => true,
        }
    }
}

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
