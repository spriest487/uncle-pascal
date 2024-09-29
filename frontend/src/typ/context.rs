pub mod builtin;
pub mod scope;
pub mod value_kind;

mod result;
mod ufcs;
mod decl;
mod def;

pub use self::builtin::*;
pub use self::decl::*;
pub use self::def::*;
pub use self::result::*;
pub use self::scope::*;
pub use self::ufcs::InstanceMethod;
pub use self::value_kind::*;
use crate::ast as syn;
use crate::ast::Ident;
use crate::ast::IdentPath;
use crate::ast::Path;
use crate::ast::Visibility;
use crate::typ::ast::EnumDecl;
use crate::typ::ast::FunctionDecl;
use crate::typ::ast::FunctionDef;
use crate::typ::ast::InterfaceDecl;
use crate::typ::ast::Literal;
use crate::typ::ast::OverloadCandidate;
use crate::typ::ast::StructDef;
use crate::typ::ast::VariantDef;
use crate::typ::ast::SELF_TY_NAME;
use crate::typ::{specialize_generic_variant, TypeParamList};
use crate::typ::specialize_struct_def;
use crate::typ::FunctionSig;
use crate::typ::Primitive;
use crate::typ::Symbol;
use crate::typ::Type;
use crate::typ::TypeError;
use crate::typ::TypeParamType;
use crate::typ::TypeResult;
use common::span::*;
use linked_hash_map::LinkedHashMap;
use std::collections::hash_map::Entry;
use std::collections::hash_map::HashMap;
use std::rc::Rc;

#[derive(Clone, Debug, PartialEq)]
pub struct Binding {
    pub ty: Type,
    pub kind: ValueKind,
    pub def: Option<Ident>,
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
struct MethodCollection {
    methods: HashMap<Ident, Option<FunctionDef>>,
}

impl MethodCollection {
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
    TypeDecl { ty: Type },
    FunctionDecl { owning_ty_params: Option<TypeParamList> },
    FunctionBody(FunctionBodyEnvironment),
    ClosureBody(ClosureBodyEnvironment),
    Block { allow_unsafe: bool },
}

impl Environment {
    pub fn namespace(&self) -> Option<&IdentPath> {
        match self {
            Environment::Namespace { namespace } => Some(namespace),
            _ => None,
        }
    }
    
    pub fn kind_name(&self) -> &'static str {
        match self {
            Environment::Global => "Global",
            Environment::Namespace { .. } => "Namespace",
            Environment::TypeDecl { .. } => "TypeDecl",
            Environment::FunctionDecl { .. } => "FunctionDecl",
            Environment::FunctionBody { .. } => "FunctionBody",
            Environment::ClosureBody { .. } => "ClosureBody",
            Environment::Block { .. } => "Block",
        }
    }
}

impl From<FunctionBodyEnvironment> for Environment {
    fn from(value: FunctionBodyEnvironment) -> Self {
        Environment::FunctionBody(value)
    }
}

impl From<ClosureBodyEnvironment> for Environment {
    fn from(value: ClosureBodyEnvironment) -> Self {
        Environment::ClosureBody(value)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct FunctionBodyEnvironment {
    pub result_ty: Type, 
    pub ty_params: Option<TypeParamList>
}

#[derive(Clone, Debug, PartialEq)]
pub struct ClosureBodyEnvironment {
    pub result_ty: Option<Type>, 
    pub captures: LinkedHashMap<Ident, Type>
}

#[derive(Clone, Debug)]
pub struct Context {
    // span for the whole module, should be position 0 of the primary source file
    module_span: Span,

    next_scope_id: ScopeID,
    scopes: ScopeStack,

    method_defs: HashMap<Type, MethodCollection>,

    // builtin methods declarations for primitive types that can't be declared normally in code
    primitive_methods: HashMap<Primitive, LinkedHashMap<Ident, FunctionDecl>>,
    
    // all primitives currently implement the same set of non-generic interfaces, so we can
    // just use this same list for all of them
    primitive_implements: Vec<Type>,

    /// decl ident -> definition location
    defs: HashMap<IdentPath, Def>,

    loop_stack: Vec<Span>,
}

impl Context {
    pub fn root(module_span: Span) -> Self {
        let mut root_ctx = Self {
            module_span: module_span.clone(),

            scopes: ScopeStack::new(Scope::new(ScopeID(0), Environment::Global)),
            next_scope_id: ScopeID(1),

            defs: Default::default(),

            method_defs: Default::default(),

            loop_stack: Default::default(),

            primitive_methods: HashMap::new(),
            primitive_implements: Vec::new(),
        };

        let system_path = IdentPath::new(builtin_ident(SYSTEM_UNIT_NAME), []);
        root_ctx.unit_scope(system_path, |ctx| {
            let builtin_ifaces = [
                builtin_disposable_iface(),
                builtin_displayable_iface(),
                builtin_comparable_iface(),
            ];

            for builtin_iface in builtin_ifaces {
                ctx
                    .declare_iface(Rc::new(builtin_iface), Visibility::Interface)
                    .expect("builtin interface decl must not fail");
            }

            declare_builtin_ty(ctx, NOTHING_TYPE_NAME, Type::Nothing, false, false)
                .expect("builtin type decl must not fail");
            declare_builtin_ty(ctx, ANY_TYPE_NAME, Type::Any, false, false)
                .expect("builtin type decl must not fail");

            for primitive in &Primitive::ALL {
                let primitive_ty = Type::Primitive(*primitive);
                let methods = declare_builtin_ty(ctx, primitive.name(), primitive_ty, true, true)
                    .expect("primitive type decl must not fail");
                
                ctx.primitive_methods.insert(*primitive, methods);
            }
            
            Ok(())
        }).expect("builtin unit definition must not fail");

        root_ctx.primitive_implements.push(Type::interface(builtin_displayable_name().full_path));
        root_ctx.primitive_implements.push(Type::interface(builtin_comparable_name().full_path));

        root_ctx
    }

    pub fn module_span(&self) -> &Span {
        &self.module_span
    }

    pub fn push_scope(&mut self, env: impl Into<Environment>) -> ScopeID {
        let new_id = self.next_scope_id;
        self.next_scope_id = ScopeID(self.next_scope_id.0 + 1);

        self.scopes.push_scope(Scope::new(new_id, env.into()));
        new_id
    }

    pub fn pop_scope(&mut self, id: ScopeID) -> Scope {
        assert_ne!(ScopeID(0), id, "can't pop the root scope");

        loop {
            let popped = self.scopes.pop_scope();

            if popped.id() == id {
                break popped;
            }
        }
    }

    pub fn scope<F, T>(&mut self, env: impl Into<Environment>, f: F) -> TypeResult<T>
        where F: FnOnce(&mut Context) -> TypeResult<T>
    {
        let scope_id = self.push_scope(env);

        let result = f(self);

        self.pop_scope(scope_id);

        result
    }

    pub fn unit_scope<T, F>(&mut self, unit_path: IdentPath, f: F) -> TypeResult<T>
        where F: FnOnce(&mut Context) -> TypeResult<T>,
    {
        let path_len = unit_path.as_slice().len();
        let mut path_parts = unit_path.into_parts();

        let mut unit_scopes = Vec::with_capacity(path_len);
        let mut part_path = Vec::with_capacity(path_len);

        path_parts.reverse();

        for _ in 0..path_len {
            let part = path_parts.pop().unwrap();
            let part_span = part.span.clone();
            part_path.push(part.clone());

            let part_ns = IdentPath::from_parts(part_path.clone());

            let current_scope = self.scopes.current_mut();
            match current_scope.remove_member(part_ns.last()) {
                None => {
                    // this part of the namespace is new, add a new scope for it
                    let scope = self.push_scope(Environment::Namespace { namespace: part_ns });
                    unit_scopes.push(scope);
                }

                Some(ScopeMember::Scope(existing_scope)) => {
                    // this is a previously declared namespace e.g. we are trying to define unit
                    // A.B.C and A.B has been previously declared - we take B out of the scope
                    // temporarily and make it active again. it'll be returned to its parent
                    // scope as normal when we pop it
                    unit_scopes.push(existing_scope.id());
                    self.scopes.push_scope(existing_scope);
                }

                Some(ScopeMember::Decl(decl)) => {
                    // we are trying to declare namespace A.B.C but A.B refers to something else that
                    // isn't a namespace
                    let err = NameError::Unexpected {
                        ident: part_ns.clone(),
                        actual: Named::from(decl.clone()),
                        expected: ExpectedKind::Namespace,
                    };

                    // restore the previous state
                    current_scope.insert_member(part, ScopeMember::Decl(decl)).unwrap();
                    for unit_scope in unit_scopes.into_iter().rev() {
                        self.pop_scope(unit_scope);
                    }

                    return Err(TypeError::from_name_err(err, part_span));
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
        match self.find_name(name)? {
            ScopeMemberRef::Decl { key, .. } => Some(key),
            _ => None,
        }
    }

    pub fn find_name(&self, name: &Ident) -> Option<ScopeMemberRef> {
        self.find_path(&IdentPath::from_parts([name.clone()]))
    }

    pub fn find_path(&self, path: &IdentPath) -> Option<ScopeMemberRef> {
        // start by assuming any path we are searching for might be relative
        self.find_path_rec(path, true)
    }
    
    fn find_path_rec(&self, path: &IdentPath, path_is_relative: bool) -> Option<ScopeMemberRef> {
        match self.scopes.resolve_path(path) {
            // found an alias - resolve using its real name
            Some(ScopeMemberRef::Decl {
                value: Decl::Alias(aliased),
                ..
            }) => self.find_path(aliased),
            
            // matches a decl
            decl_ref @ Some(ScopeMemberRef::Decl { .. }) => decl_ref,

            // matches a scope - does any decl match from a used unit though?
            scope_ref @ Some(ScopeMemberRef::Scope { .. }) => {
                if path_is_relative {
                    // always resolve to a decl if one matches in any using, rather than a scope
                    // even if the scope with this name is in this unit
                    match self.find_path_in_used_units(path) {
                        used_decl_ref @ Some(ScopeMemberRef::Decl { .. }) => used_decl_ref,
                        None => scope_ref,
                        used_scope_ref @ Some(ScopeMemberRef::Scope { .. }) => used_scope_ref,
                    }
                } else {
                    scope_ref
                }
            },

            // nothing matched in this scope, maybe in one of the used units
            None => {
                if path_is_relative {
                    self.find_path_in_used_units(path)
                } else {
                    None
                }
            },
        }
    }
    
    
    fn find_path_in_used_units(&self, path: &IdentPath) -> Option<ScopeMemberRef> {
        let current_path = self.scopes.current_path();

        // try it as a qualified name in a used namespaces
        // there can be multiple used units that declare the same name - if there's one
        // result, we use that, otherwise it's ambiguous
        let results: Vec<_> = current_path.all_used_units()
            .into_iter()
            .filter_map(|use_unit| {
                let mut path_in_unit = use_unit.clone();
                path_in_unit.extend(path.iter().cloned());

                // this should only be treated as an absolute path
                self.find_path_rec(&path_in_unit, false)
            })
            .collect();

        // the last `uses` import always wins
        results.into_iter().last()
    }
    
    pub fn current_closure_env(&self) -> Option<&ClosureBodyEnvironment> {
        for scope in self.scopes.iter().rev() {
            match scope.env() {
                Environment::ClosureBody(body_env) => return Some(body_env),
                _ => continue,
            }
        }
        
        None
    }
    
    pub fn current_function_env(&self) -> Option<&FunctionBodyEnvironment> {
        for scope in self.scopes.iter().rev() {
            match scope.env() {
                Environment::FunctionBody(body_env) => return Some(body_env),
                _ => continue,
            }
        }
        
        None
    }
    
    pub fn current_func_return_ty(&self) -> Option<&Type> {
        self.current_function_env().map(|env| &env.result_ty)
            .or_else(|| self.current_closure_env()
                .and_then(|env| env.result_ty.as_ref()))
    }
    
    pub fn current_enclosing_ty(&self) -> Option<&Type> {
        for scope in self.scopes.iter().rev() {
            match scope.env() {
                Environment::TypeDecl { ty, .. } => return Some(ty),
                _ => continue,
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

    fn declare(&mut self, name: Ident, decl: Decl) -> TypeResult<()> {
        let local_name_path = IdentPath::from_parts([name.clone()]);

        match self.find_path(&local_name_path) {
            Some(ScopeMemberRef::Decl {
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

                        Err(TypeError::NameError {
                            span: name.span.clone(),
                            err: NameError::AlreadyDeclared {
                                new: name,
                                existing_kind: ScopeMemberKind::Decl,
                                existing: old_ident,
                            },
                        })
                    }
                }
            }

            Some(old_ref) => {
                if let ScopeMemberRef::Decl { value: old_decl, .. } = old_ref {
                    if is_valid_builtin_redecl(&decl) 
                        && old_decl.visibility() == decl.visibility() {
                        return Ok(())
                    }
                }
                
                let old_kind = old_ref.kind();
                let old_ident = match old_ref {
                    ScopeMemberRef::Decl { key, parent_path, .. } => {
                        Path::new(key.clone(), parent_path.keys().cloned())
                    },

                    ScopeMemberRef::Scope { path } => {
                        Path::from_parts(path.keys().cloned())
                    },
                };
                
                Err(TypeError::NameError {
                    span: name.span().clone(),
                    err: NameError::AlreadyDeclared {
                        new: name,
                        existing_kind: old_kind,
                        existing: old_ident,
                    },
                })
            }

            _ => {
                self.scopes
                    .insert_decl(name.clone(), decl)
                    .map_err(|err| {
                        TypeError::NameError {
                            err,
                            span: name.span().clone(),
                        }
                    })
            }
        }
    }

    pub fn declare_binding(&mut self, name: Ident, binding: Binding) -> TypeResult<()> {
        self.declare(name, Decl::BoundValue(binding))?;
        Ok(())
    }

    pub fn declare_iface(
        &mut self,
        iface: Rc<InterfaceDecl>,
        visibility: Visibility,
    ) -> TypeResult<()> {
        let name = iface.name.full_path.last().clone();
        let iface_ty = Type::Interface(Box::new(iface.name.full_path.clone()));
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
        variant: Rc<VariantDef>,
        visibility: Visibility,
    ) -> TypeResult<()> {
        let name = variant.name.ident().clone();

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

    pub fn declare_class(&mut self, class: Rc<StructDef>, visibility: Visibility) -> TypeResult<()> {
        let name = class.name.ident().clone();

        let class_ty = match class.kind {
            syn::StructKind::Class => Type::Class(Box::new(class.name.clone())),
            syn::StructKind::Record | syn::StructKind::PackedRecord => Type::Record(Box::new(class.name.clone())),
        };

        self.declare_type(name.clone(), class_ty.clone(), visibility)?;

        let map_unexpected = |_, _| unreachable!();
        self.define(
            name,
            Def::Class(class.clone()),
            DefDeclMatch::always_match,
            map_unexpected,
        )?;

        Ok(())
    }

    pub fn declare_enum(&mut self, enum_decl: Rc<EnumDecl>, visibility: Visibility) -> TypeResult<()> {
        let name = enum_decl.name.ident().clone();

        let enum_ty = Type::Enum(Box::new(enum_decl.name.clone()));

        self.declare_type(name.clone(), enum_ty.clone(), visibility)?;

        self.define(
            name,
            Def::Enum(enum_decl.clone()),
            DefDeclMatch::always_match,
            |_, _| unreachable!(),
        )?;
        
        for item in &enum_decl.items {
            let ord_val = item.value.as_ref().expect("enum ord values must exist after typechecking");
            self.declare_const(item.ident.clone(), Literal::Integer(*ord_val), enum_ty.clone(), visibility, item.span.clone())?;
        }

        Ok(())
    }

    /// declare the type params of a function in the local scope
    pub fn declare_type_params(&mut self, names: &TypeParamList) -> TypeResult<()> {
        for (pos, param) in names.items.iter().enumerate() {
            let is_iface = param
                .constraint
                .as_ref()
                .map(|c| c.is_ty.clone())
                .map(Box::new);

            self.declare_type(
                param.name.clone(),
                Type::GenericParam(Box::new(TypeParamType {
                    name: param.name.clone(),
                    is_iface,
                    pos,
                })),
                Visibility::Implementation,
            )?;
        }

        Ok(())
    }

    pub fn declare_self_ty(&mut self, ty: Type, span: Span) -> TypeResult<()> {
        let self_ident = Ident::new(SELF_TY_NAME, span);
        self.declare_type(self_ident, ty, Visibility::Implementation)
    }

    pub fn declare_type(
        &mut self,
        name: Ident,
        ty: Type,
        visibility: Visibility,
    ) -> TypeResult<()> {
        self.declare(name, Decl::Type { ty: ty.clone(), visibility })?;

        Ok(())
    }

    pub fn declare_function(
        &mut self,
        name: Ident,
        func_decl: &FunctionDecl,
        visibility: Visibility,
    ) -> TypeResult<()> {
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

    pub fn declare_alias(&mut self, name: Ident, aliased: IdentPath) -> TypeResult<()> {
        self.declare(name, Decl::Alias(aliased))
    }

    pub fn declare_const(
        &mut self,
        name: Ident,
        val: Literal,
        ty: Type,
        visibility: Visibility,
        span: Span,
    ) -> TypeResult<()> {
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
            ScopeMemberRef::Decl {
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

    fn insert_method_def(
        &mut self,
        ty: Type,
        def: FunctionDef
    ) -> NameResult<()> {
        let method = &def.decl.name.ident;
        
        match ty {
            Type::Interface(iface_name) => {
                // check the method exists
                let iface = self.find_iface_def(&iface_name)?;
                let iface_ty = Type::Interface(iface_name);

                if iface.get_method(&def.decl.name.ident).is_none() {
                    return Err(NameError::MemberNotFound {
                        base: NameContainer::Type(iface_ty),
                        member: method.clone(),
                    });
                }

                self.insert_method_def_entry(iface_ty, def)
            }

            Type::Record(sym) | Type::Class(sym) => {
                let struct_def = self.find_struct_def(&sym.full_path)?;
                let struct_ty = Type::struct_type(*sym, struct_def.kind);

                struct_def
                    .find_method(&method)
                    .ok_or_else(|| NameError::MemberNotFound {
                        base: NameContainer::Type(struct_ty.clone()),
                        member: method.clone(),
                    })?;

                self.insert_method_def_entry(struct_ty, def)
            }

            Type::Primitive(primitive) => {
                let ty_methods = self.get_primitive_methods(primitive);
                let primitive_ty = Type::Primitive(primitive);

                ty_methods
                    .get(method)
                    .ok_or_else(|| NameError::MemberNotFound {
                        base: NameContainer::Type(Type::Primitive(primitive)),
                        member: method.clone(),
                    })?;

                self.insert_method_def_entry(primitive_ty, def)
            }

            _ => {
                unreachable!("should only call this with types supporting methods")
            }
        }
    }

    fn insert_method_def_entry(&mut self, ty: Type, def: FunctionDef) -> NameResult<()> {
        let methods = self
            .method_defs
            .entry(ty.clone())
            .or_insert_with(MethodCollection::new);

        match methods.methods.entry(def.decl.name.ident.clone()) {
            Entry::Vacant(vacant) => {
                vacant.insert(Some(def));
                Ok(())
            }

            Entry::Occupied(mut occupied) => {
                match occupied.get_mut() {
                    Some(existing) => Err(NameError::AlreadyDefined {
                        ident: IdentPath::from(def.decl.name.ident.clone()),
                        existing: existing.decl.span().clone(),
                    }),
                    
                    empty => {
                        *empty = Some(def);
                        Ok(())
                    }
                }
            }
        }
    }

    pub fn define_method(
        &mut self,
        ty: Type,
        method_def: FunctionDef,
    ) -> TypeResult<()> {
        let span = method_def.decl.span().clone();
        self
            .insert_method_def(ty, method_def)
            .map_err(|err| {
                TypeError::from_name_err(err, span) 
            })?;

        Ok(())
    }

    pub fn find_method(
        &self,
        ty: &Type,
        method: &Ident,
    ) -> Option<&FunctionDef> {
        let method_defs = self.method_defs.get(ty)?;
        let method = method_defs.methods.get(method)?.as_ref()?;
        
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
    ) -> TypeResult<()>
    where
        DeclPred: Fn(&Decl) -> DefDeclMatch,
        MapUnexpected: Fn(IdentPath, Named) -> NameError,
    {
        let full_name = IdentPath::new(
            name.clone(), 
            self.scopes.current_path().keys().cloned()
        );

        match self.scopes.current_path().find(&name) {
            None => {
                return Err(TypeError::NameError {
                    err: NameError::not_found(name.clone()),
                    span: def.ident().span().clone(),
                });
            }

            Some(ScopeMemberRef::Decl {
                value,
                parent_path: _,
                key,
            }) => {
                match decl_predicate(value) {
                    DefDeclMatch::Match => {
                        if let Some(existing) = self.defs.get(&full_name) {
                            // allow the redeclaration of certain builtin types in System.pas,
                            // as long as the new definition matches the builtin one exactly
                            if !is_valid_builtin_redecl(value) || *existing != def {
                                dbg!(existing);
                                dbg!(&def);
                                
                                let err = NameError::AlreadyDefined {
                                    ident: full_name,
                                    existing: existing.ident().span().clone(),
                                };

                                return Err(TypeError::NameError {
                                    err,
                                    span: def.ident().span().clone(),
                                });    
                            }
                        }
                    }

                    DefDeclMatch::Mismatch => {
                        return Err(TypeError::NameError {
                            err: NameError::DefDeclMismatch {
                                decl: key.span().clone(),
                                def: def.ident().span().clone(),
                                ident: full_name,
                            },
                            span: def.ident().span.clone(),
                        });
                    }

                    DefDeclMatch::WrongKind => {
                        let unexpected = Named::Decl(value.clone());
                        let err = map_unexpected(full_name, unexpected);
                        return Err(TypeError::NameError { err, span: def.span().clone() });
                    }
                }
            }

            Some(ScopeMemberRef::Scope { path }) => {
                let path = IdentPath::from_parts(path.keys().cloned());
                let err = map_unexpected(full_name, Named::Namespace(path));

                return Err(TypeError::NameError { err, span: def.span().clone() });
            }
        }

        self.defs.insert(full_name, def);

        Ok(())
    }

    pub fn define_function(
        &mut self,
        name: Ident,
        def: FunctionDef,
    ) -> TypeResult<()> {
        let sig = FunctionSig::of_decl(&def.decl);

        // defining a function - only the sig needs to match, the visibility of the definition doesn't matter
        // since the implementation of an interface func can be in the implementation section, but the implementation
        // of an implementation func can't be in the interface section, since it comes last
        let is_func_decl = |decl: &Decl| match decl {
            Decl::Function { sig: existing_sig, .. } => {
                if sig != **existing_sig {
                    DefDeclMatch::Mismatch
                } else {
                    DefDeclMatch::Match
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

    pub fn find_type(&self, name: &IdentPath) -> NameResult<(IdentPath, &Type)> {
        match self.find_path(name) {
            Some(ScopeMemberRef::Decl {
                value: Decl::Type { ty, .. },
                key,
                ref parent_path,
                ..
            }) => {
                let parent_path = Path::new(key.clone(), parent_path.keys().cloned());
                Ok((parent_path, ty))
            }

            Some(ScopeMemberRef::Decl { value: other, .. }) => {
                Err(NameError::Unexpected {
                    ident: name.clone(),
                    expected: ExpectedKind::AnyType,
                    actual: other.clone().into(),
                })
            }

            Some(ScopeMemberRef::Scope { path }) => {
                Err(NameError::Unexpected {
                    ident: name.clone(),
                    actual: Named::Namespace(IdentPath::from_parts(path.keys().cloned())),
                    expected: ExpectedKind::AnyType,
                })
            }

            None => Err(NameError::not_found(name.clone()))
        }
    }

    pub fn find_def(&self, name: &IdentPath) -> Option<&Def> {
        self.defs.get(name)
    }

    pub fn find_struct_def(&self, name: &IdentPath) -> NameResult<&Rc<StructDef>> {
        match self.defs.get(name) {
            Some(Def::Class(class_def)) => Ok(class_def),

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

            None => Err(NameError::not_found(name.clone())),
        }
    }

    pub fn instantiate_struct_def(&self, name: &Symbol) -> NameResult<Rc<StructDef>> {
        name.expect_not_unspecialized()?;

        let generic_def = self.find_struct_def(&name.full_path)?;

        let specialized_def = match &name.type_args {
            Some(type_args) => {
                specialize_struct_def(generic_def, type_args, self)?
            }
            None => {
                generic_def.clone()
            },
        };

        Ok(specialized_def)
    }

    pub fn find_variant_def(&self, name: &IdentPath) -> NameResult<Rc<VariantDef>> {
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

            None => Err(NameError::not_found(name.clone())),
        }
    }

    pub fn instantiate_variant_def(&self, name: &Symbol) -> NameResult<Rc<VariantDef>> {
        name.expect_not_unspecialized()?;

        let base_def = self.find_variant_def(&name.full_path)?;

        let instance_def = match &name.type_args {
            Some(type_args) => {
                let instance_def = specialize_generic_variant(
                    base_def.as_ref(), 
                    type_args, 
                    self
                )?;

                Rc::new(instance_def)
            }

            None => base_def,
        };

        Ok(instance_def)
    }

    pub fn find_iface(&self, name: &IdentPath) -> NameResult<IdentPath> {
        match self.find_path(name) {
            Some(ScopeMemberRef::Decl {
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

            Some(ScopeMemberRef::Decl { value: other, .. }) => {
                Err(NameError::Unexpected {
                    ident: name.clone(),
                    actual: other.clone().into(),
                    expected: ExpectedKind::Interface,
                })
            }

            Some(ScopeMemberRef::Scope { path }) => {
                Err(NameError::Unexpected {
                    ident: name.clone(),
                    actual: Named::Namespace(path.to_namespace()),
                    expected: ExpectedKind::Interface,
                })
            }

            None => {
                Err(NameError::not_found(name.clone()))
            },
        }
    }

    pub fn find_iface_def(&self, name: &IdentPath) -> NameResult<&Rc<InterfaceDecl>> {
        match self.defs.get(name) {
            Some(Def::Interface(iface_def)) => Ok(iface_def),

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

            None => Err(NameError::not_found(name.clone())),
        }
    }

    pub fn is_implementation(&self, self_ty: &Type, iface_ty: &Type) -> NameResult<bool> {
        if self_ty == iface_ty {
            return Ok(true);
        }

        match self_ty {
            Type::GenericParam(param_ty) => match &param_ty.is_iface {
                Some(as_iface) => Ok(**as_iface == *iface_ty),
                None => Ok(false),
            },

            Type::Primitive(..) => {
                Ok(self.primitive_implements.contains(iface_ty))
            }

            Type::Record(name) | Type::Class(name) => {
                let def = self.instantiate_struct_def(name)?;
                Ok(def.implements.contains(iface_ty))
            }

            _ => Ok(false),
        }
    }
    
    pub fn is_implementation_at(&self, self_ty: &Type, iface_ty: &Type, at: &Span) -> TypeResult<bool> {
        match self.is_implementation(self_ty, iface_ty) {
            Ok(is_impl) => Ok(is_impl),
            Err(err) => Err(TypeError::from_name_err(err, at.clone())),
        }
    }

    pub fn find_function(&self, name: &IdentPath) -> NameResult<(IdentPath, Rc<FunctionSig>)> {
        match self.find_path(name) {
            Some(ScopeMemberRef::Decl {
                value: Decl::Function { sig, .. },
                key,
                ref parent_path,
                ..
            }) => {
                let func_path = Path::new(key.clone(), parent_path.keys().cloned());
                Ok((func_path, sig.clone()))
            }

            Some(ScopeMemberRef::Decl { value: other, .. }) => {
                Err(NameError::Unexpected {
                    ident: name.clone(),
                    actual: other.clone().into(),
                    expected: ExpectedKind::Function,
                })
            }

            Some(ScopeMemberRef::Scope { path }) => {
                Err(NameError::Unexpected {
                    ident: name.clone(),
                    actual: Named::Namespace(path.to_namespace()),
                    expected: ExpectedKind::Function,
                })
            }

            None => Err(NameError::NotFound { ident: name.clone() }),
        }
    }

    /// Get the methods implemented by this primitive type
    pub fn get_primitive_methods(
        &self, 
        primitive: Primitive
    ) -> &LinkedHashMap<Ident, FunctionDecl> {
        &self.primitive_methods[&primitive]
    }

    /// Get the interface types implemented by this primitive type
    pub fn get_primitive_impls(&self, _primitive: Primitive) -> &[Type] {
        &self.primitive_implements
    }

    pub fn find_instance_member<'ty, 'ctx: 'ty>(
        &'ctx self,
        of_ty: &'ty Type,
        member: &Ident,
    ) -> NameResult<InstanceMember> {
        let data_member = of_ty.find_data_member(member, self)?;

        let methods = ufcs::find_instance_methods_of(of_ty, self)?;
        let matching_methods: Vec<_> = methods
            .iter()
            .filter(|m| *m.ident() == *member)
            .collect();

        match (data_member, matching_methods.len()) {
            (Some(data_member), 0) => Ok(InstanceMember::Data { ty: data_member.ty }),

            // unambiguous method
            (None, 1) => match matching_methods.last().unwrap() {
                ufcs::InstanceMethod::Method { owning_ty: iface_ty, decl } => {
                    Ok(InstanceMember::Method {
                        iface_ty: iface_ty.clone(),
                        method: decl.name.ident.clone(),
                    })
                },

                ufcs::InstanceMethod::FreeFunction { func_name, sig } => {
                    Ok(InstanceMember::UFCSCall {
                        func_name: func_name.clone(),
                        sig: sig.clone(),
                    })
                }
            },

            // no data member, no methods
            (None, 0) => Err(NameError::MemberNotFound {
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

    pub fn is_unsized_ty(&self, ty: &Type) -> NameResult<bool> {
        match ty {
            Type::Nothing | Type::MethodSelf => Ok(true),

            Type::Record(class) => {
                match self.find_struct_def(&class.full_path) {
                    Ok(..) => Ok(false),
                    Err(NameError::NotFound { .. }) => Ok(true),
                    Err(err) => Err(err),
                }
            }

            Type::Variant(variant) => match self.find_variant_def(&variant.full_path) {
                Ok(..) => Ok(false),
                Err(NameError::NotFound { .. }) => Ok(true),
                Err(err) => Err(err.into()),
            },

            Type::Array(array_ty) => self.is_unsized_ty(&array_ty.element_ty),

            Type::Any
            | Type::Class(..)
            | Type::GenericParam(_)
            | Type::Interface(..)
            | Type::Pointer(..)
            | Type::Primitive(..)
            | Type::Nil
            | Type::Function(..)
            | Type::DynArray { .. }
            | Type::Enum(..) => Ok(false),
        }
    }

    pub fn find_type_member(&self, ty: &Type, member_ident: &Ident) -> NameResult<TypeMember> {
        match ty {
            Type::Interface(iface) => {
                let iface_def = self.find_iface_def(iface)?;
                let method_decl = iface_def
                    .get_method(member_ident)
                    .ok_or_else(|| {
                        NameError::MemberNotFound {
                            member: member_ident.clone(),
                            base: NameContainer::Type(ty.clone()),
                        }
                    })?;

                Ok(TypeMember::Method {
                    decl: method_decl.decl.clone(),
                })
            }

            _ => Err(NameError::MemberNotFound {
                base: NameContainer::Type(ty.clone()),
                member: member_ident.clone(),
            }),
        }
    }

    pub fn undefined_syms(&self) -> Vec<IdentPath> {
        let mut syms = Vec::new();

        let current_path = self.scopes.current_path();
        let current_scopes = current_path.as_slice();

        for i in (0..current_scopes.len()).rev() {
            let scope = current_scopes[i];

            let current_scope_ns = IdentPath::from_parts(current_scopes[0..=i]
                .iter()
                .flat_map(|s| s.key())
                .cloned());

            // only functions and methods can possibly be undefined
            for (ident, decl) in scope.members() {
                match decl {
                    // all types except interfaces must define any methods they declare
                    ScopeMember::Decl(Decl::Type { ty, .. }) => {
                        syms.append(&mut self.undefined_ty_members(ty));
                    },
                    
                    ScopeMember::Decl(Decl::Function { .. }) => {
                        let decl_path = current_scope_ns
                            .clone()
                            .child(ident.clone());

                        if self.defs.get(&decl_path).is_none() {
                            // eprintln!("undefined: {}", decl_path);
                            syms.push(decl_path);
                        }
                    }
                    
                    _ => {}
                }
            }
        }

        syms
    }
    
    fn undefined_ty_members(self: &Context, ty: &Type) -> Vec<IdentPath> {
        if ty.as_iface().is_some() {
            return Vec::new();
        }
        
        let ty_name = match ty.full_path() {
            Some(path) => path,
            None => return Vec::new(),
        };
        
        let ty_method_defs = self.method_defs.get(&ty);
        
        let ty_methods = ty
            .methods(self)
            .expect("illegal state: undefined_ty_members failed to get methods from type");
        
        let mut missing = Vec::new();

        for method in ty_methods {
            // don't need to check the sigs again, they wouldn't be added
            // if they didn't match
            let has_def = ty_method_defs
                .and_then(|method_defs| {
                    method_defs
                        .methods
                        .get(&method.name.ident)
                        .map(|def| def.is_some())
                })
                .unwrap_or(false);
                
            if !has_def {
                missing.push(ty_name.clone().child(method.name.ident.clone()));
            }
        }
        
        missing
    }

    /// Mark a local decl as initialized.
    /// No effect if the decl exists and is already initialized.
    /// Panics if the decl doesn't exist or isn't a kind of decl which can be initialized.
    pub fn initialize(&mut self, local_id: &Ident) {
        for scope in self.scopes.iter_mut().rev() {
            if let Some(member) = scope.get_decl_mut(local_id) {
                if let ScopeMember::Decl(Decl::BoundValue(Binding { kind, .. })) = member {
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

    pub fn get_closure_scope(&self) -> Option<&Scope> {
        for scope in self.scopes.iter().rev() {
            if let Environment::ClosureBody { .. } = scope.env() {
                return Some(scope);
            }
        }

        None
    }

    pub fn add_closure_capture(&mut self, name: &Ident, ty: &Type) {
        for scope in self.scopes.iter_mut().rev() {
            if let Environment::ClosureBody(body) = scope.env_mut() {
                if let Some(old_ty) = body.captures.insert(name.clone(), ty.clone()) {
                    if old_ty != *ty {
                        panic!("closure capture did not match previous type: {} (declared as {}, was previously {})", name, ty, old_ty)
                    }
                }
                return;
            }
        }

        panic!("called add_closure_capture without a closure scope active");
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
                ScopeMember::Decl(Decl::BoundValue(Binding {
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
                    .all(|ctx| {
                        match ctx.find_name(&uninit_name).unwrap() {
                            ScopeMemberRef::Decl {
                                value, parent_path, ..
                            } => match value {
                                Decl::BoundValue(binding) => {
                                    parent_path.as_slice().len() == this_depth
                                        && binding.kind == ValueKind::Mutable
                                }

                                _ => false,
                            },

                            ScopeMemberRef::Scope { .. } => false,
                        }
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
            Type::Class(class) => self.namespace().is_parent_of(&class.full_path),
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
            ufcs::InstanceMethod::Method { owning_ty: iface_ty, decl } => {
                (iface_ty.clone(), decl.name.ident.clone())
            }

            ufcs::InstanceMethod::FreeFunction { sig, func_name, .. } => {
                let of_ty = sig.params.first().unwrap().ty.clone();
                (of_ty.clone(), func_name.last().clone())
            }
        })
        .collect()
}
