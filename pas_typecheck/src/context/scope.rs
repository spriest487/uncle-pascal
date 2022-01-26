use crate::{AlreadyDeclared, Decl, Environment, Member, MemberRef, Namespace, NamespaceStack, PathRef};
use pas_syn::ast::Visibility;
use pas_syn::{Ident, IdentPath};
use std::borrow::Borrow;
use std::collections::HashMap;
use std::fmt;
use std::hash::Hash;

#[derive(Clone, Debug, PartialEq, Eq, Ord, PartialOrd, Hash, Copy)]
pub struct ScopeID(pub usize);

#[derive(Clone, Debug, PartialEq)]
pub struct Scope {
    id: ScopeID,
    env: Environment,
    decls: HashMap<Ident, Member<Scope>>,

    use_units: Vec<IdentPath>,
}

impl Scope {
    pub fn new(id: ScopeID, env: Environment) -> Self {
        Self {
            id,
            env,
            decls: HashMap::new(),

            use_units: Vec::new(),
        }
    }

    pub fn add_use_unit(&mut self, use_unit: IdentPath) {
        if !self.use_units.contains(&use_unit) {
            self.use_units.push(use_unit);
        }
    }

    pub fn use_units(&self) -> &[IdentPath] {
        &self.use_units
    }

    pub fn id(&self) -> ScopeID {
        self.id
    }

    pub fn env(&self) -> &Environment {
        &self.env
    }

    pub fn iter_decls(&self) -> impl Iterator<Item = (&Ident, &Member<Scope>)> {
        self.decls.iter()
    }

    pub fn get_decl(&self, ident: &Ident) -> Option<&Member<Scope>> {
        self.decls.get(ident)
    }

    pub fn get_decl_mut(&mut self, ident: &Ident) -> Option<&mut Member<Scope>> {
        self.decls.get_mut(ident)
    }
}

impl Namespace for Scope {
    type Key = Ident;
    type Value = Decl;

    fn key(&self) -> Option<&Self::Key> {
        match &self.env {
            Environment::Namespace { namespace } => Some(namespace.last()),
            _ => None,
        }
    }

    fn keys(&self) -> Vec<Ident> {
        self.decls.keys().cloned().collect()
    }

    fn get_member<Q>(&self, member_key: &Q) -> Option<(&Ident, &Member<Self>)>
    where
        Ident: Borrow<Q>,
        Q: Hash + Eq + ?Sized + fmt::Debug,
    {
        self.decls
            .iter()
            .find(|(k, _v)| (*k).borrow() == member_key)
    }

    fn insert_member(
        &mut self,
        key: Ident,
        member_val: Member<Self>,
    ) -> Result<(), AlreadyDeclared<Ident>> {
        if let Some(existing) = self.decls.get(&key) {
            let kind = existing.kind();
            let mut path = self.keys();
            path.push(key.clone());

            return Err(AlreadyDeclared(path, kind));
        }

        self.decls.insert(key.clone(), member_val);
        Ok(())
    }

    fn replace_member(&mut self, key: Ident, member_val: Member<Self>) {
        self.decls.insert(key, member_val);
    }
}

impl<'a> PathRef<'a, Scope> {
    pub fn to_namespace(&self) -> IdentPath {
        let namespace = self.as_slice().iter().filter_map(|s| s.key().cloned());
        IdentPath::from_parts(namespace)
    }

    pub fn all_used_units(&self) -> Vec<IdentPath> {
        let mut unit_paths = Vec::new();

        for scope in self.as_slice().iter().rev() {
            for used_unit in scope.use_units() {
                if !unit_paths.contains(used_unit) {
                    unit_paths.push(used_unit.clone());
                }
            }
        }

        unit_paths
    }
}

impl NamespaceStack<Scope> {
    pub fn visit_visible<Visitor>(&self, visitor: Visitor)
    where
        Visitor: FnMut(&[Ident], &Ident, &Decl),
    {
        self.visit_members(
            |ns_path, key, _member| {
                let member_path = IdentPath::new(key.clone(), ns_path.to_vec());
                self.is_accessible(&member_path)
            },
            visitor,
        );
    }

    pub fn is_accessible(&self, name: &IdentPath) -> bool {
        let current_path = self.current_path();
        let current_ns = current_path.to_namespace();
        let current_uses = current_path.all_used_units();

        match self.resolve(name.as_slice()) {
            Some(MemberRef::Value {
                     parent_path, value, ..
                 }) => match value {
                Decl::Type { visibility, .. } | Decl::Function { visibility, .. } => {
                    match visibility {
                        Visibility::Exported => true,
                        Visibility::Private => {
                            let decl_unit_ns = IdentPath::from_parts(parent_path.keys().cloned());

                            current_ns == decl_unit_ns || current_ns.is_parent_of(&decl_unit_ns)
                        }
                    }
                }

                Decl::Alias(..) => false,

                _ => true,
            },

            Some(MemberRef::Namespace { .. }) => {
                current_uses.contains(name)
            }

            _ => true,
        }
    }
}
