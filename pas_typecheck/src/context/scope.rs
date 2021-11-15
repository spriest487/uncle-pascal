use std::borrow::Borrow;
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::hash::Hash;
use pas_syn::Ident;
use crate::{Decl, Environment, Member, Namespace};

#[derive(Clone, Debug, PartialEq, Eq, Ord, PartialOrd, Hash, Copy)]
pub struct ScopeID(pub usize);

#[derive(Clone, Debug, PartialEq)]
pub struct Scope {
    id: ScopeID,
    env: Environment,
    decls: HashMap<Ident, Member<Scope>>,
}

impl Scope {
    pub fn new(id: ScopeID, env: Environment) -> Self {
        Self {
            id,
            env,
            decls: HashMap::new(),
        }
    }

    pub fn id(&self) -> ScopeID {
        self.id
    }

    pub fn env(&self) -> &Environment {
        &self.env
    }

    pub fn iter_decls(&self) -> impl Iterator<Item=(&Ident, &Member<Scope>)> {
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
            Environment::Module { namespace } => Some(namespace.last()),
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
