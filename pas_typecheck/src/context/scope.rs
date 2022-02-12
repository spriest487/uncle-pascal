#[cfg(test)]
mod test;
mod scope_stack;
mod path_ref;

use crate::{Decl, Environment, NameError, NamingResult};
use pas_syn::{Ident, IdentPath};
use std::{
    borrow::Borrow,
    cmp::Ordering,
    collections::HashMap,
    fmt::{self, Write},
    hash::Hash,
};
pub use self::{
    scope_stack::*,
    path_ref::*,
};

#[derive(Clone, Debug, PartialEq, Eq, Ord, PartialOrd, Hash, Copy)]
pub struct ScopeID(pub usize);

#[derive(Clone, Debug, PartialEq)]
pub struct Scope {
    id: ScopeID,
    env: Environment,
    decls: HashMap<Ident, Member>,

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

    pub fn key(&self) -> Option<&Ident> {
        match &self.env {
            Environment::Namespace { namespace } => Some(namespace.last()),
            _ => None,
        }
    }

    pub fn keys(&self) -> Vec<Ident> {
        self.decls.keys().cloned().collect()
    }

    pub fn get_member<Q>(&self, member_key: &Q) -> Option<(&Ident, &Member)>
    where
        Ident: Borrow<Q>,
        Q: Hash + Eq + ?Sized + fmt::Debug,
    {
        self.decls
            .iter()
            .find(|(k, _v)| (*k).borrow() == member_key)
    }

    pub fn insert_member(&mut self, key: Ident, member_val: Member) -> NamingResult<()> {
        if let Some(existing) = self.decls.get(&key) {
            let kind = existing.kind();
            let mut path = self.keys();
            path.push(key.clone());

            return Err(NameError::AlreadyDeclared {
                new: key,
                existing: IdentPath::from_parts(path),
                existing_kind: kind,
            });
        }

        self.decls.insert(key.clone(), member_val);
        Ok(())
    }

    pub fn replace_member(&mut self, key: Ident, member_val: Member) {
        self.decls.insert(key, member_val);
    }

    pub fn remove_member(&mut self, key: &Ident) -> Option<Member> {
        self.decls.remove(key)
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

    pub fn members(&self) -> impl Iterator<Item = (&Ident, &Member)> {
        self.decls.iter()
    }

    pub fn get_decl(&self, ident: &Ident) -> Option<&Member> {
        self.decls.get(ident)
    }

    pub fn get_decl_mut(&mut self, ident: &Ident) -> Option<&mut Member> {
        self.decls.get_mut(ident)
    }

    #[allow(unused)]
    pub fn to_debug_string(&self) -> Result<String, fmt::Error> {
        let mut output = String::new();
        self.to_debug_string_rec(0, &mut output)?;

        Ok(output)
    }

    fn to_debug_string_rec(&self, indent: usize, debug_str: &mut String) -> fmt::Result {
        for _ in 0..indent {
            write!(debug_str, "  ")?;
        }
        match self.env.namespace() {
            Some(ns) => writeln!(debug_str, "Namespace scope: {}", ns)?,
            None => writeln!(debug_str, "Anonymous scope")?,
        }

        let mut members: Vec<_> = self.members().collect();
        members.sort_by(
            |(key_a, decl_a), (key_b, decl_b)| match (decl_a.kind(), decl_b.kind()) {
                (NameKind::Name, NameKind::Namespace) => Ordering::Less,
                (NameKind::Namespace, NameKind::Name) => Ordering::Greater,
                _ => key_a.name.cmp(&key_b.name),
            },
        );

        for (key, member) in members {
            match member {
                Member::Value(decl) => {
                    for _ in 0..indent + 1 {
                        write!(debug_str, "  ")?;
                    }
                    writeln!(debug_str, "{} ({})", key, decl.to_string())?;
                }

                Member::Namespace(scope) => {
                    scope.to_debug_string_rec(indent + 1, debug_str)?;
                }
            }
        }

        Ok(())
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum NameKind {
    Namespace,
    Name,
}

impl fmt::Display for NameKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            NameKind::Namespace => write!(f, "Namespace"),
            NameKind::Name => write!(f, "Name"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Member {
    Namespace(Scope),
    Value(Decl),
}

impl Member {
    pub fn kind(&self) -> NameKind {
        match self {
            Member::Namespace(_) => NameKind::Namespace,
            Member::Value(_) => NameKind::Name,
        }
    }
}

#[derive(Debug, Clone)]
pub enum MemberRef<'s> {
    Value {
        parent_path: PathRef<'s>,
        key: &'s Ident,
        value: &'s Decl,
    },
    Namespace {
        // todo: separate this into parent_path and key
        // refs to namespaces always refer to keyed namespaces, so we should present the top level's
        // key as a Ident ref instead of the Option<Ident> we get from path.top().key()
        path: PathRef<'s>,
    },
}

impl<'s> MemberRef<'s> {
    pub fn as_value(&self) -> Option<&'s Decl> {
        match self {
            MemberRef::Value { value, .. } => Some(value),
            MemberRef::Namespace { .. } => None,
        }
    }

    pub fn kind(&self) -> NameKind {
        match self {
            MemberRef::Value { .. } => NameKind::Name,
            MemberRef::Namespace { .. } => NameKind::Namespace,
        }
    }
}