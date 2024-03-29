#[cfg(test)]
mod test;
mod scope_stack;
mod path_ref;

use crate::{Decl, Environment, NameError, NameResult};
use pas_syn::{Ident, IdentPath};
use std::{
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
    decls: HashMap<Ident, ScopeMember>,

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

    pub fn get_member(&self, member_key: &Ident) -> Option<(&Ident, &ScopeMember)> {
        self.decls.get_key_value(member_key)
    }

    pub fn insert_member(&mut self, key: Ident, member_val: ScopeMember) -> NameResult<()> {
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

    pub fn replace_member(&mut self, key: Ident, member_val: ScopeMember) {
        self.decls.insert(key, member_val);
    }

    pub fn remove_member(&mut self, key: &Ident) -> Option<ScopeMember> {
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

    pub fn members(&self) -> impl Iterator<Item = (&Ident, &ScopeMember)> {
        self.decls.iter()
    }

    pub fn get_decl(&self, ident: &Ident) -> Option<&ScopeMember> {
        self.decls.get(ident)
    }

    pub fn get_decl_mut(&mut self, ident: &Ident) -> Option<&mut ScopeMember> {
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
                (ScopeMemberKind::Decl, ScopeMemberKind::Scope) => Ordering::Less,
                (ScopeMemberKind::Scope, ScopeMemberKind::Decl) => Ordering::Greater,
                _ => key_a.name.cmp(&key_b.name),
            },
        );

        for (key, member) in members {
            match member {
                ScopeMember::Decl(decl) => {
                    for _ in 0..indent + 1 {
                        write!(debug_str, "  ")?;
                    }
                    writeln!(debug_str, "{} ({})", key, decl.to_string())?;
                }

                ScopeMember::Scope(scope) => {
                    scope.to_debug_string_rec(indent + 1, debug_str)?;
                }
            }
        }

        Ok(())
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum ScopeMemberKind {
    Scope,
    Decl,
}

impl fmt::Display for ScopeMemberKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ScopeMemberKind::Scope => write!(f, "Scope"),
            ScopeMemberKind::Decl => write!(f, "Decl"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ScopeMember {
    Scope(Scope),
    Decl(Decl),
}

impl ScopeMember {
    pub fn kind(&self) -> ScopeMemberKind {
        match self {
            ScopeMember::Scope(_) => ScopeMemberKind::Scope,
            ScopeMember::Decl(_) => ScopeMemberKind::Decl,
        }
    }
}

#[derive(Debug, Clone)]
pub enum ScopeMemberRef<'s> {
    Decl {
        parent_path: ScopePathRef<'s>,
        key: &'s Ident,
        value: &'s Decl,
    },
    Scope {
        // todo: separate this into parent_path and key
        // refs to namespaces always refer to keyed namespaces, so we should present the top level's
        // key as a Ident ref instead of the Option<Ident> we get from path.top().key()
        path: ScopePathRef<'s>,
    },
}

impl<'s> ScopeMemberRef<'s> {
    pub fn as_value(&self) -> Option<&'s Decl> {
        match self {
            ScopeMemberRef::Decl { value, .. } => Some(value),
            ScopeMemberRef::Scope { .. } => None,
        }
    }

    pub fn kind(&self) -> ScopeMemberKind {
        match self {
            ScopeMemberRef::Decl { .. } => ScopeMemberKind::Decl,
            ScopeMemberRef::Scope { .. } => ScopeMemberKind::Scope,
        }
    }
}