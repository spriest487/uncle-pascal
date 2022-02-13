use pas_syn::{
    ast::Visibility,
    Ident, IdentPath
};
use crate::{Decl, ScopeMember, ScopeMemberRef, NameError, ScopeMemberKind, NameResult, scope::*};

#[derive(Debug, Clone)]
pub struct ScopeStack {
    scopes: Vec<Scope>,
}

impl ScopeStack {
    pub fn new(root: Scope) -> Self {
        Self {
            scopes: vec![root],
        }
    }

    pub fn push_scope(&mut self, ns: Scope) {
        self.scopes.push(ns)
    }

    pub fn pop_scope(&mut self) {
        if self.scopes.len() == 1 {
            panic!("can't pop the root scope");
        }

        let popped = self
            .scopes
            .pop()
            .expect("pop called with no active scopes");

        if let Some(popped_key) = popped.key().cloned() {
            let current = self.scopes.last_mut().unwrap();
            if current
                .insert_member(popped_key, ScopeMember::Scope(popped))
                .is_err()
            {
                unreachable!(
                    "should never be possible to declare something with same key as current scope"
                );
            }
        }
    }

    pub fn insert_decl(
        &mut self,
        member_key: impl Into<Ident>,
        decl: Decl,
    ) -> NameResult<()> {
        let member_key = member_key.into();
        let top = self.current_mut();

        // trying to insert a key with the same name as the namespace
        if top.key() == Some(&member_key) {
            return Err(NameError::AlreadyDeclared {
                new: member_key,
                existing: IdentPath::from_parts(top.keys()),
                existing_kind: ScopeMemberKind::Scope,
            });
        }

        top.insert_member(member_key.clone(), ScopeMember::Decl(decl))
    }

    // todo: different error codes for "not defined at all" vs "defined but not in the current scope"
    pub fn replace_decl(
        &mut self,
        member_key: impl Into<Ident>,
        member: Decl,
    ) -> NameResult<()> {
        let member_key = member_key.into();
        let top = self.current_mut();

        match top.get_member(&member_key) {
            Some(_) => {
                top.replace_member(member_key, ScopeMember::Decl(member));
                Ok(())
            }

            None => Err(NameError::NotFound {
                ident: IdentPath::from(member_key),
            }),
        }
    }

    pub fn iter(&self) -> impl ExactSizeIterator<Item=&Scope> + DoubleEndedIterator<Item=&Scope> {
        self.scopes.iter()
    }

    pub fn current_path(&self) -> ScopePathRef {
        ScopePathRef {
            namespaces: self.scopes.iter().collect(),
        }
    }

    pub fn iter_mut(&mut self) -> impl ExactSizeIterator<Item=&mut Scope> + DoubleEndedIterator<Item=&mut Scope> {
        self.scopes.iter_mut()
    }

    pub fn current_path_mut(&mut self) -> ScopePathRefMut {
        ScopePathRefMut {
            namespaces: self.scopes.iter_mut().collect(),
        }
    }

    pub fn current_mut(&mut self) -> &mut Scope {
        self.scopes.last_mut().unwrap()
    }

    pub fn resolve_path(&self, path: &IdentPath) -> Option<ScopeMemberRef> {
        let current_path = self.current_path();

        let mut next_path = current_path.clone();
        for (i, part) in path.iter().enumerate() {
            // special case because the namespace stack may not contain namespaces that are under
            // construction during this call e.g. if we are looking for decl A.B.C but A.B is the
            // current namespace, A won't contain B - it isn't stored there until it gets popped
            if next_path.is_parent_of(&current_path) {
                // if the next named part in the current path is the part we're looking for, the
                // path we're looking for is the current path
                let next_named_part = current_path.namespaces[next_path.namespaces.len()..].iter()
                    .find_map(|s| s.key());
                if next_named_part == Some(part) {
                    next_path = current_path.clone();
                    continue;
                }
            }

            match next_path.find(part)? {
                ScopeMemberRef::Scope { path } => {
                    // found a namespace that matches this part, look for the next part inside
                    // that namespace
                    next_path = path;
                },

                ScopeMemberRef::Decl {
                    key,
                    value,
                    parent_path,
                } => {
                    let is_last = i == path.len() - 1;

                    return if is_last {
                        Some(ScopeMemberRef::Decl {
                            parent_path,
                            key,
                            value,
                        })
                    } else {
                        None
                    };
                }
            }
        }

        Some(ScopeMemberRef::Scope { path: next_path })
    }

    pub fn visit_members<Predicate, Visitor>(&self, predicate: Predicate, mut visitor: Visitor)
        where
            Visitor: FnMut(&[Ident], &Ident, &Decl),
            Predicate: Fn(&[Ident], &Ident, &ScopeMember) -> bool,
    {
        let mut ns_path = Vec::with_capacity(8);

        for ns_index in (0..self.scopes.len()).rev() {
            ns_path.clear();
            ns_path.extend(
                self.scopes[0..=ns_index]
                    .iter()
                    .filter_map(|ns| ns.key())
                    .cloned(),
            );

            let ns = &self.scopes[ns_index];
            let ns_keys = ns.keys().into_iter().filter_map(|k| ns.get_member(&k));

            for (key, member) in ns_keys {
                visit_member(key, member, &mut ns_path, &predicate, &mut visitor);
            }
        }
    }

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

        match self.resolve_path(name) {
            Some(ScopeMemberRef::Decl {
                     parent_path, value, ..
                 }) => match value {
                Decl::Type { visibility, .. } | Decl::Function { visibility, .. } => {
                    match visibility {
                        Visibility::Interface => true,
                        Visibility::Implementation => {
                            let decl_unit_ns = IdentPath::from_parts(parent_path.keys().cloned());

                            current_ns == decl_unit_ns || current_ns.is_parent_of(&decl_unit_ns)
                        }
                    }
                }

                Decl::Alias(..) => false,

                _ => true,
            },

            Some(ScopeMemberRef::Scope { .. }) => {
                current_uses.contains(name)
            }

            _ => true,
        }
    }

    // collect the used units for the current scope
    pub fn current_used_units(&self) -> Vec<IdentPath> {
        let mut current_use_units = Vec::new();

        for scope in self.current_path().as_slice().iter().rev() {
            for use_unit in scope.use_units() {
                if !current_use_units.contains(use_unit) {
                    current_use_units.push(use_unit.clone());
                }
            }
        }

        current_use_units
    }
}

fn visit_member<Predicate, Visitor>(
    key: &Ident,
    member: &ScopeMember,
    ns_path: &mut Vec<Ident>,
    predicate: &Predicate,
    visitor: &mut Visitor,
) where
    Visitor: FnMut(&[Ident], &Ident, &Decl),
    Predicate: Fn(&[Ident], &Ident, &ScopeMember) -> bool,
{
    if !predicate(ns_path, key, member) {
        return;
    }

    match member {
        ScopeMember::Decl(value) => {
            visitor(ns_path, key, value);
        }

        ScopeMember::Scope(ns) => {
            let nested_ns_keys = ns.keys();
            let nested_ns_members = nested_ns_keys.into_iter().filter_map(|k| ns.get_member(&k));

            ns_path.push(key.clone());
            for (key, member) in nested_ns_members {
                visit_member(key, member, ns_path, predicate, visitor);
            }
            ns_path.pop();
        }
    }
}

fn print_scope(ns: &Scope, indent: usize, f: &mut fmt::Formatter) -> fmt::Result {
    for _ in 0..indent {
        write!(f, " ")?;
    }

    match ns.key() {
        Some(key) => writeln!(f, "{}: [", key)?,
        None => writeln!(f, "(anon): [")?,
    }

    let members = ns
        .keys()
        .into_iter()
        .map(|key| ns.get_member(&key).unwrap());

    let member_indent = indent + 4;
    for (k, v) in members {
        match v {
            ScopeMember::Scope(child) => {
                print_scope(child, member_indent, f)?;
            }

            ScopeMember::Decl(val) => {
                for _ in 0..member_indent {
                    write!(f, " ")?;
                }

                writeln!(f, "{}: {}", k, val)?;
            }
        }
    }

    for _ in 0..indent {
        write!(f, " ")?;
    }
    writeln!(f, "]")
}

impl fmt::Display for ScopeStack {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "[")?;

        for ns in &self.scopes {
            print_scope(ns, 4, f)?;
        }

        write!(f, "]")
    }
}