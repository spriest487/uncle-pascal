use pas_syn::ast::Visibility;
use pas_syn::{Ident, IdentPath};
use crate::{Decl, Member, MemberRef, NameError, NameKind, NamingResult, scope::*};

#[derive(Debug, Clone)]
pub struct NamespaceStack {
    namespaces: Vec<Scope>,
}

impl NamespaceStack {
    pub fn new(root: Scope) -> Self {
        Self {
            namespaces: vec![root],
        }
    }

    pub fn push(&mut self, ns: Scope) {
        self.namespaces.push(ns)
    }

    pub fn pop(&mut self) {
        if self.namespaces.len() == 1 {
            panic!("can't pop the root namespace");
        }

        let popped = self
            .namespaces
            .pop()
            .expect("pop called with no active namespaces");

        if let Some(popped_key) = popped.key().cloned() {
            let current = self.namespaces.last_mut().unwrap();
            if current
                .insert_member(popped_key, Member::Namespace(popped))
                .is_err()
            {
                unreachable!(
                    "should never be possible to declare something with same key as current NS"
                );
            }
        }
    }

    pub fn insert(
        &mut self,
        member_key: impl Into<Ident>,
        member: Decl,
    ) -> NamingResult<()> {
        let member_key = member_key.into();
        let top = self.current_mut();

        // trying to insert a key with the same name as the namespace
        if top.key() == Some(&member_key) {
            return Err(NameError::AlreadyDeclared {
                new: member_key,
                existing: IdentPath::from_parts(top.keys()),
                existing_kind: NameKind::Namespace,
            });
        }

        top.insert_member(member_key.clone(), Member::Value(member))
    }

    // todo: different error codes for "not defined at all" vs "defined but not in the current scope"
    pub fn replace(
        &mut self,
        member_key: impl Into<Ident>,
        member: Decl,
    ) -> NamingResult<()> {
        let member_key = member_key.into();
        let top = self.current_mut();

        match top.get_member(&member_key) {
            Some(_) => {
                top.replace_member(member_key, Member::Value(member));
                Ok(())
            }

            None => Err(NameError::NotFound(member_key)),
        }
    }

    pub fn iter(&self) -> impl ExactSizeIterator<Item=&Scope> + DoubleEndedIterator<Item=&Scope> {
        self.namespaces.iter()
    }

    pub fn current_path(&self) -> PathRef {
        PathRef {
            namespaces: self.namespaces.iter().collect(),
        }
    }

    pub fn iter_mut(&mut self) -> impl ExactSizeIterator<Item=&mut Scope> + DoubleEndedIterator<Item=&mut Scope> {
        self.namespaces.iter_mut()
    }

    pub fn current_path_mut(&mut self) -> PathRefMut {
        PathRefMut {
            namespaces: self.namespaces.iter_mut().collect(),
        }
    }

    pub fn current_mut(&mut self) -> &mut Scope {
        self.namespaces.last_mut().unwrap()
    }

    pub fn resolve_path(&self, path: &[Ident]) -> Option<MemberRef> {
        let mut current = self.current_path();
        for (i, part) in path.iter().enumerate() {
            match current.find(part)? {
                MemberRef::Namespace { path } => {
                    // found a namespace that matches this part, look for the next part inside
                    // that namespace
                    current = path;
                },

                MemberRef::Value {
                    key,
                    value,
                    parent_path,
                } => {
                    let is_last = i == path.len() - 1;

                    return if is_last {
                        Some(MemberRef::Value {
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

        Some(MemberRef::Namespace { path: current })
    }

    pub fn visit_members<Predicate, Visitor>(&self, predicate: Predicate, mut visitor: Visitor)
        where
            Visitor: FnMut(&[Ident], &Ident, &Decl),
            Predicate: Fn(&[Ident], &Ident, &Member) -> bool,
    {
        let mut ns_path = Vec::with_capacity(8);

        for ns_index in (0..self.namespaces.len()).rev() {
            ns_path.clear();
            ns_path.extend(
                self.namespaces[0..=ns_index]
                    .iter()
                    .filter_map(|ns| ns.key())
                    .cloned(),
            );

            let ns = &self.namespaces[ns_index];
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

        match self.resolve_path(name.as_slice()) {
            Some(MemberRef::Value {
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

            Some(MemberRef::Namespace { .. }) => {
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
    member: &Member,
    ns_path: &mut Vec<Ident>,
    predicate: &Predicate,
    visitor: &mut Visitor,
) where
    Visitor: FnMut(&[Ident], &Ident, &Decl),
    Predicate: Fn(&[Ident], &Ident, &Member) -> bool,
{
    if !predicate(ns_path, key, member) {
        return;
    }

    match member {
        Member::Value(value) => {
            visitor(ns_path, key, value);
        }

        Member::Namespace(ns) => {
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

fn print_ns(ns: &Scope, indent: usize, f: &mut fmt::Formatter) -> fmt::Result {
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
            Member::Namespace(child) => {
                print_ns(child, member_indent, f)?;
            }

            Member::Value(val) => {
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

impl fmt::Display for NamespaceStack {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "[")?;

        for ns in &self.namespaces {
            print_ns(ns, 4, f)?;
        }

        write!(f, "]")
    }
}