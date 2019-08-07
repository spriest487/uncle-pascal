use std::{borrow::Borrow, fmt, hash::Hash};

#[derive(Debug, Clone, PartialEq)]
pub enum Member<NS: Namespace> {
    Namespace(NS),
    Value(NS::Value),
}

#[derive(Debug, Clone)]
pub enum MemberRef<'s, NS: Namespace> {
    Value {
        parent_path: PathRef<'s, NS>,
        key: &'s NS::Key,
        value: &'s NS::Value,
    },
    Namespace {
        // todo: separate this into parent_path and key
        // refs to namespaces always refer to keyed namespaces, so we should present the top level's
        // key as a NS::Key ref instead of the Option<NS::Key> we get from path.top().key()
        path: PathRef<'s, NS>,
    },
}

impl<'s, NS: Namespace> MemberRef<'s, NS> {
    pub fn as_value(&self) -> Option<&'s NS::Value> {
        match self {
            MemberRef::Value { value, .. } => Some(value),
            MemberRef::Namespace { .. } => None,
        }
    }
}

pub trait Namespace: Sized {
    type Key: Eq + PartialEq + Hash + Clone;
    type Value;

    fn key(&self) -> Option<&Self::Key>;
    fn keys(&self) -> Vec<Self::Key>;

    fn get_member<Q>(&self, member_key: &Q) -> Option<(&Self::Key, &Member<Self>)>
    where
        Self::Key: Borrow<Q>,
        Q: Hash + Eq + ?Sized;

    fn insert_member(&mut self, key: Self::Key, member_val: Member<Self>) -> Result<(), Self::Key>;
    fn replace_member(&mut self, key: Self::Key, member_val: Member<Self>);
}

#[derive(Debug, Clone)]
pub struct PathRef<'s, NS: Namespace> {
    namespaces: Vec<&'s NS>,
}

impl<'s, NS: Namespace> PathRef<'s, NS> {
    pub fn find<Q>(&self, key: &Q) -> Option<MemberRef<'s, NS>>
    where
        NS::Key: Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.namespaces
            .iter()
            .enumerate()
            .rev()
            .find_map(|(i, ns)| {
                let mut path: Vec<_> = self.namespaces[0..=i].to_vec();

                let member_ref = if ns.key().map(Borrow::borrow) == Some(key) {
                    MemberRef::Namespace {
                        path: PathRef { namespaces: path },
                    }
                } else {
                    let (key, member) = ns.get_member(key.borrow())?;

                    match member {
                        Member::Namespace(ns) => {
                            path.push(ns);
                            MemberRef::Namespace {
                                path: PathRef { namespaces: path },
                            }
                        }

                        Member::Value(value) => MemberRef::Value {
                            key,
                            value,
                            parent_path: PathRef { namespaces: path },
                        },
                    }
                };

                Some(member_ref)
            })
    }

    pub fn as_slice(&self) -> &[&NS] {
        self.namespaces.as_slice()
    }

    pub fn top(&self) -> &NS {
        self.namespaces.last().unwrap()
    }

    pub fn keys(&self) -> impl Iterator<Item = &NS::Key> {
        self.namespaces.iter().cloned().filter_map(NS::key)
    }
}

#[derive(Debug)]
pub struct PathRefMut<'s, NS: Namespace> {
    namespaces: Vec<&'s mut NS>,
}

impl<'s, NS: Namespace> PathRefMut<'s, NS> {
    pub fn top(&'s mut self) -> &'s mut NS {
        self.namespaces.last_mut().unwrap()
    }

    pub fn as_slice(&self) -> &[&'s mut NS] {
        self.namespaces.as_slice()
    }
}

#[derive(Debug, Clone)]
pub struct NamespaceStack<NS: Namespace> {
    namespaces: Vec<NS>,
}

#[derive(Debug)]
pub struct AlreadyDeclared<Key>(pub Vec<Key>);

#[derive(Debug)]
pub struct NotDefinedHere<Key>(pub Key);

impl<NS: Namespace> NamespaceStack<NS> {
    pub fn new(root: NS) -> Self {
        Self {
            namespaces: vec![root],
        }
    }

    pub fn push(&mut self, ns: NS) {
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
        member_key: impl Into<NS::Key>,
        member: NS::Value,
    ) -> Result<(), AlreadyDeclared<NS::Key>> {
        let member_key = member_key.into();
        let top = self.current_mut();

        if top.key() == Some(&member_key) {
            return Err(AlreadyDeclared(top.keys()));
        }

        top.insert_member(member_key.clone(), Member::Value(member))
            .map_err(|key| {
                let mut path = top.keys();
                path.push(key);
                AlreadyDeclared(path)
            })
    }

    // todo: different error codes for "not defined at all" vs "defined but not in the current scope"
    pub fn replace(
        &mut self,
        member_key: impl Into<NS::Key>,
        member: NS::Value,
    ) -> Result<(), NotDefinedHere<NS::Key>> {
        let member_key = member_key.into();
        let top = self.current_mut();

        match top.get_member(&member_key) {
            Some(_) => {
                top.replace_member(member_key, Member::Value(member));
                Ok(())
            }

            None => Err(NotDefinedHere(member_key)),
        }
    }

    pub fn current_path(&self) -> PathRef<NS> {
        PathRef {
            namespaces: self.namespaces.iter().collect(),
        }
    }

    pub fn current_path_mut(&mut self) -> PathRefMut<NS> {
        PathRefMut {
            namespaces: self.namespaces.iter_mut().collect(),
        }
    }

    pub fn current_mut(&mut self) -> &mut NS {
        self.namespaces.last_mut().unwrap()
    }

    pub fn resolve(&self, path: &[NS::Key]) -> Option<MemberRef<NS>> {
        let mut current = self.current_path();
        for (i, part) in path.iter().enumerate() {
            current = match current.find(part)? {
                MemberRef::Namespace { path } => path,

                MemberRef::Value {
                    key,
                    value,
                    parent_path,
                } => {
                    let is_last = i == path.len() - 1;

                    return if is_last {
                        Some(MemberRef::Value {
                            key,
                            value,
                            parent_path,
                        })
                    } else {
                        None
                    };
                }
            }
        }

        Some(MemberRef::Namespace { path: current })
    }
}

fn print_ns<NS: Namespace>(ns: &NS, indent: usize, f: &mut fmt::Formatter) -> fmt::Result
where
    NS::Key: fmt::Display,
    NS::Value: fmt::Display,
{
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

impl<NS: Namespace> fmt::Display for NamespaceStack<NS>
where
    NS::Key: fmt::Display,
    NS::Value: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "[")?;

        for ns in &self.namespaces {
            print_ns(ns, 4, f)?;
        }

        write!(f, "]")
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use std::collections::hash_map::{Entry, HashMap};

    #[derive(Debug)]
    struct TestNamespace {
        key: Option<String>,
        members: HashMap<String, Member<TestNamespace>>,
    }

    impl Namespace for TestNamespace {
        type Key = String;
        type Value = usize;

        fn key(&self) -> Option<&Self::Key> {
            self.key.as_ref()
        }

        fn keys(&self) -> Vec<String> {
            self.members.keys().cloned().collect()
        }

        fn get_member<Q>(&self, member_key: &Q) -> Option<(&String, &Member<Self>)>
        where
            Self::Key: Borrow<Q>,
            Q: Hash + Eq + ?Sized,
        {
            self.members
                .iter()
                .find(|(k, _v)| (*k).borrow() == member_key)
        }

        fn insert_member(&mut self, key: Self::Key, member: Member<Self>) -> Result<(), String> {
            match self.members.entry(key.clone()) {
                Entry::Vacant(entry) => {
                    entry.insert(member);
                    Ok(())
                }
                Entry::Occupied(entry) => Err(entry.key().clone()),
            }
        }

        fn replace_member(&mut self, key: Self::Key, member_val: Member<Self>) {
            self.members.insert(key, member_val);
        }
    }

    fn ns(key: impl Into<String>) -> TestNamespace {
        TestNamespace {
            key: Some(key.into()),
            members: HashMap::new(),
        }
    }

    fn ns_anon() -> TestNamespace {
        TestNamespace {
            key: None,
            members: HashMap::new(),
        }
    }

    #[test]
    fn finds_val_in_same_ctx() {
        let mut namespaces: NamespaceStack<TestNamespace> = NamespaceStack::new(ns("A"));
        namespaces.insert("x", 123).unwrap();

        match namespaces.current_path().find("x") {
            Some(MemberRef::Value {
                value, parent_path, ..
            }) => {
                assert_eq!(
                    Some("A".to_string()),
                    parent_path.as_slice().last().unwrap().key
                );
                assert_eq!(*value, 123);
            }
            _ => panic!("x should be a value"),
        }
    }

    #[test]
    fn finds_val_in_parent_ctx() {
        let mut namespaces: NamespaceStack<TestNamespace> = NamespaceStack::new(ns("A"));
        namespaces.insert("x", 123).unwrap();
        namespaces.push(ns("B"));

        match namespaces.current_path().find("x") {
            Some(MemberRef::Value {
                value, parent_path, ..
            }) => {
                assert_eq!(
                    Some("A".to_string()),
                    parent_path.as_slice().last().unwrap().key
                );
                assert_eq!(*value, 123);
            }
            _ => panic!("x should be a value"),
        }

        namespaces.pop();
    }

    #[test]
    fn finds_top_val_of_shadowed_name() {
        let mut namespaces: NamespaceStack<TestNamespace> = NamespaceStack::new(ns("A"));
        namespaces.insert("x", 321).unwrap();
        namespaces.push(ns("B"));
        namespaces.insert("x", 123).unwrap();

        match namespaces.current_path().find("x") {
            Some(MemberRef::Value {
                value, parent_path, ..
            }) => {
                assert_eq!(
                    Some("B".to_string()),
                    parent_path.as_slice().last().unwrap().key
                );
                assert_eq!(*value, 123);
            }
            _ => panic!("x should be a value"),
        }

        namespaces.pop();
    }

    #[test]
    fn finds_val_in_anon_namespace() {
        let mut namespaces: NamespaceStack<TestNamespace> = NamespaceStack::new(ns("A"));
        namespaces.push(ns_anon());
        namespaces.insert("x", 123).unwrap();
        namespaces.push(ns("B"));

        match namespaces.current_path().find("x") {
            Some(MemberRef::Value {
                value, parent_path, ..
            }) => {
                assert_eq!(None, parent_path.as_slice().last().unwrap().key);
                assert_eq!(*value, 123);
            }
            _ => panic!("x should be a value"),
        }

        namespaces.pop();
        namespaces.pop();
    }

    #[test]
    fn finds_val_through_anon_namespace() {
        let mut namespaces: NamespaceStack<TestNamespace> = NamespaceStack::new(ns("A"));
        namespaces.push(ns_anon());
        namespaces.push(ns("B"));
        namespaces.insert("x", 123).unwrap();

        match namespaces.current_path().find("x") {
            Some(MemberRef::Value {
                value, parent_path, ..
            }) => {
                assert_eq!(
                    Some("B".to_string()),
                    parent_path.as_slice().last().unwrap().key
                );
                assert_eq!(*value, 123);
            }
            _ => panic!("x should be a value"),
        }

        namespaces.pop();
        namespaces.pop();
    }

    #[test]
    fn finds_val_in_declared_ns() {
        let mut namespaces: NamespaceStack<TestNamespace> = NamespaceStack::new(ns("A"));

        namespaces.push(ns("B"));
        namespaces.insert("x", 123).unwrap();
        namespaces.pop();

        let b = match namespaces.current_path().find("B") {
            Some(MemberRef::Namespace { path }) => path,
            _ => panic!("B should be a namespace"),
        };

        match b.find("x") {
            Some(MemberRef::Value {
                value, parent_path, ..
            }) => {
                assert_eq!(
                    Some("B".to_string()),
                    parent_path.as_slice().last().unwrap().key
                );
                assert_eq!(*value, 123);
            }
            _ => panic!("x should be a value"),
        }
    }

    #[test]
    fn resolves_val_in_declared_ns() {
        let mut namespaces: NamespaceStack<TestNamespace> = NamespaceStack::new(ns("A"));

        namespaces.push(ns("B"));
        namespaces.insert("x", 123).unwrap();
        namespaces.pop();

        match namespaces.resolve(&["B".to_string(), "x".to_string()]) {
            Some(MemberRef::Value {
                value, parent_path, ..
            }) => {
                assert_eq!(
                    Some("B".to_string()),
                    parent_path.as_slice().last().unwrap().key
                );
                assert_eq!(*value, 123);
            }
            _ => panic!("x should be a value"),
        }
    }

    #[test]
    fn resolves_val_in_explicit_root_ns() {
        let mut namespaces: NamespaceStack<TestNamespace> = NamespaceStack::new(ns("A"));

        namespaces.insert("x", 123).unwrap();

        match namespaces.resolve(&["A".to_string(), "x".to_string()]) {
            Some(MemberRef::Value {
                value, parent_path, ..
            }) => {
                assert_eq!(
                    Some("A".to_string()),
                    parent_path.as_slice().last().unwrap().key
                );
                assert_eq!(*value, 123);
            }
            _ => panic!("x should be a value"),
        }
    }

    #[test]
    fn resolves_val_in_root_ns() {
        let mut namespaces: NamespaceStack<TestNamespace> = NamespaceStack::new(ns("A"));

        namespaces.insert("x", 123).unwrap();

        match namespaces.resolve(&["x".to_string()]) {
            Some(MemberRef::Value {
                value, parent_path, ..
            }) => {
                assert_eq!(
                    Some("A".to_string()),
                    parent_path.as_slice().last().unwrap().key
                );
                assert_eq!(*value, 123);
            }
            _ => panic!("x should be a value"),
        }
    }

    #[test]
    fn resolves_val_in_explicit_current_ns() {
        let mut namespaces: NamespaceStack<TestNamespace> = NamespaceStack::new(ns("A"));

        namespaces.push(ns("B"));
        namespaces.insert("x", 123).unwrap();

        match namespaces.resolve(&["B".to_string(), "x".to_string()]) {
            Some(MemberRef::Value {
                value, parent_path, ..
            }) => {
                assert_eq!(
                    Some("B".to_string()),
                    parent_path.as_slice().last().unwrap().key
                );
                assert_eq!(*value, 123);
            }
            _ => panic!("x should be a value"),
        }
    }

    #[test]
    fn resolves_val_in_current_ns() {
        let mut namespaces: NamespaceStack<TestNamespace> = NamespaceStack::new(ns("A"));

        namespaces.push(ns("B"));
        namespaces.insert("x", 123).unwrap();

        match namespaces.resolve(&["x".to_string()]) {
            Some(MemberRef::Value {
                value, parent_path, ..
            }) => {
                assert_eq!(
                    Some("B".to_string()),
                    parent_path.as_slice().last().unwrap().key
                );
                assert_eq!(*value, 123);
            }
            _ => panic!("x should be a value"),
        }
    }

    #[test]
    fn replaces_val_in_correct_scope() {
        let mut namespaces: NamespaceStack<TestNamespace> = NamespaceStack::new(ns("A"));
        namespaces.insert("x", 123).unwrap();
        namespaces.replace("x".to_string(), 321).unwrap();

        match namespaces.current_path().find("x") {
            Some(MemberRef::Value {
                value, parent_path, ..
            }) => {
                assert_eq!(
                    Some("A".to_string()),
                    parent_path.as_slice().last().unwrap().key
                );
                assert_eq!(*value, 321);
            }
            _ => panic!("x should be a value"),
        }
    }

    #[test]
    fn replace_returns_err_for_val_in_wrong_scope() {
        let mut namespaces: NamespaceStack<TestNamespace> = NamespaceStack::new(ns("A"));
        namespaces.insert("x", 123).unwrap();

        namespaces.push(ns("B"));

        assert!(namespaces.replace("x".to_string(), 321).is_err());
    }

    #[test]
    fn finds_popped_scope_by_key() {
        let mut namespaces: NamespaceStack<TestNamespace> = NamespaceStack::new(ns("A"));
        namespaces.push(ns_anon());
        namespaces.push(ns("B"));
        namespaces.pop();

        match namespaces.current_path().find("B") {
            Some(MemberRef::Namespace { path }) => {
                let path_keys: Vec<_> = path.keys().cloned().collect();
                assert_eq!(&["A".to_string(), "B".to_string()], path_keys.as_slice());
            }
            _ => panic!("B should be a namespace"),
        }
    }
}
