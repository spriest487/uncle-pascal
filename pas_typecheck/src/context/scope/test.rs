use crate::{
    ast::Literal, Decl, Environment, MemberRef, NamespaceStack, Primitive, Scope, ScopeID, Type,
};
use pas_common::span::Span;
use pas_syn::{ast::Visibility, Ident, IdentPath};

fn new_scope(name: &str) -> Scope {
    let namespace = IdentPath::from_parts(vec![ident(name)]);

    Scope::new(ScopeID(0), Environment::Namespace { namespace })
}

fn new_scope_anon() -> Scope {
    Scope::new(
        ScopeID(0),
        Environment::Block {
            allow_unsafe: false,
        },
    )
}

fn ident(name: &str) -> Ident {
    Ident::new(name, Span::zero("test"))
}

fn const_decl(val: i32) -> Decl {
    Decl::Const {
        ty: Type::Primitive(Primitive::Int32),
        span: Span::zero("test"),
        val: Literal::Integer(val.into()),
        visibility: Visibility::Interface,
    }
}

#[test]
fn finds_val_in_same_ctx() {
    let mut namespaces: NamespaceStack = NamespaceStack::new(new_scope("A"));
    namespaces.insert(ident("x"), const_decl(123)).unwrap();

    match namespaces.current_path().find(&ident("x")) {
        Some(MemberRef::Value {
            value, parent_path, ..
        }) => {
            assert_eq!(
                Some(ident("A")).as_ref(),
                parent_path.as_slice().last().unwrap().key()
            );
            assert_eq!(*value, const_decl(123));
        }
        _ => panic!("x should be a value"),
    }
}

#[test]
fn finds_val_in_parent_ctx() {
    let mut namespaces: NamespaceStack = NamespaceStack::new(new_scope("A"));
    namespaces.insert(ident("x"), const_decl(123)).unwrap();
    namespaces.push(new_scope("B"));

    match namespaces.current_path().find(&ident("x")) {
        Some(MemberRef::Value {
            value, parent_path, ..
        }) => {
            assert_eq!(
                Some(ident("A")).as_ref(),
                parent_path.as_slice().last().unwrap().key(),
            );
            assert_eq!(*value, const_decl(123));
        }
        _ => panic!("x should be a value"),
    }

    namespaces.pop();
}

#[test]
fn finds_top_val_of_shadowed_name() {
    let mut namespaces: NamespaceStack = NamespaceStack::new(new_scope("A"));
    namespaces.insert(ident("x"), const_decl(321)).unwrap();
    namespaces.push(new_scope("B"));
    namespaces.insert(ident("x"), const_decl(123)).unwrap();

    match namespaces.current_path().find(&ident("x")) {
        Some(MemberRef::Value {
            value, parent_path, ..
        }) => {
            assert_eq!(
                Some(ident("B")).as_ref(),
                parent_path.as_slice().last().unwrap().key(),
            );
            assert_eq!(*value, const_decl(123));
        }
        _ => panic!("x should be a value"),
    }

    namespaces.pop();
}

#[test]
fn finds_val_in_anon_namespace() {
    let mut namespaces: NamespaceStack = NamespaceStack::new(new_scope("A"));
    namespaces.push(new_scope_anon());
    namespaces.insert(ident("x"), const_decl(123)).unwrap();
    namespaces.push(new_scope("B"));

    match namespaces.current_path().find(&ident("x")) {
        Some(MemberRef::Value {
            value, parent_path, ..
        }) => {
            assert_eq!(None, parent_path.as_slice().last().unwrap().key());
            assert_eq!(*value, const_decl(123));
        }
        _ => panic!("x should be a value"),
    }

    namespaces.pop();
    namespaces.pop();
}

#[test]
fn finds_val_through_anon_namespace() {
    let mut namespaces: NamespaceStack = NamespaceStack::new(new_scope("A"));
    namespaces.push(new_scope_anon());
    namespaces.push(new_scope("B"));
    namespaces.insert(ident("x"), const_decl(123)).unwrap();

    match namespaces.current_path().find(&ident("x")) {
        Some(MemberRef::Value {
            value, parent_path, ..
        }) => {
            assert_eq!(
                Some(ident("B")).as_ref(),
                parent_path.as_slice().last().unwrap().key()
            );
            assert_eq!(*value, const_decl(123));
        }
        _ => panic!("x should be a value"),
    }

    namespaces.pop();
    namespaces.pop();
}

#[test]
fn finds_val_in_declared_ns() {
    let mut namespaces: NamespaceStack = NamespaceStack::new(new_scope("A"));

    namespaces.push(new_scope("B"));
    namespaces.insert(ident("x"), const_decl(123)).unwrap();
    namespaces.pop();

    let b = match namespaces.current_path().find(&ident("B")) {
        Some(MemberRef::Namespace { path }) => path,
        _ => panic!("B should be a namespace"),
    };

    match b.find(&ident("x")) {
        Some(MemberRef::Value {
            value, parent_path, ..
        }) => {
            assert_eq!(
                Some(ident("B")).as_ref(),
                parent_path.as_slice().last().unwrap().key(),
            );
            assert_eq!(*value, const_decl(123));
        }
        _ => panic!("x should be a value"),
    }
}

#[test]
fn resolves_val_in_declared_ns() {
    let mut namespaces: NamespaceStack = NamespaceStack::new(new_scope("A"));

    namespaces.push(new_scope("B"));
    namespaces.insert(ident("x"), const_decl(123)).unwrap();
    namespaces.pop();

    let path = IdentPath::from_parts([ident("B"), ident("x")]);

    match namespaces.resolve_path(path.as_slice()) {
        Some(MemberRef::Value {
            value, parent_path, ..
        }) => {
            assert_eq!(
                Some(ident("B")).as_ref(),
                parent_path.as_slice().last().unwrap().key(),
            );
            assert_eq!(*value, const_decl(123));
        }
        _ => panic!("x should be a value"),
    }
}

#[test]
fn resolves_val_in_explicit_root_ns() {
    let mut namespaces: NamespaceStack = NamespaceStack::new(new_scope("A"));

    namespaces.insert(ident("x"), const_decl(123)).unwrap();

    let path = IdentPath::from_parts([ident("A"), ident("x")]);

    match namespaces.resolve_path(path.as_slice()) {
        Some(MemberRef::Value {
            value, parent_path, ..
        }) => {
            assert_eq!(
                Some(ident("A")).as_ref(),
                parent_path.as_slice().last().unwrap().key(),
            );
            assert_eq!(*value, const_decl(123));
        }
        _ => panic!("x should be a value"),
    }
}

#[test]
fn resolves_val_in_root_ns() {
    let mut namespaces: NamespaceStack = NamespaceStack::new(new_scope("A"));

    namespaces.insert(ident("x"), const_decl(123)).unwrap();

    let path = IdentPath::from_parts([ident("x")]);

    match namespaces.resolve_path(path.as_slice()) {
        Some(MemberRef::Value {
            value, parent_path, ..
        }) => {
            assert_eq!(
                Some(ident("A")).as_ref(),
                parent_path.as_slice().last().unwrap().key(),
            );
            assert_eq!(*value, const_decl(123));
        }
        _ => panic!("x should be a value"),
    }
}

#[test]
fn resolves_val_in_explicit_current_ns() {
    let mut namespaces: NamespaceStack = NamespaceStack::new(new_scope("A"));

    namespaces.push(new_scope("B"));
    namespaces.insert(ident("x"), const_decl(123)).unwrap();

    let path = IdentPath::from_parts([ident("B"), ident("x")]);

    match namespaces.resolve_path(path.as_slice()) {
        Some(MemberRef::Value {
            value, parent_path, ..
        }) => {
            assert_eq!(
                Some(ident("B")).as_ref(),
                parent_path.as_slice().last().unwrap().key(),
            );
            assert_eq!(*value, const_decl(123));
        }
        _ => panic!("x should be a value"),
    }
}

#[test]
fn resolves_val_in_current_ns() {
    let mut namespaces: NamespaceStack = NamespaceStack::new(new_scope("A"));

    namespaces.push(new_scope("B"));
    namespaces.insert(ident("x"), const_decl(123)).unwrap();

    let path = IdentPath::from_parts([ident("x")]);

    match namespaces.resolve_path(path.as_slice()) {
        Some(MemberRef::Value {
            value, parent_path, ..
        }) => {
            assert_eq!(
                Some(ident("B")).as_ref(),
                parent_path.as_slice().last().unwrap().key(),
            );
            assert_eq!(*value, const_decl(123));
        }
        _ => panic!("x should be a value"),
    }
}

#[test]
fn replaces_val_in_correct_scope() {
    let mut namespaces: NamespaceStack = NamespaceStack::new(new_scope("A"));
    namespaces.insert(ident("x"), const_decl(123)).unwrap();
    namespaces
        .replace(ident("x"), const_decl(321))
        .unwrap();

    match namespaces.current_path().find(&ident("x")) {
        Some(MemberRef::Value {
            value, parent_path, ..
        }) => {
            assert_eq!(
                Some(ident("A")).as_ref(),
                parent_path.as_slice().last().unwrap().key(),
            );
            assert_eq!(*value, const_decl(321));
        }
        _ => panic!("x should be a value"),
    }
}

#[test]
fn replace_returns_err_for_val_in_wrong_scope() {
    let mut namespaces: NamespaceStack = NamespaceStack::new(new_scope("A"));
    namespaces.insert(ident("x"), const_decl(123)).unwrap();

    namespaces.push(new_scope("B"));

    assert!(namespaces
        .replace(ident("x"), const_decl(321))
        .is_err());
}

#[test]
fn finds_popped_scope_by_key() {
    let mut namespaces: NamespaceStack = NamespaceStack::new(new_scope("A"));
    namespaces.push(new_scope_anon());
    namespaces.push(new_scope("B"));
    namespaces.pop();

    match namespaces.current_path().find(&ident("B")) {
        Some(MemberRef::Namespace { path }) => {
            let path_keys: Vec<_> = path.keys().cloned().collect();
            assert_eq!(&[ident("A"), ident("B")], path_keys.as_slice());
        }
        _ => panic!("B should be a namespace"),
    }
}

fn visit_all_to_vec(namespaces: &NamespaceStack) -> Vec<(String, Decl)> {
    let mut visited = Vec::new();

    namespaces.visit_members(
        |_ns_path, _key, _member| true,
        |path, _, val| {
            let path_str = IdentPath::from_parts(path.to_vec()).to_string();
            visited.push((path_str, val.clone()));
        },
    );

    visited
}

#[test]
fn visit_all_visits_parent_scopes_rev_order() {
    let mut namespaces: NamespaceStack = NamespaceStack::new(new_scope("A"));
    namespaces.insert(ident("a_val"), const_decl(1)).unwrap();
    namespaces.push(new_scope("B"));
    namespaces.insert(ident("b_val"), const_decl(2)).unwrap();

    let visited = visit_all_to_vec(&namespaces);

    assert_eq!(2, visited.len());
    assert_eq!(("A::B::b_val".to_string(), const_decl(2)), visited[0]);
    assert_eq!(("A::a_val".to_string(), const_decl(1)), visited[1]);
}

#[test]
fn visit_all_visits_unnamed_scopes() {
    let mut namespaces: NamespaceStack = NamespaceStack::new(new_scope("A"));
    namespaces.insert(ident("a_val"), const_decl(1)).unwrap();
    namespaces.push(new_scope_anon());
    namespaces.insert(ident("b_val"), const_decl(2)).unwrap();

    let visited = visit_all_to_vec(&namespaces);

    assert_eq!(2, visited.len());
    assert_eq!(("A::b_val".to_string(), const_decl(2)), visited[0]);
    assert_eq!(("A::a_val".to_string(), const_decl(1)), visited[1]);
}
