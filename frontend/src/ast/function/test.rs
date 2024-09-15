use super::*;
use crate::ast::type_name::IdentTypeName;
use crate::pp::Preprocessor;
use pas_common::BuildOptions;

fn parse_func_decl(src: &str) -> FunctionDecl<Span> {
    let test_unit = Preprocessor::new("test", BuildOptions::default())
        .preprocess(src)
        .unwrap();
    let tokens = TokenTree::tokenize(test_unit).unwrap();
    let mut token_stream = TokenStream::new(tokens, Span::zero("test"));

    let decl = FunctionDecl::parse(&mut token_stream).unwrap();
    token_stream.finish().unwrap();

    decl
}

fn make_ident_path<const N: usize>(names: [&str; N]) -> IdentPath {
    IdentPath::from_parts(names.iter()
        .map(|n| Ident::new(n, Span::zero("test")))
    )
}

fn make_iface_ty<const N: usize>(name: IdentPath, ty_args: [&str; N]) -> TypeName {
    let test_span = Span::zero("test");

    TypeName::Ident(IdentTypeName {
        ident: name,
        type_args: match N {
            0 => None,
            _ => Some(TypeList {
                span: test_span.clone(),
                items: ty_args.iter()
                    .map(|arg_name| {
                        TypeName::Ident(IdentTypeName {
                            ident: IdentPath::from(Ident::new(arg_name, test_span.clone())),
                            type_args: None,
                            indirection: 0,
                            span: test_span.clone(),
                        })
                    })
                    .collect(),
            })
        },
        span: test_span,
        indirection: 0,
    })
}

#[test]
fn parses_simple_name_decl() {
    let decl = parse_func_decl("function TestFunc()");

    assert_eq!(1, decl.ident.as_slice().len());
    assert_eq!(make_ident_path(["TestFunc"]), decl.ident);
}

#[test]
fn parses_unqualified_iface_impl_decl() {
    let decl = parse_func_decl("function TestFunc of I(self: A)");

    assert_eq!(
        Some(make_iface_ty(make_ident_path(["I"]), [])),
        decl.impl_iface.map(|i| i.iface)
    );
    assert_eq!(make_ident_path(["TestFunc"]), decl.ident);
}

#[test]
fn parses_qualified_iface_impl_decl() {
    let decl = parse_func_decl("function TestFunc of NS_A.I(self: A)");

    assert_eq!(
        Some(make_iface_ty(make_ident_path(["NS_A", "I"]), [])),
        decl.impl_iface.map(|i| i.iface)
    );
    assert_eq!(make_ident_path(["TestFunc"]), decl.ident);
}

#[test]
fn parses_func_with_type_params() {
    let decl = parse_func_decl("function TestFunc[A]()");

    assert_eq!(1, decl.ident.as_slice().len());
    assert_eq!(make_ident_path(["TestFunc"]), decl.ident);

    let decl_ty_params = decl.type_params.expect("expected type params to be parsed");
    assert_eq!(1, decl_ty_params.items.len());
    assert_eq!("A", decl_ty_params.items[0].name.name.as_str());
}

#[test]
fn parses_method_of_interface_with_type_args() {
    let decl = parse_func_decl("function TestFunc of A[B]()");

    assert_eq!(1, decl.ident.as_slice().len());
    assert_eq!(make_ident_path(["TestFunc"]), decl.ident);

    let impl_ty = decl.impl_iface.expect("expected an impl ty to be parsed");
    let expect_iface = make_iface_ty(make_ident_path(["A"]), ["B"]);
    assert_eq!(expect_iface, impl_ty.iface);
}

#[test]
fn parses_method_of_interface_with_type_args_with_type_params() {
    let decl = parse_func_decl("function TestFunc[C] of A[B]()");

    let impl_ty = decl.impl_iface.expect("expected an impl ty to be parsed");
    let expect_iface = make_iface_ty(make_ident_path(["A"]), ["B"]);
    assert_eq!(expect_iface, impl_ty.iface);

    assert_eq!(1, decl.ident.as_slice().len());
    assert_eq!(make_ident_path(["TestFunc"]), decl.ident);

    let decl_ty_params = decl.type_params.expect("expected type params to be found");
    assert_eq!(1, decl_ty_params.items.len());
    assert_eq!("C", decl_ty_params.items[0].name.name.as_str());
}
