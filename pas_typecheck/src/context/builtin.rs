use crate::{Primitive, Symbol};
use crate::{ast, Type};
use pas_common::span::*;
use pas_syn::ast::{ClassKind, TypeDeclName};
use pas_syn::ident::*;
use std::rc::Rc;

pub fn builtin_span() -> Span {
    Span {
        file: Rc::new("<builtin>".into()),
        start: Location { line: 0, col: 0 },
        end: Location { line: 0, col: 0 },
    }
}

pub fn builtin_string_name() -> Symbol {
    let builtin_span = builtin_span();

    let system_ident = Ident::new("System", builtin_span.clone());
    let string_ident = Ident::new("String", builtin_span.clone());

    Symbol {
        decl_name: TypeDeclName {
            ident: string_ident.clone(),
            span: builtin_span,
            type_params: None,
        },
        qualified: Path::from(system_ident).child(string_ident),
        type_args: None,
    }
}

pub fn builtin_string_class() -> ast::Class {
    let builtin_span = builtin_span();

    ast::Class {
        name: builtin_string_name(),
        members: vec![
            ast::Member {
                ident: Ident::new("chars", builtin_span.clone()),
                ty: Type::from(Primitive::Byte).ptr(),
                span: builtin_span.clone(),
            },
            ast::Member {
                ident: Ident::new("len", builtin_span.clone()),
                ty: Type::from(Primitive::Int32),
                span: builtin_span.clone(),
            },
        ],
        kind: ClassKind::Object,
        span: builtin_span,
    }
}

// builtin name of the dispose method of the builtin disposable interface
pub fn builtin_disposable_dispose_name() -> Ident {
    Ident {
        name: "Dispose".to_string(),
        span: builtin_span(),
    }
}

pub fn builtin_disposable_name() -> Symbol {
    let builtin_span = builtin_span();

    let system_ident = Ident::new("System", builtin_span.clone());
    let disposable_ident = Ident::new("Disposable", builtin_span.clone());

    Symbol {
        decl_name: TypeDeclName {
            ident: disposable_ident.clone(),
            span: builtin_span.clone(),
            type_params: None,
        },
        qualified: Path::from(system_ident).child(disposable_ident),
        type_args: None,
    }
}

pub fn builtin_disposable_iface() -> ast::Interface {
    let builtin_span = builtin_span();

    ast::Interface {
        name: builtin_disposable_name(),
        methods: vec![ast::FunctionDecl {
            ident: IdentPath::from(builtin_disposable_dispose_name()),
            return_ty: None,
            impl_iface: None,
            mods: Vec::new(),
            type_params: None,
            params: vec![ast::FunctionParam {
                ident: Ident::new("self", builtin_span.clone()),
                ty: Type::MethodSelf,
                modifier: None,
                span: builtin_span.clone(),
            }],
            span: builtin_span.clone(),
        }],
        span: builtin_span,
    }
}
