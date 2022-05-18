use crate::{Primitive, Symbol};
use crate::{ast, Type};
use pas_common::span::*;
use pas_syn::ast::{StructKind, TypeDeclName};
use pas_syn::ident::*;
use std::rc::Rc;

pub const SYSTEM_UNIT_NAME: &str = "System";
pub const NOTHING_TYPE_NAME: &str = "Nothing";
pub const ANY_TYPE_NAME: &str = "Any";

pub const DISPOSABLE_IFACE_NAME: &str = "Disposable";
pub const DISPOSABLE_DISPOSE_METHOD: &str = "Dispose";

pub const DISPLAYABLE_IFACE_NAME: &str = "Displayable";
pub const DISPLAYABLE_TOSTRING_METHOD: &str = "ToString";

pub const STRING_TYPE_NAME: &str = "String";
const STRING_CHARS_FIELD: &str = "chars";
const STRING_LEN_FIELD: &str = "len";

pub fn builtin_span() -> Span {
    Span {
        file: Rc::new("<builtin>".into()),
        start: Location { line: 0, col: 0 },
        end: Location { line: 0, col: 0 },
    }
}

pub fn builtin_string_name() -> Symbol {
    let builtin_span = builtin_span();

    let system_ident = Ident::new(SYSTEM_UNIT_NAME, builtin_span.clone());
    let string_ident = Ident::new(STRING_TYPE_NAME, builtin_span.clone());

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

pub fn builtin_string_class() -> ast::StructDef {
    let builtin_span = builtin_span();

    ast::StructDef {
        name: builtin_string_name(),
        members: vec![
            ast::Member {
                ident: Ident::new(STRING_CHARS_FIELD, builtin_span.clone()),
                ty: Type::from(Primitive::UInt8).ptr(),
                span: builtin_span.clone(),
            },
            ast::Member {
                ident: Ident::new(STRING_LEN_FIELD, builtin_span.clone()),
                ty: Type::from(Primitive::Int32),
                span: builtin_span.clone(),
            },
        ],
        kind: StructKind::Class,
        span: builtin_span,
    }
}

// builtin name of the dispose method of the builtin disposable interface
pub fn builtin_disposable_dispose_name() -> Ident {
    Ident {
        name: Rc::new(DISPOSABLE_DISPOSE_METHOD.to_string()),
        span: builtin_span(),
    }
}

pub fn builtin_disposable_name() -> Symbol {
    let builtin_span = builtin_span();

    let system_ident = Ident::new(SYSTEM_UNIT_NAME, builtin_span.clone());
    let disposable_ident = Ident::new(DISPOSABLE_IFACE_NAME, builtin_span.clone());

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

pub fn builtin_disposable_iface() -> ast::InterfaceDecl {
    let builtin_span = builtin_span();

    ast::InterfaceDecl {
        name: builtin_disposable_name(),
        methods: vec![ast::InterfaceMethodDecl {
            decl: ast::FunctionDecl {
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
            }
        }],
        span: builtin_span,
    }
}
