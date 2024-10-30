use crate::ast::{FunctionDeclKind, Path};
use crate::ast::StructKind;
use crate::ast::Visibility;
use crate::ast::{Access, IdentPath};
use crate::typ::{ast, FunctionSigParam, FunctionSig};
use crate::typ::ast::MethodDecl;
use crate::typ::ast::SELF_PARAM_NAME;
use crate::typ::Context;
use crate::typ::Environment;
use crate::typ::Primitive;
use crate::typ::Symbol;
use crate::typ::Type;
use crate::typ::TypeResult;
use crate::Ident;
use common::span::*;
use linked_hash_map::LinkedHashMap;
use std::rc::Rc;

pub const SYSTEM_UNIT_NAME: &str = "System";
pub const NOTHING_TYPE_NAME: &str = "Nothing";
pub const ANY_TYPE_NAME: &str = "Any";

pub const DISPOSABLE_IFACE_NAME: &str = "Disposable";
pub const DISPOSABLE_DISPOSE_METHOD: &str = "Dispose";

pub const COMPARABLE_IFACE_NAME: &str = "Comparable";
pub const COMPARABLE_COMPARE_NAME: &str = "Compare";

// TODO: we can probably eventually replace this with an "Object" interface which all class? types
// implicitly implement, and has GetType() etc equivalents
pub const DISPLAYABLE_IFACE_NAME: &str = "Displayable";
pub const DISPLAYABLE_TOSTRING_METHOD: &str = "ToString";

pub const STRING_TYPE_NAME: &str = "String";
const STRING_CHARS_FIELD: &str = "chars";
const STRING_LEN_FIELD: &str = "len";

pub const STRING_CONCAT_FUNC_NAME: &str = "StringConcat";

pub const BUILTIN_FILENAME: &str = "<builtin>";

pub fn get_mem_sig() -> FunctionSig {
    FunctionSig {
        return_ty: Type::Primitive(Primitive::UInt8).ptr(),
        params: vec![FunctionSigParam {
            ty: Type::Primitive(Primitive::Int32),
            modifier: None,
        }],
        type_params: None,
    }
}

pub fn free_mem_sig() -> FunctionSig {
    FunctionSig {
        return_ty: Type::Nothing,
        params: vec![FunctionSigParam {
            ty: Type::Primitive(Primitive::UInt8).ptr(),
            modifier: None,
        }],
        type_params: None,
    }
}

pub fn builtin_span() -> Span {
    Span {
        file: Rc::new(BUILTIN_FILENAME.into()),
        start: Location { line: 0, col: 0 },
        end: Location { line: 0, col: 0 },
    }
}

pub fn builtin_ident(name: &str) -> Ident {
    Ident::new(name, builtin_span())
}

pub fn builtin_unit_path(name: &str) -> IdentPath {
    let builtin_span = builtin_span();

    IdentPath::new(
        Ident::new(name, builtin_span.clone()),
        vec![Ident::new(SYSTEM_UNIT_NAME, builtin_span)]
    )
}

pub fn builtin_string_name() -> Symbol {
    let builtin_span = builtin_span();

    let system_ident = Ident::new(SYSTEM_UNIT_NAME, builtin_span.clone());
    let string_ident = Ident::new(STRING_TYPE_NAME, builtin_span.clone());

    Symbol {
        full_path: Path::from(system_ident).child(string_ident),
        type_args: None,
        type_params: None,
    }
}

pub fn builtin_string_class() -> ast::StructDef {
    let builtin_span = builtin_span();

    ast::StructDef {
        name: builtin_string_name(),
        fields: vec![
            ast::FieldDecl {
                ident: Ident::new(STRING_CHARS_FIELD, builtin_span.clone()),
                ty: Type::from(Primitive::UInt8).ptr(),
                span: builtin_span.clone(),
                access: Access::Private,
            },
            ast::FieldDecl {
                ident: Ident::new(STRING_LEN_FIELD, builtin_span.clone()),
                ty: Type::from(Primitive::Int32),
                span: builtin_span.clone(),
                access: Access::Private,
            },
        ],
        methods: Vec::new(),
        implements: vec![
            Type::interface(builtin_displayable_name().full_path),
            Type::interface(builtin_comparable_name().full_path),
        ],
        forward: false,
        kind: StructKind::Class,
        span: builtin_span,
    }
}

// builtin name of the dispose method of the builtin disposable interface
pub fn builtin_disposable_dispose_name(owning_ty: Option<Type>) -> ast::TypedFunctionName {
    let iface_ty = Type::interface(builtin_disposable_name().full_path);
    let owning_ty = owning_ty.unwrap_or(iface_ty.clone());
    
    ast::TypedFunctionName::new_method(
        builtin_ident(DISPOSABLE_DISPOSE_METHOD),
        owning_ty,
        builtin_span(),
    )
}

pub fn builtin_disposable_name() -> Symbol {
    Symbol::from(IdentPath::from_vec(vec![
        builtin_ident(SYSTEM_UNIT_NAME), 
        builtin_ident(DISPOSABLE_IFACE_NAME)
    ]))
}

pub fn builtin_disposable_iface() -> ast::InterfaceDecl {
    let builtin_span = builtin_span();

    ast::InterfaceDecl {
        name: builtin_disposable_name(),
        methods: vec![ast::InterfaceMethodDecl {
            decl: Rc::new(ast::FunctionDecl {
                name: builtin_disposable_dispose_name(None),
                kind: FunctionDeclKind::Function,
                return_ty: Type::Nothing,
                mods: Vec::new(),
                type_params: None,
                params: vec![ast::FunctionParam {
                    ident: Ident::new(SELF_PARAM_NAME, builtin_span.clone()),
                    ty: Type::MethodSelf,
                    modifier: None,
                    span: builtin_span.clone(),
                }],
                span: builtin_span.clone(),
            }),
        }],
        forward: false,
        span: builtin_span,
    }
}

pub fn builtin_comparable_name() -> Symbol {
    Symbol::from(IdentPath::from_vec(vec![
        builtin_ident(SYSTEM_UNIT_NAME),
        builtin_ident(COMPARABLE_IFACE_NAME),
    ]))
} 

pub fn builtin_comparable_iface() -> ast::InterfaceDecl {
    let builtin_span = builtin_span();
    
    let iface_ty = Type::interface(builtin_comparable_name().full_path);

    ast::InterfaceDecl {
        name: builtin_comparable_name(),
        methods: vec![
            ast::InterfaceMethodDecl {
                decl: Rc::new(builtin_comparable_compare_method(iface_ty, Type::MethodSelf)),
            }
        ],
        forward: false,
        span: builtin_span.clone(),
    }
}

pub fn builtin_comparable_compare_method(owning_ty: Type, self_ty: Type) -> ast::FunctionDecl {
    let builtin_span = builtin_span();

    ast::FunctionDecl {
        name: ast::TypedFunctionName::new_method(
            builtin_ident(COMPARABLE_COMPARE_NAME),
            owning_ty.clone(),
            builtin_span.clone(),
        ),
        kind: FunctionDeclKind::Function,
        params: vec![
            ast::FunctionParam {
                ident: builtin_ident(SELF_PARAM_NAME),
                ty: self_ty.clone(),
                modifier: None,
                span: builtin_span.clone(),
            },
            ast::FunctionParam {
                ident: builtin_ident("other"),
                ty: self_ty.clone(),
                modifier: None,
                span: builtin_span.clone(),
            }
        ],
        return_ty: Type::from(Primitive::Int32),
        type_params: None,
        mods: Vec::new(),
        span: builtin_span.clone(),
    }
}

pub fn builtin_displayable_name() -> Symbol {
    Symbol::from(IdentPath::from_vec(vec![
        builtin_ident(SYSTEM_UNIT_NAME),
        builtin_ident(DISPLAYABLE_IFACE_NAME),
    ]))
}

pub fn builtin_displayable_iface() -> ast::InterfaceDecl {
    let builtin_span = builtin_span();
    
    let iface_ty = Type::interface(builtin_displayable_name().full_path); 

    ast::InterfaceDecl {
        name: builtin_displayable_name(),
        methods: vec![
            ast::InterfaceMethodDecl {
                decl: Rc::new(builtin_displayable_display_method(iface_ty, Type::MethodSelf)),
            }
        ],
        forward: false,
        span: builtin_span.clone(),
    }
}

pub fn builtin_displayable_display_method(owning_ty: Type, self_ty: Type) -> ast::FunctionDecl {
    let builtin_span = builtin_span();

    ast::FunctionDecl {
        name: ast::TypedFunctionName::new_method(
            builtin_ident(DISPLAYABLE_TOSTRING_METHOD),
            owning_ty.clone(),
            builtin_span.clone(),
        ),
        kind: FunctionDeclKind::Function,
        params: vec![
            ast::FunctionParam {
                ident: builtin_ident(SELF_PARAM_NAME),
                ty: self_ty,
                modifier: None,
                span: builtin_span.clone(),
            }
        ],
        return_ty: Type::class(IdentPath::from_vec(vec![
            Ident::new(SYSTEM_UNIT_NAME, builtin_span.clone()),
            Ident::new(STRING_TYPE_NAME, builtin_span.clone()),
        ])),
        type_params: None,
        mods: Vec::new(),
        span: builtin_span.clone(),
    }
}

pub fn declare_builtin_ty(
    ctx: &mut Context,
    name: &str,
    ty: Type,
    comparable: bool,
    displayable: bool
) -> TypeResult<LinkedHashMap<Ident, MethodDecl>> {
    let builtin_span = builtin_span();
    
    let ident = Ident::new(name, builtin_span.clone());
    ctx.declare_type(ident, ty.clone(), Visibility::Interface, false)?;

    let type_env = Environment::TypeDecl { ty: ty.clone() };
    ctx.scope(type_env, |ctx| {
        let mut methods = LinkedHashMap::new();
        
        if displayable {
            let display_method = Rc::new(builtin_displayable_display_method(ty.clone(), ty.clone()));
            ctx.declare_function(display_method.name.ident.clone(), display_method.clone(), Visibility::Interface)?;

            methods.insert(display_method.name.ident.clone(), MethodDecl {
                decl: display_method,
                access: Access::Published,
            });
        }

        if comparable {
            let compare_method = Rc::new(builtin_comparable_compare_method(ty.clone(), ty.clone()));
            ctx.declare_function(compare_method.name.ident.clone(), compare_method.clone(), Visibility::Interface)?;
            
            methods.insert(compare_method.name.ident.clone(), MethodDecl {
                decl: compare_method,
                access: Access::Published,
            });
        }

        Ok(methods)
    })
}
