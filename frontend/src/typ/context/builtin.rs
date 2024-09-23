use crate::ast::IdentPath;
use crate::ast::Path;
use crate::ast::StructKind;
use crate::ast::TypeDeclName;
use crate::ast::Visibility;
use crate::typ::ast;
use crate::typ::Context;
use crate::typ::Decl;
use crate::typ::Environment;
use crate::typ::Primitive;
use crate::typ::Symbol;
use crate::typ::Type;
use crate::typ::TypecheckResult;
use common::span::*;
use std::rc::Rc;
use linked_hash_map::LinkedHashMap;
use crate::Ident;
use crate::typ::ast::{FunctionDecl, SELF_PARAM_NAME};

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

pub const BUILTIN_FILENAME: &str = "<builtin>";

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
            ast::Field {
                ident: Ident::new(STRING_CHARS_FIELD, builtin_span.clone()),
                ty: Type::from(Primitive::UInt8).ptr(),
                span: builtin_span.clone(),
            }.into(),
            ast::Field {
                ident: Ident::new(STRING_LEN_FIELD, builtin_span.clone()),
                ty: Type::from(Primitive::Int32),
                span: builtin_span.clone(),
            }.into(),
        ],
        kind: StructKind::Class,
        span: builtin_span,
    }
}

// builtin name of the dispose method of the builtin disposable interface
pub fn builtin_disposable_dispose_name() -> ast::TypedFunctionName {
    ast::TypedFunctionName::new_method(
        builtin_ident(DISPOSABLE_DISPOSE_METHOD), 
        Type::interface(builtin_disposable_name().qualified), 
        builtin_span()
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
            decl: ast::FunctionDecl {
                name: builtin_disposable_dispose_name(),
                // TODO: this shouldn't be optional, use unspecified type for parsed functions
                return_ty: Some(Type::Nothing),
                mods: Vec::new(),
                type_params: None,
                params: vec![ast::FunctionParam {
                    ident: Ident::new(SELF_PARAM_NAME, builtin_span.clone()),
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

pub fn builtin_comparable_name() -> Symbol {
    Symbol::from(IdentPath::from_vec(vec![
        builtin_ident(SYSTEM_UNIT_NAME),
        builtin_ident(COMPARABLE_IFACE_NAME),
    ]))
} 

pub fn builtin_comparable_iface() -> ast::InterfaceDecl {
    let builtin_span = builtin_span();
    
    let iface_ty = Type::interface(builtin_comparable_name().qualified);

    ast::InterfaceDecl {
        name: builtin_comparable_name(),
        methods: vec![
            ast::InterfaceMethodDecl {
                decl: builtin_comparable_compare_method(iface_ty, Type::MethodSelf),
            }
        ],
        span: builtin_span.clone(),
    }
}

pub fn builtin_comparable_compare_method(owning_ty: Type, self_ty: Type) -> ast::FunctionDecl {
    let builtin_span = builtin_span();

    ast::FunctionDecl {
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
        name: ast::TypedFunctionName::new_method(
            builtin_ident(COMPARABLE_COMPARE_NAME),
            owning_ty.clone(),
            builtin_span.clone(),
        ),
        return_ty: Some(Type::from(Primitive::Int32)),
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
    
    let iface_ty = Type::interface(builtin_displayable_name().qualified); 

    ast::InterfaceDecl {
        name: builtin_displayable_name(),
        methods: vec![
            ast::InterfaceMethodDecl {
                decl: builtin_displayable_display_method(iface_ty, Type::MethodSelf),
            }
        ],
        span: builtin_span.clone(),
    }
}

pub fn builtin_displayable_display_method(owning_ty: Type, self_ty: Type) -> ast::FunctionDecl {
    let builtin_span = builtin_span();

    ast::FunctionDecl {
        params: vec![
            ast::FunctionParam {
                ident: builtin_ident(SELF_PARAM_NAME),
                ty: self_ty,
                modifier: None,
                span: builtin_span.clone(),
            }
        ],
        name: ast::TypedFunctionName::new_method(
            builtin_ident(DISPLAYABLE_TOSTRING_METHOD),
            owning_ty.clone(),
            builtin_span.clone(),
        ),
        return_ty: Some(Type::class(IdentPath::from_vec(vec![
            Ident::new(SYSTEM_UNIT_NAME, builtin_span.clone()),
            Ident::new(STRING_TYPE_NAME, builtin_span.clone()),
        ]))),
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
) -> TypecheckResult<LinkedHashMap<Ident, FunctionDecl>> {
    let builtin_span = builtin_span();
    
    let ident = Ident::new(name, builtin_span.clone());
    ctx.declare_type(ident, ty.clone(), Visibility::Interface)?;
    

    let type_env = Environment::TypeDecl { ty: ty.clone() };
    ctx.scope(type_env, |ctx| {
        let mut methods = LinkedHashMap::new();
        
        if displayable {
            let display_method = builtin_displayable_display_method(ty.clone(), ty.clone());
            ctx.declare_function(display_method.name.ident.clone(), &display_method, Visibility::Interface)?;

            methods.insert(display_method.name.ident.clone(), display_method);
        }

        if comparable {
            let compare_method = builtin_comparable_compare_method(ty.clone(), ty.clone());
            ctx.declare_function(compare_method.name.ident.clone(), &compare_method, Visibility::Interface)?;

            methods.insert(compare_method.name.ident.clone(), compare_method);
        }

        Ok(methods)
    })
}

// builtin types are OK to redeclare in System.pas
pub fn is_valid_builtin_redecl(decl: &Decl) -> bool {
    match decl {
        Decl::Type { ty: Type::Interface(iface), .. } => {            
            **iface == builtin_displayable_name().qualified 
                || **iface == builtin_comparable_name().qualified 
                || **iface == builtin_disposable_name().qualified
        },
        
        Decl::Type { ty: Type::Class(sym), .. } => {
            **sym == builtin_string_name()
        },
        
        _ => false,
    }
}
