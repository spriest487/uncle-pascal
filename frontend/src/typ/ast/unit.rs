use crate::ast;
use crate::ast::{FunctionName, IdentPath};
use crate::ast::Visibility;
use crate::ast::BindingDeclKind;
use crate::typ::ast::const_eval::ConstEval;
use crate::typ::ast::expr::expect_stmt_initialized;
use crate::typ::ast::typecheck_alias;
use crate::typ::ast::typecheck_enum_decl;
use crate::typ::ast::typecheck_expr;
use crate::typ::ast::typecheck_func_decl;
use crate::typ::ast::typecheck_func_def;
use crate::typ::ast::typecheck_iface;
use crate::typ::ast::typecheck_stmt;
use crate::typ::ast::typecheck_struct_decl;
use crate::typ::ast::typecheck_variant;
use crate::typ::ast::Expr;
use crate::typ::{typecheck_type, FunctionSig, NameContainer};
use crate::typ::typecheck_type_params;
use crate::typ::Binding;
use crate::typ::ConstTyped;
use crate::typ::Context;
use crate::typ::Environment;
use crate::typ::ExpectedKind;
use crate::typ::ModuleUnit;
use crate::typ::NameError;
use crate::typ::Named;
use crate::typ::ScopeMemberRef;
use crate::typ::Symbol;
use crate::typ::Type;
use crate::typ::TypecheckError;
use crate::typ::TypecheckResult;
use crate::typ::Typed;
use crate::typ::ValueKind;
use common::span::Span;
use common::span::Spanned;
use std::rc::Rc;

pub type Unit = ast::Unit<Typed>;
pub type UnitDecl = ast::UnitDecl<Typed>;
pub type GlobalBinding = ast::GlobalBinding<Typed>;
pub type GlobalBindingItem = ast::GlobalBindingItem<Typed>;
pub type TypeDecl = ast::TypeDecl<Typed>;
pub type TypeDeclItem = ast::TypeDeclItem<Typed>;

fn typecheck_unit_decl(
    decl: &ast::UnitDecl<Span>,
    ctx: &mut Context,
    visibility: Visibility,
) -> TypecheckResult<UnitDecl> {
    match decl {
        ast::UnitDecl::Uses { decl: uses } => {
            for use_item in &uses.units {
                typecheck_unit_uses_decl(use_item, ctx)?;
            }

            Ok(ast::UnitDecl::Uses { decl: uses.clone() })
        },

        ast::UnitDecl::FunctionDef { def: func_def } => {
            typecheck_unit_func_def(func_def, visibility, ctx)
        },

        ast::UnitDecl::FunctionDecl { decl: func_decl } => {
            typecheck_unit_func_decl(func_decl, visibility, ctx)
        },

        ast::UnitDecl::Type { decl: type_decl } => {
            typecheck_unit_type_decl(type_decl, visibility, ctx)
        },

        ast::UnitDecl::GlobalBinding { decl } => {
            let decl = typecheck_global_binding(decl, visibility, ctx)?;

            Ok(ast::UnitDecl::GlobalBinding { decl })
        }
    }
}

fn typecheck_unit_uses_decl(use_item: &ast::UseDeclItem, ctx: &mut Context) -> TypecheckResult<()> {
    match ctx.find_path(&use_item.ident) {
        // path refers to a known unit path (by alias or directly by its canon name)
        Some(ScopeMemberRef::Scope { path }) => {
            let unit_canon_ident = IdentPath::from_parts(path.keys().cloned());

            ctx.use_unit(unit_canon_ident);
        },

        // path refers to some other decl
        Some(ScopeMemberRef::Decl { value, .. }) => {
            let unexpected = Named::Decl(value.clone());
            let err = NameError::Unexpected {
                ident: use_item.ident.clone(),
                actual: unexpected,
                expected: ExpectedKind::Namespace,
            };
            return Err(TypecheckError::from_name_err(err, use_item.ident.path_span()));
        },

        // path does not exist
        None => {
            return Err(TypecheckError::from_name_err(
                NameError::NotFound {
                    ident: use_item.ident.clone(),
                },
                use_item.ident.path_span(),
            ));
        },
    }

    Ok(())
}

fn typecheck_unit_func_def(
    func_def: &ast::FunctionDef<Span>,
    visibility: Visibility,
    ctx: &mut Context,
) -> TypecheckResult<UnitDecl> {
    let func_def = typecheck_func_def(func_def, ctx)?;
    let func_decl = &func_def.decl;
    let func_name = &func_decl.name;

    match &func_decl.name.explicit_impl {        
        Some(impl_iface) => {
            // calculate the implementor type from a) the interface's definition of the method and
            // b) the arguments provided in equivalent positions
            let iface_name = impl_iface
                .as_iface()
                .expect("implemented type must be an interface");
            let iface_def = ctx
                .find_iface_def(&iface_name)
                .map_err(|err| {
                    TypecheckError::from_name_err(err, func_name.span.clone())
                })?;
            
            let method = iface_def.get_method(&func_name.ident)
                .ok_or_else(|| {
                    let err = NameError::MemberNotFound {
                        base: NameContainer::Type(impl_iface.clone()),
                        member: func_name.ident.clone(),
                    };
                    TypecheckError::from_name_err(err, func_decl.span.clone())
                })?;

            let impl_params = func_decl.params
                .iter()
                .map(|param| param.ty.clone())
                .collect::<Vec<_>>();

            let method_sig = FunctionSig::of_decl(&method.decl);

            let impl_ty = method_sig.self_ty_from_args(&impl_params)
                .cloned()
                .ok_or_else(|| TypecheckError::AmbiguousSelfType {
                    span: func_decl.span.clone(),
                    iface: impl_iface.clone(),
                    method: method.ident().clone(),
                })?;
            
            ctx.define_method_impl(iface_name, impl_ty, func_def.clone())?;
        }

        None => {
            let func_name_path = IdentPath::from(func_name.ident.clone());
            if let Err(NameError::NotFound { .. }) = ctx.find_function(&func_name_path) {
                let func_decl = &func_decl;
                ctx.declare_function(func_name.ident().clone(), func_decl, visibility)?;
            }

            ctx.define_function(func_name.ident().clone(), func_def.clone())?;
        }
    }

    Ok(UnitDecl::FunctionDef { def: func_def })
}

fn typecheck_unit_func_decl(
    func_decl: &ast::FunctionDecl<Span>,
    visibility: Visibility,
    ctx: &mut Context,
) -> TypecheckResult<UnitDecl> {
    let name = func_decl.name.clone();
    let func_decl = typecheck_func_decl(func_decl, ctx)?;

    assert!(
        func_decl.name.explicit_impl.is_none(),
        "not yet implemented: can't forward-declare method impls"
    );

    ctx.declare_function(name.ident().clone(), &func_decl, visibility)?;

    Ok(UnitDecl::FunctionDecl { decl: func_decl })
}

pub fn typecheck_unit_type_decl(
    type_decl: &ast::TypeDecl<Span>,
    visibility: Visibility,
    ctx: &mut Context,
) -> TypecheckResult<UnitDecl> {
    let decl = typecheck_type_decl(type_decl, visibility, ctx)?;

    Ok(UnitDecl::Type { decl })
}

fn typecheck_type_decl(
    type_decl: &ast::TypeDecl<Span>,
    visibility: Visibility,
    ctx: &mut Context,
) -> TypecheckResult<TypeDecl> {
    let mut items = Vec::with_capacity(type_decl.items.len());

    for type_decl_item in &type_decl.items {
        let item = typecheck_type_decl_item(type_decl_item, visibility, ctx)?;
        items.push(item);
    }

    Ok(TypeDecl {
        items,
        span: type_decl.span.clone(),
    })
}

fn typecheck_type_decl_item(
    type_decl: &ast::TypeDeclItem<Span>,
    visibility: Visibility,
    ctx: &mut Context,
) -> TypecheckResult<TypeDeclItem> {
    let decl_name = type_decl.name().clone();
    let full_name = Symbol {
        qualified: ctx.qualify_name(decl_name.ident.clone()),
        decl_name,
        type_args: None,
    };

    // type decls have an inner scope
    let ty_scope = ctx.push_scope(Environment::TypeDecl {
        full_name: full_name.clone(),
    });

    if let Some(decl_name_type_params) = &full_name.decl_name.type_params {
        let type_params = {
            let items: Vec<_> = decl_name_type_params
                .items
                .iter()
                .map(|p| {
                    ast::TypeParam {
                        name: p.clone(),

                        // todo: support type constraints for type decls
                        constraint: None,
                    }
                })
                .collect();

            ast::TypeList::new(items, decl_name_type_params.span().clone())
        };

        let checked_params = typecheck_type_params(&type_params, ctx)?;

        ctx.declare_type_params(&checked_params)?;
    }

    let type_decl = typecheck_type_decl_body(full_name, type_decl, ctx)?;

    ctx.pop_scope(ty_scope);

    match &type_decl {
        TypeDeclItem::Interface(iface) => {
            ctx.declare_iface(iface.clone(), visibility)?;
        },

        TypeDeclItem::Variant(variant) => {
            ctx.declare_variant(variant.clone(), visibility)?;
        },

        TypeDeclItem::Struct(class) => {
            ctx.declare_class(class.clone(), visibility)?;
        },

        TypeDeclItem::Enum(enum_decl) => {
            ctx.declare_enum(enum_decl.clone(), visibility)?;
        }

        TypeDeclItem::Alias(alias) => {
            ctx.declare_type(
                alias.name.decl_name.ident.clone(),
                (*alias.ty).clone(),
                visibility,
            )?;
        },
    }

    Ok(type_decl)
}

fn typecheck_type_decl_body(
    name: Symbol,
    type_decl: &ast::TypeDeclItem<Span>,
    ctx: &mut Context,
) -> TypecheckResult<TypeDeclItem> {
    let type_decl = match type_decl {
        ast::TypeDeclItem::Struct(class) => {
            let class = typecheck_struct_decl(name, class, ctx)?;
            ast::TypeDeclItem::Struct(Rc::new(class))
        },
        ast::TypeDeclItem::Interface(iface) => {
            let iface = typecheck_iface(name, iface, ctx)?;
            ast::TypeDeclItem::Interface(Rc::new(iface))
        },

        ast::TypeDeclItem::Variant(variant) => {
            let variant = typecheck_variant(name, variant, ctx)?;
            ast::TypeDeclItem::Variant(Rc::new(variant))
        },

        ast::TypeDeclItem::Alias(alias) => {
            let alias = typecheck_alias(name, alias, ctx)?;
            ast::TypeDeclItem::Alias(Rc::new(alias))
        },

        ast::TypeDeclItem::Enum(enum_decl) => {
            let enum_decl = typecheck_enum_decl(name, enum_decl, ctx)?;
            ast::TypeDeclItem::Enum(Rc::new(enum_decl))
        }
    };

    Ok(type_decl)
}

fn typecheck_global_binding(
    binding: &ast::GlobalBinding<Span>,
    visibility: Visibility,
    ctx: &mut Context,
) -> TypecheckResult<GlobalBinding> {
    let mut items = Vec::with_capacity(binding.items.len());

    for const_decl_item in &binding.items {
        let item = typecheck_global_binding_item(binding.kind, const_decl_item, visibility, ctx)?;
        items.push(item);
    }

    Ok(GlobalBinding {
        kind: binding.kind,
        items,
        span: binding.span.clone(),
    })
}

fn typecheck_global_binding_item(
    kind: BindingDeclKind,
    item: &ast::GlobalBindingItem<Span>,
    visibility: Visibility,
    ctx: &mut Context,
) -> TypecheckResult<GlobalBindingItem> {
    let span = item.span().clone();

    let (ty, val) = match kind {
        BindingDeclKind::Const => {
            let (ty, const_val_expr) = match (&item.ty, &item.val) {
                (_, None) => {
                    return Err(TypecheckError::ConstDeclWithNoValue { span });
                }

                (Some(explicit_ty), Some(val)) => {
                    // use explicitly provided type
                    let ty = typecheck_type(explicit_ty, ctx)?;
                    let const_val_expr = typecheck_expr(val, &ty, ctx)?;
                    (ty, const_val_expr)
                }

                (None, Some(val)) => {
                    // infer from provided value expr
                    let init_expr = typecheck_expr(val, &Type::Nothing, ctx)?;
                    let ty = init_expr.annotation().ty().into_owned();
                    (ty, init_expr)
                }
            };

            let const_val_literal = match const_val_expr.const_eval(ctx) {
                Some(const_val) => Ok(const_val),
                None => Err(TypecheckError::InvalidConstExpr {
                    expr: Box::new(const_val_expr),
                }),
            }?;

            ctx.declare_const(
                item.ident.clone(),
                const_val_literal.clone(),
                ty.clone(),
                visibility,
                span.clone(),
            )?;

            let annotation = ConstTyped {
                value: const_val_literal.clone(),
                span: span.clone(),
                decl: Some(item.ident.clone()),
                ty: ty.clone(),
            };

            let val = Expr::Literal(const_val_literal.clone(), annotation.into());
            (ty, Some(Box::new(val)))
        }

        BindingDeclKind::Var => {
            let (ty, val) = match (&item.ty, &item.val) {
                (Some(explicit_ty), Some(val_expr)) => {
                    let explicit_ty = typecheck_type(explicit_ty, ctx)?;
                    let val_expr = typecheck_expr(&val_expr, &explicit_ty, ctx)?;

                    (explicit_ty, Some(Box::new(val_expr)))
                },

                (Some(explicit_ty), None) => {
                    let explicit_ty = typecheck_type(explicit_ty, ctx)?;
                    (explicit_ty, None)
                }

                (None, Some(val_expr)) => {
                    let val_expr = typecheck_expr(&val_expr, &Type::Nothing, ctx)?;
                    let actual_ty = val_expr.annotation().ty().into_owned();

                    (actual_ty, Some(Box::new(val_expr)))
                }

                (None, None) => {
                    return Err(TypecheckError::BindingWithNoType {
                        binding_name: item.ident.clone(),
                        span: item.span.clone(),
                    })
                },
            };
            
            // global bindings must always be initialized or be of a default-able type
            if ty.default_val().is_none() && val.is_none() {
                return Err(TypecheckError::UninitGlobalBinding {
                    ident: item.ident.clone(),
                    ty,
                });
            }
            
            ctx.declare_binding(item.ident.clone(), Binding {
                ty: ty.clone(),
                def: Some(item.ident.clone()),
                kind: ValueKind::Mutable,
            })?;

            (ty, val)
        }
    };

    if ty == Type::Nothing {
        return Err(TypecheckError::BindingWithNoType {
            binding_name: item.ident.clone(),
            span,
        });
    }

    Ok(GlobalBindingItem {
        ty: Some(ty),
        ident: item.ident.clone(),
        val,
        span,
    })
}

pub fn typecheck_unit(unit: &ast::Unit<Span>, ctx: &mut Context) -> TypecheckResult<ModuleUnit> {
    ctx.unit_scope(unit.ident.clone(), |ctx| {
        let mut iface_decls = Vec::new();
        for decl in &unit.iface_decls {
            iface_decls.push(typecheck_unit_decl(decl, ctx, Visibility::Interface)?);
        }

        let mut impl_decls = Vec::new();
        for decl in &unit.impl_decls {
            impl_decls.push(typecheck_unit_decl(decl, ctx, Visibility::Implementation)?);
        }

        // init stmt is implicitly a block
        let init = ctx.scope(
            Environment::Block {
                allow_unsafe: false,
            },
            |ctx| {
                let mut init = Vec::new();
                for stmt in &unit.init {
                    let stmt = typecheck_stmt(stmt, &Type::Nothing, ctx)?;
                    expect_stmt_initialized(&stmt, ctx)?;
                    init.push(stmt);
                }

                Ok(init)
            },
        )?;

        let undefined = ctx.undefined_syms();
        if !undefined.is_empty() {
            return Err(TypecheckError::UndefinedSymbols {
                unit: unit.ident.clone(),
                syms: undefined,
            });
        }

        let unit_ctx = ctx.clone();

        let unit = Unit {
            kind: unit.kind,
            ident: unit.ident.clone(),
            init,
            iface_decls,
            impl_decls,
        };

        Ok(ModuleUnit {
            context: unit_ctx,
            unit,
        })
    })
}
