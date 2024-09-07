use crate::ast::{expect_stmt_initialized, typecheck_alias, typecheck_struct_decl, typecheck_expr, typecheck_func_decl, typecheck_func_def, typecheck_iface, typecheck_stmt, typecheck_variant, Expr, typecheck_enum_decl};
use crate::{ast::const_eval::ConstEval, typecheck_type, typecheck_type_params, Binding, ConstAnnotation, Context, Environment, ExpectedKind, ModuleUnit, NameError, Named, ScopeMemberRef, Symbol, Type, TypeAnnotation, TypecheckError, TypecheckResult, ValueKind};
use pas_common::span::{Span, Spanned};
use pas_syn::{ast, ast::Visibility, IdentPath};
use std::rc::Rc;
use pas_syn::ast::BindingDeclKind;

pub type Unit = ast::Unit<TypeAnnotation>;
pub type UnitDecl = ast::UnitDecl<TypeAnnotation>;
pub type GlobalBinding = ast::GlobalBinding<TypeAnnotation>;
pub type GlobalBindingItem = ast::GlobalBindingItem<TypeAnnotation>;
pub type TypeDecl = ast::TypeDecl<TypeAnnotation>;
pub type TypeDeclItem = ast::TypeDeclItem<TypeAnnotation>;

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
    let name = func_def.decl.ident.single().clone();

    let func_def = typecheck_func_def(func_def, ctx)?;
    if let Some(impl_iface) = &func_def.decl.impl_iface {
        let iface_decl = impl_iface
            .iface
            .as_iface()
            .expect("implemented type must be an interface");

        ctx.define_method_impl(iface_decl, impl_iface.for_ty.clone(), func_def.clone())?;
    } else {
        if let Err(NameError::NotFound { .. }) = ctx.find_function(&func_def.decl.ident) {
            let func_decl = &func_def.decl;
            ctx.declare_function(func_def.decl.ident.last().clone(), func_decl, visibility)?;
        }

        ctx.define_function(name, func_def.clone())?;
    }

    Ok(UnitDecl::FunctionDef { def: func_def })
}

fn typecheck_unit_func_decl(
    func_decl: &ast::FunctionDecl<Span>,
    visibility: Visibility,
    ctx: &mut Context,
) -> TypecheckResult<UnitDecl> {
    let name = func_decl.ident.single().clone();
    let func_decl = typecheck_func_decl(func_decl, ctx)?;

    assert!(
        func_decl.impl_iface.is_none(),
        "not yet implemented: can't forward-declare method impls"
    );

    ctx.declare_function(name.clone(), &func_decl, visibility)?;

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
    // type decls have an inner scope
    let ty_scope = ctx.push_scope(Environment::TypeDecl);

    let decl_name = type_decl.ident().clone();
    let full_name = Symbol {
        qualified: ctx.qualify_name(decl_name.ident.clone()),
        decl_name,
        type_args: None,
    };

    if let Some(decl_name_type_params) = &full_name.decl_name.type_params {
        let type_params = {
            let items: Vec<_> = decl_name_type_params
                .items
                .iter()
                .map(|p| {
                    ast::TypeParam {
                        ident: p.clone(),

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

            let annotation = ConstAnnotation {
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
