use crate::ast::const_eval::ConstEval;
use crate::ast::prelude::*;
use crate::ModuleUnit;

pub type Unit = ast::Unit<TypeAnnotation>;
pub type UnitDecl = ast::UnitDecl<TypeAnnotation>;
pub type ConstDecl = ast::ConstDecl<TypeAnnotation>;

fn typecheck_unit_decl(decl: &ast::UnitDecl<Span>, ctx: &mut Context) -> TypecheckResult<UnitDecl> {
    match decl {
        ast::UnitDecl::Uses { decl: uses } => {
            for unit in &uses.units {
                typecheck_unit_uses_decl(unit, ctx)?;
            }

            Ok(ast::UnitDecl::Uses { decl: uses.clone() })
        }

        ast::UnitDecl::FunctionDef {
            decl: func_def,
            visibility,
        } => typecheck_unit_func_def(func_def, *visibility, ctx),

        ast::UnitDecl::FunctionDecl {
            decl: func_decl,
            visibility,
        } => typecheck_unit_func_decl(func_decl, *visibility, ctx),

        ast::UnitDecl::Type {
            decl: type_decl,
            visibility,
        } => typecheck_unit_type_decl(type_decl, *visibility, ctx),

        ast::UnitDecl::Const { decl, visibility } => {
            let decl = typecheck_const_decl(decl, *visibility, ctx)?;

            Ok(ast::UnitDecl::Const {
                decl,
                visibility: *visibility,
            })
        }
    }
}

fn typecheck_unit_uses_decl(unit_path: &IdentPath, ctx: &mut Context) -> TypecheckResult<()> {
    match ctx.find_path(&unit_path) {
        // path refers to a known unit path (by alias or directly by its canon name)
        Some(ScopeMemberRef::Namespace { path }) => {
            let unit_canon_ident = IdentPath::from_parts(path.keys().cloned());

            ctx.use_unit(unit_canon_ident);
        }

        // path refers to some other decl
        Some(ScopeMemberRef::Value { value, .. }) => {
            let unexpected = Named::Decl(value.clone());
            let err = NameError::Unexpected {
                ident: unit_path.clone(),
                actual: unexpected,
                expected: ExpectedKind::Namespace,
            };
            return Err(TypecheckError::from(err));
        }

        // path does not exist
        None => {
            return Err(TypecheckError::from(NameError::NotFound(unit_path.last().clone())));
        }
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
        if let Err(NameError::NotFound(..)) = ctx.find_function(&func_def.decl.ident) {
            let func_decl = &func_def.decl;
            ctx.declare_function(func_def.decl.ident.last().clone(), func_decl, visibility)?;
        }

        ctx.define_function(name, func_def.clone(), visibility)?;
    }

    Ok(UnitDecl::FunctionDef {
        decl: func_def,
        visibility,
    })
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

    Ok(ast::UnitDecl::FunctionDecl {
        decl: func_decl,
        visibility,
    })
}

fn typecheck_unit_type_decl(
    type_decl: &ast::TypeDecl<Span>,
    visibility: Visibility,
    ctx: &mut Context,
) -> TypecheckResult<UnitDecl> {
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

    let type_decl = typecheck_type_decl(full_name, type_decl, ctx)?;

    ctx.pop_scope(ty_scope);

    match &type_decl {
        ast::TypeDecl::Interface(iface) => {
            ctx.declare_iface(iface.clone(), visibility)?;
        }

        ast::TypeDecl::Variant(variant) => {
            ctx.declare_variant(variant.clone(), visibility)?;
        }

        ast::TypeDecl::Class(class) => {
            ctx.declare_class(class.clone(), visibility)?;
        }
    }

    Ok(ast::UnitDecl::Type {
        decl: type_decl,
        visibility,
    })
}

fn typecheck_const_decl(
    decl: &ast::ConstDecl<Span>,
    visibility: Visibility,
    ctx: &mut Context,
) -> TypecheckResult<ConstDecl> {
    let span = decl.span().clone();

    let (ty, const_val_expr) = match &decl.ty {
        Some(explicit_ty) => {
            // use explicitly provided type
            let ty = typecheck_type(explicit_ty, ctx)?;
            let const_val_expr = typecheck_expr(&decl.val, &ty, ctx)?;

            (ty, const_val_expr)
        }
        None => {
            // infer from provided value expression
            let const_val_expr = typecheck_expr(&decl.val, &Type::Nothing, ctx)?;
            let ty = const_val_expr.annotation().ty().into_owned();

            (ty, const_val_expr)
        }
    };

    let const_val_literal = match const_val_expr.const_eval(ctx) {
        Some(const_val) => Ok(const_val),
        None => Err(TypecheckError::InvalidConstExpr {
            expr: Box::new(const_val_expr),
        }),
    }?;

    ctx.declare_const(
        decl.ident.clone(),
        const_val_literal.clone(),
        ty.clone(),
        visibility,
        span.clone(),
    )?;

    let const_val = Expression::Literal(
        const_val_literal.clone(),
        ConstAnnotation {
            value: const_val_literal.clone(),
            span: span.clone(),
            decl: Some(span.clone()),
            ty: ty.clone(),
        }.into(),
    );

    Ok(ast::ConstDecl {
        ty: Some(ty),
        ident: decl.ident.clone(),
        val: Box::new(const_val),
        span,
    })
}

pub fn typecheck_unit(unit: &ast::Unit<Span>, ctx: &mut Context) -> TypecheckResult<ModuleUnit> {
    ctx.unit_scope(unit.ident.clone(), |ctx| {
        let mut decls = Vec::new();
        for decl in &unit.decls {
            decls.push(typecheck_unit_decl(decl, ctx)?);
        }

        let mut init = Vec::new();
        for stmt in &unit.init {
            let stmt = typecheck_stmt(stmt, &Type::Nothing, ctx)?;
            expect_stmt_initialized(&stmt, ctx)?;
            init.push(stmt);
        }

        let undefined = ctx.undefined_syms();
        if !undefined.is_empty() {
            return Err(TypecheckError::UndefinedSymbols {
                unit: unit.ident.clone(),
                syms: undefined,
            });
        }

        let unit_ctx = ctx.clone();

        let unit = Unit {
            ident: unit.ident.clone(),
            init,
            decls,
        };

        Ok(ModuleUnit {
            context: unit_ctx,
            unit,
        })
    })
}
