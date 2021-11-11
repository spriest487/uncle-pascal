use crate::ast::prelude::*;
use crate::ModuleUnit;

pub type Unit = ast::Unit<TypeAnnotation>;
pub type UnitDecl = ast::UnitDecl<TypeAnnotation>;

fn typecheck_unit_decl(decl: &ast::UnitDecl<Span>, ctx: &mut Context) -> TypecheckResult<UnitDecl> {
    match decl {
        ast::UnitDecl::Uses { decl: uses } => {
            for unit in &uses.units {
                match ctx.resolve(&IdentPath::from(unit.clone())) {
                    Some(MemberRef::Namespace { path }) => {
                        let aliased_unit = IdentPath::from_parts(path.keys().cloned());

                        for decl_key in path.top().keys() {
                            let new_alias = aliased_unit.clone().child(decl_key.clone());
                            let decl_path = ctx.resolve_alias(&new_alias).unwrap();

                            if ctx.is_accessible(&new_alias) {
                                // don't re-export names imported as aliases in the unit
                                let reexport = decl_path.parent().as_ref() != Some(&aliased_unit);

                                if !reexport {
                                    ctx.declare_alias(decl_key, new_alias)?;
                                }
                            }
                        }
                    }

                    Some(MemberRef::Value { value, .. }) => {
                        let unexpected = Named::Decl(value.clone());
                        let err = NameError::Unexpected {
                            ident: unit.clone().into(),
                            actual: unexpected,
                            expected: ExpectedKind::Namespace,
                        };
                        return Err(TypecheckError::from(err));
                    }

                    None => {
                        return Err(TypecheckError::from(NameError::NotFound(unit.clone())));
                    }
                }
            }

            Ok(ast::UnitDecl::Uses { decl: uses.clone() })
        }

        ast::UnitDecl::FunctionDef {
            decl: func_def,
            visibility,
        } => {
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
                    ctx.declare_function(
                        func_def.decl.ident.last().clone(),
                        func_decl,
                        *visibility,
                    )?;
                }

                ctx.define_function(name, func_def.clone(), *visibility)?;
            }

            Ok(ast::UnitDecl::FunctionDef {
                decl: func_def,
                visibility: *visibility,
            })
        }

        ast::UnitDecl::FunctionDecl {
            decl: func_decl,
            visibility,
        } => {
            let name = func_decl.ident.single().clone();
            let func_decl = typecheck_func_decl(func_decl, ctx)?;

            assert!(
                func_decl.impl_iface.is_none(),
                "not yet implemented: can't forward-declare method impls"
            );

            ctx.declare_function(name.clone(), &func_decl, *visibility)?;

            Ok(ast::UnitDecl::FunctionDecl {
                decl: func_decl,
                visibility: *visibility,
            })
        }

        ast::UnitDecl::Type {
            decl: type_decl,
            visibility,
        } => {
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
                    let items: Vec<_> = decl_name_type_params.items.iter()
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
                    ctx.declare_iface(iface.clone(), *visibility)?;
                }

                ast::TypeDecl::Variant(variant) => {
                    ctx.declare_variant(variant.clone(), *visibility)?;
                }

                ast::TypeDecl::Class(class) => {
                    ctx.declare_class(class.clone(), *visibility)?;
                }
            }

            Ok(ast::UnitDecl::Type {
                decl: type_decl,
                visibility: *visibility,
            })
        }
    }
}

pub fn typecheck_unit(unit: &ast::Unit<Span>, ctx: &mut Context) -> TypecheckResult<ModuleUnit> {
    let unit_scope = ctx.push_scope(Environment::Module {
        namespace: unit.ident.clone(),
    });

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

    ctx.pop_scope(unit_scope);

    let unit = Unit {
        ident: unit.ident.clone(),
        init,
        decls,
    };

    Ok(ModuleUnit {
        context: unit_ctx,
        unit,
    })
}
