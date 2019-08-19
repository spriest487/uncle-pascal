use crate::ast::prelude::*;

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
                            let aliased = aliased_unit.clone().child(decl_key.clone());
                            ctx.declare_alias(decl_key, aliased)?;
                        }
                    }

                    Some(MemberRef::Value { value, .. }) => {
                        let unexpected = UnexpectedValue::Decl(value.clone());
                        let err = NameError::ExpectedNamespace(unit.clone().into(), unexpected);
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

                ctx.define_method_impl(iface_decl, impl_iface.for_ty.clone(), name)?;
            } else {
                if let Err(NameError::NotFound(..)) = ctx.find_function(&func_def.decl.ident) {
                    let func_decl = &func_def.decl;
                    ctx.declare_function(func_def.decl.ident.last().clone(), func_decl, *visibility)?;
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
            let ty_scope = ctx.push_scope(None);

            let decl_name = type_decl.ident().clone();
            let full_name = QualifiedDeclName {
                qualified: ctx.qualify_name(decl_name.ident.clone()),
                decl_name,
                type_args: Vec::new(),
            };

            for (pos, name) in full_name.decl_name.type_params.iter().enumerate() {
                let param_ty = TypeParam {
                    pos,
                    name: name.clone(),
                };
                ctx.declare_type(
                    name.clone(),
                    Type::GenericParam(param_ty),
                    Visibility::Private,
                )?;
            }

            let type_decl = typecheck_type_decl(full_name, type_decl, ctx)?;

            ctx.pop_scope(ty_scope);

            let decl_ty = Type::of_decl(&type_decl);
            ctx.declare_type(
                type_decl.ident().decl_name.ident.clone(),
                decl_ty,
                *visibility,
            )?;

            Ok(ast::UnitDecl::Type {
                decl: type_decl,
                visibility: *visibility,
            })
        }
    }
}

pub fn typecheck_unit(unit: &ast::Unit<Span>, ctx: &mut Context) -> TypecheckResult<Unit> {
    let unit_scope = ctx.push_scope(Some(unit.ident.clone()));

    let mut decls = Vec::new();
    for decl in &unit.decls {
        decls.push(typecheck_unit_decl(decl, ctx)?);
    }

    let mut init = Vec::new();
    for stmt in &unit.init {
        let stmt = typecheck_stmt(stmt, ctx)?;
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

    ctx.pop_scope(unit_scope);

    Ok(Unit {
        ident: unit.ident.clone(),
        init,
        decls,
    })
}
