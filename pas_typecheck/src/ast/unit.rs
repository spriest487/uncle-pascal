use crate::ast::prelude::*;

pub type Unit = ast::Unit<TypeAnnotation>;
pub type UnitDecl = ast::UnitDecl<TypeAnnotation>;

fn typecheck_unit_decl(decl: &ast::UnitDecl<Span>, ctx: &mut Context) -> TypecheckResult<UnitDecl> {
    match decl {
        ast::UnitDecl::Uses(uses) => unimplemented!("uses {:#?}", uses),

        ast::UnitDecl::FunctionDef(func_def) => {
            let func_def = typecheck_func_def(func_def, ctx)?;
            if let Some(impl_iface) = &func_def.decl.impl_iface {
                ctx.define_method_impl(
                    impl_iface.iface.clone(),
                    impl_iface.for_ty.clone(),
                    func_def.decl.ident.clone(),
                )?;
            } else {
                ctx.define_function(
                    func_def.decl.ident.clone(),
                    FunctionSig::of_decl(&func_def.decl).clone(),
                    func_def.decl.span.clone(),
                )?;
            }

            Ok(ast::UnitDecl::FunctionDef(func_def))
        },

        ast::UnitDecl::FunctionDecl(func_decl) => {
            let func_decl = typecheck_func_decl(func_decl, ctx)?;
            let sig = FunctionSig::of_decl(&func_decl).clone();
            ctx.declare_function(func_decl.ident.clone(), sig)?;
            Ok(ast::UnitDecl::FunctionDecl(func_decl))
        },

        ast::UnitDecl::Type(type_decl) => {
            let type_decl = typecheck_type_decl(type_decl, ctx)?;
            let decl_ty = Type::of_decl(&type_decl);
            ctx.declare_type(type_decl.ident().last().clone(), decl_ty)?;
            Ok(ast::UnitDecl::Type(type_decl))
        },
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
        init.push(typecheck_stmt(stmt, ctx)?)
    }

    ctx.pop_scope(unit_scope)?;

    Ok(Unit {
        ident: unit.ident.clone(),
        init,
        decls,
    })
}
