use {
    crate::ast::prelude::*
};

pub type Unit = ast::Unit<TypeAnnotation>;
pub type UnitDecl = ast::UnitDecl<TypeAnnotation>;

fn typecheck_unit_decl(decl: &ast::UnitDecl<Span>, ctx: &mut Context) -> TypecheckResult<UnitDecl> {
    match decl {
        ast::UnitDecl::Function(func_decl) => {
            let func_decl = typecheck_func_decl(func_decl, ctx)?;
            let sig = FunctionSig::of_decl(&func_decl).clone();
            ctx.declare_function(func_decl.ident.clone(), sig)?;
            Ok(ast::UnitDecl::Function(func_decl))
        }

        ast::UnitDecl::Type(type_decl) => {
            let type_decl = typecheck_type_decl(type_decl, ctx)?;
            let class_ty = Type::of_decl(&type_decl);
            ctx.declare_type(type_decl.ident().clone(), class_ty)?;
            Ok(ast::UnitDecl::Type(type_decl))
        }
    }
}

pub fn typecheck_unit(unit: &ast::Unit<Span>) -> TypecheckResult<Unit> {
    let mut unit_ctx = Context::root();

    let mut decls = Vec::new();
    for decl in &unit.decls {
        decls.push(typecheck_unit_decl(decl, &mut unit_ctx)?);
    }

    let mut init = Vec::new();
    for stmt in &unit.init {
        init.push(typecheck_stmt(stmt, &mut unit_ctx)?)
    }

    Ok(Unit {
        init,
        decls,
    })
}
