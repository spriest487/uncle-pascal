use {
    crate::{
        result::*,
        context::Context,
        ast::*,
        TypeAnnotation,
    },
    pas_syn::{
        Span,
        ast,
    },
};

pub type Unit = ast::Unit<TypeAnnotation>;
pub type UnitNode = ast::Unit<TypeAnnotation>;

pub fn typecheck_unit(unit: &ast::Unit<Span>) -> TypecheckResult<UnitNode> {
    let mut unit_ctx = Context::root();

    let mut init = Vec::new();
    for stmt in &unit.init {
        init.push(typecheck_stmt(stmt, &mut unit_ctx)?)
    }

    Ok(Unit {
        init
    })
}
