use std::rc::Rc;

use semantic::{
    Expression,
    Scope,
    SemanticContext,
    SemanticResult,
    SemanticError,
    expect_valid_op,
};
use syntax;
use node::{self};
use operators;

pub type ConstDecl = node::ConstDecl<SemanticContext>;

impl ConstDecl {
    pub fn annotate(decl: &syntax::ConstDecl, scope: Rc<Scope>) -> SemanticResult<(Rc<Scope>)> {
        /* todo: explicit type isn't saved in scope yet, we still use the const default type */
        let explicit_type = match decl.decl_type.as_ref() {
            Some(explicit_type_name) => {
                let explicit_type = explicit_type_name.resolve(scope.clone())?;
                Some(explicit_type)
            }
            None => None
        };

        let (value, _) = Expression::annotate(&decl.value, explicit_type.as_ref(), scope.clone())?;
        let const_value = value.to_const_value()?;

        if let Some(expected) = explicit_type.as_ref() {
            expect_valid_op(operators::Assignment, Some(expected), &value, &value.context)
                .map_err(|_| {
                    SemanticError::invalid_const_value(value.clone())
                })?;
        };

        let result_scope = scope.as_ref().clone()
            .with_const(&decl.name, const_value, explicit_type);

        Ok(Rc::new(result_scope))
    }
}

impl ConstDecl {
    pub fn scope(&self) -> &Scope {
        self.context.scope.as_ref()
    }
}