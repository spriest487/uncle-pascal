use std::rc::Rc;

use semantic::*;
use syntax;
use node::{self};

pub type ConstDecl = node::ConstDecl<SemanticContext>;

impl ConstDecl {
    pub fn annotate(decl: &syntax::ConstDecl, scope: Rc<Scope>) -> SemanticResult<(Rc<Scope>)> {
        let (value, _) = Expression::annotate(&decl.value, scope.clone())?;
        let const_value = value.to_const_value()?;

        /* todo: explicit type isn't saved in scope yet, we still use the const default type */
        if let Some(explicit_type_name) = &decl.decl_type {
            let explicit_type = explicit_type_name.resolve(scope.clone())?;
            let actual_type = const_value.value_type();

            if !explicit_type.assignable_from(&actual_type) {
                return Err(SemanticError::invalid_const_value(value))?;
            }
        }

        let result_scope = scope.as_ref().clone()
            .with_const(&decl.name, const_value);

        Ok(Rc::new(result_scope))
    }
}

impl ConstDecl {
    pub fn scope(&self) -> &Scope {
        self.context.scope.as_ref()
    }
}