use std::rc::Rc;

use semantic::*;
use syntax;
use node::{self};

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
        let actual_type = const_value.value_type();

        let valid_assignment = explicit_type.as_ref()
            .map(|ty| ty.assignable_from(&actual_type))
            .unwrap_or(true);
        if !valid_assignment {
            return Err(SemanticError::invalid_const_value(value))?;
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