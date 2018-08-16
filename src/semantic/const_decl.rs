use std::rc::Rc;

use semantic::*;
use syntax;
use node::{self};

pub type ConstDecls = node::ConstDecls<ScopedSymbol, SemanticContext>;
pub type ConstDecl = node::ConstDecl<ScopedSymbol, SemanticContext>;

impl ConstDecl {
    pub fn annotate(decl: &syntax::ConstDecl, scope: &mut Scope) -> SemanticResult<Self> {
        let context = SemanticContext {
            token: decl.context.token().clone(),
            scope: Rc::new(scope.clone()),
        };

        let value = Expression::annotate(&decl.value, Rc::new(scope.clone()))?;
        let const_value = value.to_const_value()?;

        *scope = scope.clone().with_const(&decl.name, const_value);

        let decl_type = match &decl.decl_type {
            None => None,
            Some(type_name) => {
                let ty = scope.get_type(&type_name)
                    .map_err(|not_found| {
                        SemanticError::unknown_type(not_found, context.clone())
                    })?;

                Some(ty)
            }
        };

        Ok(ConstDecl {
            name: decl.name.clone(),
            value,
            decl_type,
            context,
        })
    }
}

impl ConstDecl {
    pub fn scope(&self) -> &Scope {
        self.context.scope.as_ref()
    }
}