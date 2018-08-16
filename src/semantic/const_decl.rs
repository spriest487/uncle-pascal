use std::rc::Rc;

use semantic::*;
use syntax;
use node::{self};

pub type ConstDecls = node::ConstDecls<SemanticContext>;
pub type ConstDecl = node::ConstDecl<SemanticContext>;

impl ConstDecl {
    pub fn annotate(decl: &syntax::ConstDecl, scope: Rc<Scope>) -> SemanticResult<(Self, Scope)> {
        let context = SemanticContext {
            token: decl.context.token().clone(),
            scope: scope.clone(),
        };

        let (value, _) = Expression::annotate(&decl.value, scope.clone())?;
        let const_value = value.to_const_value()?;

        let result_scope = scope.as_ref().clone()
            .with_const(&decl.name, const_value);

        let decl_type = match &decl.decl_type {
            None => None,
            Some(type_name) => Some(type_name.resolve(scope.clone())?),
        };

        let result_decl = ConstDecl {
            name: decl.name.clone(),
            value,
            decl_type,
            context,
        };

        Ok((result_decl, result_scope))
    }
}

impl ConstDecl {
    pub fn scope(&self) -> &Scope {
        self.context.scope.as_ref()
    }
}