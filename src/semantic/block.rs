use std::{
    rc::Rc
};

use node;
use syntax;
use semantic::*;

pub type Block = node::Block<SemanticContext>;

impl Block {
    pub fn annotate(block: &syntax::Block, scope: Rc<Scope>) -> SemanticResult<(Self, Rc<Scope>)> {
        let mut statements = Vec::new();

        let mut inner_scope = scope.clone();

        for src_statement in block.statements.iter() {
            let (statement, new_scope) = Expression::annotate(src_statement, inner_scope)?;
            inner_scope = new_scope;
            statements.push(statement);
        }

        let block = Block {
            statements,
            context: SemanticContext {
                scope: scope.clone(),
                token: block.context.token().clone(),
            }
        };

        /* we don't want to propagate names outside the inner scope, but we do want to propagate
        initialization */
        let mut scope_out = scope.clone();
        for name in inner_scope.initialized_since(scope.as_ref()) {
            let new_scope = scope_out.as_ref().clone().initialize_symbol(name);
            scope_out = Rc::new(new_scope);
        }

        Ok((block, scope_out))
    }

    pub fn type_check(&self) -> Result<(), SemanticError> {
        self.statements.iter()
            .map(|statement| statement.type_check())
            .collect()
    }
}
