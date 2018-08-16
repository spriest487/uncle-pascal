use std::{
    rc::Rc
};

use node;
use syntax;
use semantic::*;

pub type Block = node::Block<SemanticContext>;

impl Block {
    pub fn annotate(block: &syntax::Block, scope: Rc<Scope>) -> Result<Self, SemanticError> {
        let mut statements = Vec::new();
        let mut local_scope = scope.clone();

        for src_statement in block.statements.iter() {
            let statement = Expression::annotate(src_statement, local_scope.clone())?;

            /* after each let binding, the scope for following statements
            includes that let binding */
            if statement.is_let_binding() {
                let (name, value) = statement.clone().unwrap_let_binding();

                let inferred_type = value.expr_type()?
                    .ok_or_else(|| {
                        SemanticError::type_not_assignable(None, statement.context.clone())
                    })?;

                local_scope = Rc::new(local_scope
                    .as_ref()
                    .clone()
                    .with_symbol_local(&name, inferred_type));
            }
            statements.push(statement);
        }

        let block = Block {
            statements,
            context: SemanticContext {
                scope: scope.clone(),
                token: block.context.token().clone(),
            }
        };

        Ok(block)
    }

    pub fn type_check(&self) -> Result<(), SemanticError> {
        self.statements.iter()
            .map(|statement| statement.type_check())
            .collect()
    }
}
