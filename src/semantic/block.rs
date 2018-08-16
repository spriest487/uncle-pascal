use node;
use syntax;
use semantic::*;

pub type Block = node::Block<ScopedSymbol>;

impl Block {
    pub fn annotate(block: &syntax::Block, scope: &Scope)
        -> Result<Self, SemanticError> {
        let mut statements = Vec::new();
        let mut local_scope = scope.clone();

        for src_statement in block.statements.iter() {
            let statement = Expression::annotate(src_statement, &local_scope)?;

            /* after each let binding, the scope for following statements
            includes that let binding */
            if statement.is_let_binding() {
                let (name, value) = statement.clone().unwrap_let_binding();

                let inferred_type = value.expr_type()?
                    .ok_or_else(|| {
                        SemanticError::type_not_assignable(None, statement.context.clone())
                    })?;

                local_scope = local_scope.with_symbol_local(&name, inferred_type);
            }
            statements.push(statement);
        }

        Ok(Self {
            statements,
            context: block.context.clone(),
        })
    }

    pub fn type_check(&self) -> Result<(), SemanticError> {
        self.statements.iter()
            .map(|statement| statement.type_check())
            .collect()
    }
}
