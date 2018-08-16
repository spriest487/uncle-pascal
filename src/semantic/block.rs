use node;
use syntax;
use semantic::*;

pub type Block = node::Block<ScopedSymbol>;

impl Block {
    pub fn annotate(block: &syntax::Block, scope: &Scope)
        -> Result<Self, SemanticError> {
        let statements = block.statements.iter()
            .map(|statement| {
                Expression::annotate(statement, scope)
            })
            .collect::<Result<_, _>>()?;

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
