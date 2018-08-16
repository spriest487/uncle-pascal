use std::{rc::Rc};
use node;
use syntax;
use semantic::*;

pub type Program = node::Program<SemanticContext>;

impl Program {
    pub fn annotate(program: &syntax::Program, scope: Scope) -> Result<Self, SemanticError> {
        let (decls, program_scope) = Unit::annotate_impls(program.decls.iter(), scope)?;
        let program_scope = Rc::new(program_scope);

        let program_block = Block::annotate(&program.program_block, program_scope.clone())?;
        program_block.type_check()?;

        let program = Program {
            name: program.name.clone(),
            uses: Unit::annotate_uses(program.uses.iter(), program_scope.clone()),
            decls,
            program_block,
        };

        Ok(program)
    }
}
