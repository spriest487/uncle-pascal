use std::{rc::Rc};
use node;
use syntax;
use semantic::*;

pub type Program = node::Program<SemanticContext>;

impl Program {
    pub fn annotate(program: &syntax::Program, scope: Rc<Scope>) -> Result<Self, SemanticError> {
        let (decls, program_scope) = Unit::annotate_impls(program.decls.iter(), scope)?;

        let (program_block, _) = Block::annotate(&program.program_block, program_scope.clone())?;
        program_block.type_check()?;

        let program = Program {
            name: program.name.clone(),
            uses: Unit::annotate_uses(program.uses.iter(), program_scope),
            decls,
            program_block,
        };

        Ok(program)
    }
}
