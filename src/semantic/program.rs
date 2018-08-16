use node;
use syntax;
use semantic::*;

pub type Program = node::Program<ScopedSymbol>;

impl Program {
    pub fn annotate(program: &syntax::Program, scope: Scope) -> Result<Self, SemanticError> {
        let (decls, program_scope) = Unit::annotate_decls(program.decls.iter(), scope)?;

        let program_block = Block::annotate(&program.program_block, &program_scope)?;
        program_block.type_check()?;

        let program = Program {
            name: program.name.clone(),
            uses: program.uses.clone(),
            decls,
            program_block,
        };

        Ok(program)
    }
}
