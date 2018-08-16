use std::{
    rc::Rc,
};
use linked_hash_map::LinkedHashMap;

use node;
use syntax;
use semantic::*;

pub type Program = node::Program<SemanticContext>;

impl Program {
    pub fn annotate(program: &syntax::Program,
                    available_units: &LinkedHashMap<String, ModuleUnit>)
                    -> Result<(Self, Rc<Scope>), SemanticError> {
        let program_scope = Scope::new_root();
        let uses = Unit::annotate_uses(program.uses.iter(), &Rc::new(program_scope.clone()));

        let program_scope = Rc::new(Unit::reference_uses(
            program_scope,
            &uses,
            available_units
        )?);

        let (decls, program_scope) = Unit::annotate_impls(program.decls.iter(), program_scope)?;

        let (program_block, program_scope) = Block::annotate(&program.program_block, &program_scope.clone())?;
        program_block.type_check()?;

        let program = Program {
            name: program.name.clone(),
            uses,
            decls,
            program_block,
        };

        Ok((program, program_scope))
    }
}
