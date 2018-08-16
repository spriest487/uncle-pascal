use node;
use syntax;
use semantic::*;

pub type Program = node::Program<Symbol>;

impl Program {
    pub fn annotate(program: &syntax::Program) -> Result<Self, SemanticError> {
        let global_scope = Scope::default();

        let vars = Vars::annotate(&program.vars, &global_scope)?;

        let functions = program.functions.iter()
            .map(|f| Function::annotate(f, &global_scope))
            .collect::<Result<Vec<_>, _>>()?;

        let program_scope = global_scope
            .with_symbols(vars.decls.clone())
            .with_symbols(functions.iter().cloned().map(|f| {
                let identifier = node::Identifier::parse(&f.name);
                let sig_type = f.signature_type();

                Symbol::new(identifier, sig_type)
            }));

        let program_block = Block::annotate(&program.program_block, &program_scope)?;

        let type_decls = program.type_decls.iter()
            .map(|decl| RecordDecl::annotate(decl, &program_scope))
            .collect::<Result<_, _>>()?;

        Ok(Program {
            name: program.name.clone(),
            uses: program.uses.clone(),
            vars,
            functions,
            type_decls,
            program_block,
        })
    }

    pub fn type_check(&self) -> Result<(), SemanticError> {
        for func in self.functions.iter() {
            func.type_check()?;
        }

        self.program_block.type_check()
    }
}
