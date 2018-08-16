use node;
use syntax;
use semantic::*;

pub type Program = node::Program<ScopedSymbol>;

impl Program {
    pub fn annotate(program: &syntax::Program) -> Result<Self, SemanticError> {
        let mut program_scope = Scope::default();

        let mut type_decls = Vec::new();
        for parsed_decl in program.type_decls.iter() {
            let record_decl = RecordDecl::annotate(parsed_decl, &program_scope)?;

            program_scope = program_scope.with_type(record_decl.name.clone(),
                                                    record_decl.record_type());

            type_decls.push(record_decl);
        }

        let vars = Vars::annotate(&program.vars, &program_scope)?;

        let functions = program.functions.iter()
            .map(|f| Function::annotate(f, &program_scope))
            .collect::<Result<Vec<_>, _>>()?;

        program_scope = program_scope
            .with_vars(vars.decls.clone());

        for function in functions.iter().cloned() {
            let sig = function.signature_type();
            program_scope = program_scope.with_symbol(function.name, sig);
        }

        let program_block = Block::annotate(&program.program_block, &program_scope)?;

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
