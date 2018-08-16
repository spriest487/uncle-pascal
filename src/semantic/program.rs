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

        let mut program_scope = global_scope
            .with_symbols(vars.decls.clone())
            .with_symbols(functions.iter().cloned().map(|f| {
                let identifier = node::Identifier::parse(&f.name);
                let sig_type = f.signature_type();

                Symbol::new(identifier, sig_type)
            }));

        let mut type_decls = Vec::new();
        for parsed_decl in program.type_decls.iter() {
            let record_decl = RecordDecl::annotate(parsed_decl, &program_scope)?;

            program_scope = program_scope.with_type(node::Identifier::parse(&record_decl.name),
                                                    record_decl.record_type());

            type_decls.push(record_decl);
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
