use node;
use syntax;
use semantic::*;

pub type Program = node::Program<ScopedSymbol>;

impl Program {
    pub fn annotate(program: &syntax::Program) -> Result<Self, SemanticError> {
        let mut program_scope = Scope::default();
        let mut decls = Vec::new();

        for decl in program.decls.iter() {
            match decl {
                &node::UnitDeclaration::Record(ref parsed_decl) => {
                    let record_decl = RecordDecl::annotate(parsed_decl, &program_scope)?;

                    program_scope = program_scope.with_type(record_decl.name.clone(),
                                                            record_decl.record_type());

                    decls.push(node::UnitDeclaration::Record(record_decl))
                }

                &node::UnitDeclaration::Function(ref parsed_func) => {
                    let func_decl = Function::annotate(parsed_func, &program_scope)?;

                    program_scope = program_scope.with_symbol(func_decl.name.clone(),
                                                              func_decl.signature_type());

                    decls.push(node::UnitDeclaration::Function(func_decl))
                }

                &node::UnitDeclaration::Vars(ref parsed_vars) => {
                    let vars = Vars::annotate(parsed_vars, &program_scope)?;
                    program_scope = program_scope.with_vars(vars.decls.iter());
                    decls.push(node::UnitDeclaration::Vars(vars))
                }
            }
        }

        let program_block = Block::annotate(&program.program_block, &program_scope)?;

        Ok(Program {
            name: program.name.clone(),
            uses: program.uses.clone(),
            decls,
            program_block,
        })
    }

    pub fn type_check(&self) -> Result<(), SemanticError> {
        for decl in self.decls.iter() {
            match decl {
                &node::UnitDeclaration::Function(ref func) => {
                    func.type_check()?;
                },
                _ => {}
            }
        }

        self.program_block.type_check()
    }
}
