use std::{
    rc::Rc,
};
use types::Symbol;
use node::{self, Identifier};
use syntax;
use semantic::*;

pub type VarDecl = node::VarDecl<ScopedSymbol, SemanticContext>;
pub type VarDecls = node::VarDecls<ScopedSymbol, SemanticContext>;

impl Into<Symbol> for VarDecl {
    fn into(self) -> Symbol {
        Symbol::new(Identifier::from(&self.name), self.decl_type)
    }
}

impl VarDecl {
    pub fn annotate(decl: &syntax::VarDecl, scope: Rc<Scope>)
                    -> Result<Self, SemanticError> {
        let var_context = SemanticContext {
            scope: scope.clone(),
            token: decl.context.token().clone(),
        };
        let var_type = scope.get_type(&decl.decl_type)
            .map_err(|not_found| {
                SemanticError::unknown_type(not_found, var_context.clone())
            })?;

        Ok(VarDecl {
            name: decl.name.clone(),
            context: var_context,
            decl_type: var_type,
        })
    }

    pub fn scope(&self) -> &Scope {
        self.context.scope.as_ref()
    }
}

impl VarDecls {
    pub fn annotate(vars: &syntax::VarDecls, scope: Rc<Scope>) -> SemanticResult<Self> {
        let decls = vars.decls.iter()
            .map(|v| -> Result<VarDecl, SemanticError> {
                VarDecl::annotate(v, scope.clone())
            })
            .collect::<Result<_, _>>()?;

        Ok(Self {
            decls
        })
    }
}
