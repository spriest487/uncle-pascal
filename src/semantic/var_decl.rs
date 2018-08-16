use node;
use syntax;
use semantic::*;

pub type VarDecl = node::VarDecl<Symbol>;

impl Into<Symbol> for VarDecl {
    fn into(self) -> Symbol {
        Symbol::new(node::Identifier::parse(&self.name), self.decl_type)
    }
}

impl VarDecl {
    pub fn annotate(decl: &syntax::VarDecl, scope: &Scope) -> Result<Self, SemanticError> {
        let var_type = scope.get_type(&decl.decl_type)
            .cloned()
            .ok_or_else(|| SemanticError::UnknownType(decl.decl_type.clone()))?;

        Ok(VarDecl {
            name: decl.name.clone(),
            decl_type: var_type,
        })
    }
}

pub type Vars = node::Vars<Symbol>;

impl Vars {
    pub fn annotate(vars: &syntax::Vars, scope: &Scope) -> Result<Self, SemanticError> {
        let decls = vars.decls.iter()
            .map(|v| -> Result<VarDecl, SemanticError> {
                VarDecl::annotate(v, scope)
            })
            .collect::<Result<_, _>>()?;

        Ok(Self {
            decls
        })
    }
}
