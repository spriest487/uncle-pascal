use node;
use syntax;
use semantic::*;

pub type VarDecl = node::VarDecl<ScopedSymbol>;

impl Into<Symbol> for VarDecl {
    fn into(self) -> Symbol {
        Symbol::new(node::Identifier::from(self.name.as_str()), self.decl_type)
    }
}

impl VarDecl {
    pub fn annotate(decl: &syntax::VarDecl, scope: &Scope) -> Result<Self, SemanticError> {
        let base_var_type = scope.get_type(&decl.decl_type)
            .cloned()
            .ok_or_else(|| SemanticError::unknown_type(decl.decl_type.clone(),
                                                       decl.context.clone()))?;

        let var_type = if decl.modifiers.contains(&node::VarModifier::Pointer) {
            base_var_type.pointer()
        } else {
            base_var_type
        };

        Ok(VarDecl {
            name: decl.name.clone(),
            context: decl.context.clone(),
            decl_type: var_type,
            modifiers: decl.modifiers.clone(),
        })
    }
}

pub type Vars = node::VarDecls<ScopedSymbol>;

impl Vars {
    pub fn annotate(vars: &syntax::VarDecls, scope: &Scope) -> Result<Self, SemanticError> {
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
