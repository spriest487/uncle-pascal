use types::Symbol;
use node;
use syntax;
use semantic::*;

pub type VarDecl = node::VarDecl<ScopedSymbol>;

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub enum SemanticVarsKind {
    Local,
    Namespaced,
}

impl Into<Symbol> for VarDecl {
    fn into(self) -> Symbol {
        Symbol::new(self.name, self.decl_type)
    }
}

impl VarDecl {
    pub fn annotate(decl: &syntax::VarDecl, scope: &Scope, kind: SemanticVarsKind) -> Result<Self, SemanticError> {
        let base_var_type = scope.get_type(&decl.decl_type)
            .ok_or_else(|| SemanticError::unknown_type(decl.decl_type.clone(),
                                                       decl.context.clone()))?;

        let var_type = if decl.modifiers.contains(&node::VarModifier::Pointer) {
            base_var_type.pointer()
        } else {
            base_var_type
        };

        let qualified_name = if decl.name.namespace.len() != 0 {
            return Err(SemanticError::illegal_name(decl.name.to_string(), decl.context.clone()))
        } else {
            match kind {
                SemanticVarsKind::Local => decl.name.clone(),
                SemanticVarsKind::Namespaced => scope.qualify_local_name(&decl.name.name),
            }
        };

        Ok(VarDecl {
            name: qualified_name,
            context: decl.context.clone(),
            decl_type: var_type,
            modifiers: decl.modifiers.clone(),
        })
    }
}

pub type Vars = node::VarDecls<ScopedSymbol>;

impl Vars {
    pub fn annotate(vars: &syntax::VarDecls, scope: &Scope, kind: SemanticVarsKind) -> Result<Self, SemanticError> {
        let decls = vars.decls.iter()
            .map(|v| -> Result<VarDecl, SemanticError> {
                VarDecl::annotate(v, scope, kind)
            })
            .collect::<Result<_, _>>()?;

        Ok(Self {
            decls
        })
    }
}
