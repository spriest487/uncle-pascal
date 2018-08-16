use std::{
    rc::Rc,
};
use types::Symbol;
use node;
use syntax;
use semantic::*;

pub type VarDecl = node::VarDecl<ScopedSymbol, SemanticContext>;
pub type VarDecls = node::VarDecls<ScopedSymbol, SemanticContext>;

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
    pub fn annotate(decl: &syntax::VarDecl,
                    scope: Rc<Scope>,
                    kind: SemanticVarsKind)
                    -> Result<Self, SemanticError> {
        let var_context = SemanticContext {
            scope: scope.clone(),
            token: decl.context.token().clone(),
        };
        let var_type = scope.get_type(&decl.decl_type)
            .map_err(|not_found| {
                SemanticError::unknown_type(not_found, var_context.clone())
            })?;

        let qualified_name = if decl.name.namespace.len() != 0 {
            return Err(SemanticError::illegal_name(decl.name.to_string(), var_context));
        } else {
            match kind {
                SemanticVarsKind::Local => decl.name.clone(),
                SemanticVarsKind::Namespaced => scope.qualify_local_name(&decl.name.name),
            }
        };

        Ok(VarDecl {
            name: qualified_name,
            context: var_context,
            decl_type: var_type,
        })
    }

    pub fn scope(&self) -> &Scope {
        self.context.scope.as_ref()
    }
}

impl VarDecls {
    pub fn annotate(vars: &syntax::VarDecls,
                    scope: Rc<Scope>,
                    kind: SemanticVarsKind) -> SemanticResult<Self> {
        let decls = vars.decls.iter()
            .map(|v| -> Result<VarDecl, SemanticError> {
                VarDecl::annotate(v, scope.clone(), kind)
            })
            .collect::<Result<_, _>>()?;

        Ok(Self {
            decls
        })
    }
}
