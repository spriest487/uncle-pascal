use std::{
    rc::Rc,
};
use types::TypedSymbol;
use node::{self, Identifier};
use syntax;
use semantic::*;

pub type VarDecl = node::VarDecl<SemanticContext>;
pub type VarDecls = node::VarDecls<SemanticContext>;

impl Into<TypedSymbol> for VarDecl {
    fn into(self) -> TypedSymbol {
        TypedSymbol::new(Identifier::from(&self.name), self.decl_type)
    }
}

impl VarDecl {
    pub fn annotate(decl: &syntax::VarDecl, scope: Rc<Scope>) -> SemanticResult<Self> {
        let var_context = SemanticContext {
            scope: scope.clone(),
            token: decl.context.token().clone(),
        };
        let var_type = decl.decl_type.resolve(scope.clone())?;

        let default_value = match decl.default_value.as_ref() {
            Some(default_expr) => {
                let (default_val, _) = Expression::annotate(default_expr, scope.clone())?;
                Some(default_val.into_const_expr()?)
            }
            None => None,
        };

        Ok(VarDecl {
            name: decl.name.clone(),
            context: var_context,
            decl_type: var_type,
            default_value,
        })
    }

    pub fn scope(&self) -> &Scope {
        self.context.scope.as_ref()
    }
}

impl VarDecls {
    pub fn annotate(vars: &syntax::VarDecls,
                    mut scope: Rc<Scope>)
                    -> SemanticResult<(Self, Rc<Scope>)> {
        let decls: Vec<VarDecl> = vars.decls.iter()
            .map(|v| VarDecl::annotate(v, scope.clone()))
            .collect::<SemanticResult<_>>()?;

        for var in decls.iter() {
            scope = Rc::new(scope.as_ref().clone()
                .with_global_var(&var.name, var.decl_type.clone()));
        }

        Ok((Self { decls }, scope))
    }
}
