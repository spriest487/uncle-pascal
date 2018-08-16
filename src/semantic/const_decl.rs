use std::rc::Rc;

use semantic::*;
use syntax;
use node::{self};

pub type ConstDecls = node::ConstDecls<ScopedSymbol, SemanticContext>;
pub type ConstDecl = node::ConstDecl<ScopedSymbol, SemanticContext>;

impl ConstDecls {
    pub fn annotate(const_decls: &syntax::ConstDecls, scope: Rc<Scope>) -> SemanticResult<Self> {
        let decls = const_decls.decls.iter()
            .map(|decl| {
                ConstDecl::annotate(decl, scope.clone())
            })
            .collect::<SemanticResult<_>>()?;

        Ok(ConstDecls {
            decls
        })
    }
}

impl ConstDecl {
    pub fn annotate(decl: &syntax::ConstDecl, scope: Rc<Scope>) -> SemanticResult<Self> {
        let context = SemanticContext {
            token: decl.context.token().clone(),
            scope: scope.clone(),
        };

        let value = Expression::annotate(&decl.value, scope)?;

        Ok(ConstDecl {
            name: decl.name.clone(),
            value,
            context,
        })
    }
}

impl ConstDecl {
    pub fn scope(&self) -> &Scope {
        self.context.scope.as_ref()
    }
}