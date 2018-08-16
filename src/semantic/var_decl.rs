use std::{
    rc::Rc,
};
use types::Type;
use node;
use syntax;
use semantic::*;

pub type VarDecl = node::VarDecl<SemanticContext>;

impl VarDecl {
    pub fn annotate(decl: &syntax::VarDecl,
                    scope: Rc<Scope>,
                    mut binding_kind: BindingKind)
                    -> SemanticResult<(Self, Rc<Scope>)> {
        let var_context = SemanticContext {
            scope: scope.clone(),
            token: decl.context.token().clone(),
        };
        let var_type: Type = decl.decl_type.resolve(scope.clone())?;

        let default_value = match decl.default_value.as_ref() {
            Some(default_expr) => {
                let (default_val, _) = Expression::annotate(default_expr, scope.clone())?;
                Some(default_val.into_const_expr()?)
            }
            None => None,
        };

        // todo: we always default-initialize records right now, but we shouldn't
        if var_type.is_record() {
            binding_kind = BindingKind::Mutable;
        }

        let new_scope = Rc::new(scope.as_ref().clone()
            .with_binding(&decl.name, var_type.clone(), binding_kind));

        Ok((VarDecl {
            name: decl.name.clone(),
            context: var_context,
            decl_type: var_type,
            default_value,
        }, new_scope))
    }

    pub fn annotate_all(decls: &[syntax::VarDecl],
                        mut scope: Rc<Scope>,
                        binding_kind: BindingKind)
                        -> SemanticResult<(Vec<Self>, Rc<Scope>)> {
        let mut vars = Vec::new();
        for var in decls.iter() {
            let (var, new_scope) = VarDecl::annotate(var, scope, binding_kind)?;

            vars.push(var);
            scope = new_scope;
        }

        Ok((vars, scope))
    }

    pub fn scope(&self) -> &Scope {
        self.context.scope.as_ref()
    }
}
