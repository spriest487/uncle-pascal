use std::{
    rc::Rc,
};
use types::Type;
use node::{
    self,
    Context,
};
use syntax;
use semantic::{
    Declaration,
    Scope,
    SemanticContext,
    SemanticResult,
    Expression,
    BindingKind,
};

pub type VarDecl = node::VarDecl<SemanticContext>;

impl VarDecl {
    pub fn annotate(decl: &syntax::VarDecl,
                    scope: &Rc<Scope>,
                    binding_kind: BindingKind)
                    -> SemanticResult<(Self, Rc<Scope>)> {
        let var_context = SemanticContext {
            scope: scope.clone(),
            token: decl.context.token().clone(),
            type_hint: None,
        };
        let var_type: Type = decl.decl_type.resolve(scope.clone())?;

        let default_value = match decl.default_value.as_ref() {
            Some(default_expr) => {
                let (default_val, _) = Expression::annotate(
                    default_expr,
                    Some(&var_type),
                    scope.clone())?;
                Some(default_val.into_const_expr(Some(&var_type))?)
            }
            None => None,
        };

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
            let (var, new_scope) = VarDecl::annotate(var, &scope, binding_kind)?;

            vars.push(var);
            scope = new_scope;
        }

        Ok((vars, scope))
    }
}

impl Declaration for VarDecl {
    fn local_name(&self) -> &str {
        &self.name
    }

    fn context(&self) -> &SemanticContext {
        &self.context
    }
}
