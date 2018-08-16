use node;
use syntax;
use semantic::*;

const RESULT_VAR_NAME : &str = "result";

pub type Function = node::FunctionDecl<ScopedSymbol>;

impl Function {
    pub fn annotate(function: &syntax::Function, scope: &Scope) -> Result<Self, SemanticError> {
        let return_type = match function.return_type {
            Some(ref func_return_type) => {
                let found_type = scope.get_type(func_return_type)
                    .cloned()
                    .ok_or_else(|| SemanticError::unknown_type(func_return_type.clone(),
                        function.context.clone()))?;

                Some(found_type)
            }

            None => None
        };

        /* there can't already be a local symbol called "result" */
        function.local_vars.decls.iter().find(|decl| decl.name == RESULT_VAR_NAME)
            .map(|_| Err(SemanticError::illegal_name(RESULT_VAR_NAME.to_owned(),
                function.context.clone())))
            .unwrap_or(Ok(()))?;

        let mut local_vars = Vars::annotate(&function.local_vars, &scope)?;

        if let &Some(ref result_var_type) = &return_type {
            local_vars.decls.push(VarDecl {
                name: RESULT_VAR_NAME.to_owned(),
                context: function.context.clone(),
                decl_type: result_var_type.clone()
            });
        }

        let args = Vars::annotate(&function.args, scope)?;

        let local_scope = scope.clone()
            .with_vars(args.decls.iter())
            .with_vars(local_vars.decls.iter());

        let body = Block::annotate(&function.body, &local_scope)?;

        Ok(Function {
            name: function.name.clone(),
            context: function.context.clone(),
            return_type,
            local_vars,
            args,
            body,
        })
    }

    pub fn signature_type(&self) -> DeclaredType {
        let sig = FunctionSignature {
            return_type: self.return_type.clone(),
            name: self.name.clone(),
            arg_types: self.args.decls.iter().map(|arg| arg.decl_type.clone()).collect(),
        };

        DeclaredType::Function(Box::from(sig))
    }

    pub fn type_check(&self) -> Result<(), SemanticError> {
        //todo: check args are all valid types

        self.body.type_check()
    }
}