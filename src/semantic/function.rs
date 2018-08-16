use node;
use syntax;
use semantic::*;

const RESULT_VAR_NAME : &str = "result";

pub type Function = node::Function<Symbol>;

impl Function {
    pub fn annotate(function: &syntax::Function, scope: &Scope) -> Result<Self, SemanticError> {
        let return_type = scope.get_type(&function.return_type)
            .cloned()
            .ok_or_else(|| SemanticError::UnknownType(function.return_type.clone()))?;

        /* there can't already be a local symbol called "result" */
        function.local_vars.decls.iter().find(|decl| decl.name == RESULT_VAR_NAME)
            .map(|_| Err(SemanticError::IllegalName(RESULT_VAR_NAME.to_owned())))
            .unwrap_or(Ok(()))?;

        let mut local_vars = Vars::annotate(&function.local_vars, &scope)?;
        local_vars.decls.push(VarDecl {
            name: RESULT_VAR_NAME.to_owned(),
            decl_type: return_type.clone()
        });

        let args = Vars::annotate(&function.args, scope)?;

        let local_scope = scope.clone()
            .with_symbols(args.decls.clone())
            .with_symbols(local_vars.decls.clone());

        let body = Block::annotate(&function.body, &local_scope)?;

        Ok(Function {
            name: function.name.clone(),
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
