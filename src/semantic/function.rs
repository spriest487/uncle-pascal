use node::{self, Identifier};
use syntax;
use semantic::*;
use types::{DeclaredType, FunctionSignature, RecordKind};

const RESULT_VAR_NAME: &str = "result";

pub type Function = node::FunctionDecl<ScopedSymbol>;

impl Function {
    pub fn annotate(function: &syntax::FunctionDecl,
                    scope: &Scope) -> Result<Self, SemanticError> {
        let return_type = match function.return_type {
            Some(ref func_return_type) => {
                let mut found_type = scope.get_type(&func_return_type.name)
                    .map(|mut return_type| {
                        for _ in 0..func_return_type.indirection {
                            return_type = return_type.pointer();
                        }
                        return_type
                    });

                let return_type = found_type
                    .ok_or_else(|| SemanticError::unknown_type(func_return_type.name.clone(),
                                                               function.context.clone()))?;

                Some(return_type)
            }

            None => None
        };

        /* there can't already be a local symbol called "result" */
        let result_id = Identifier::from(RESULT_VAR_NAME);
        function.local_vars.decls.iter().find(|decl| decl.name == result_id)
            .map(|_| Err(SemanticError::illegal_name(RESULT_VAR_NAME.to_owned(),
                                                     function.context.clone())))
            .unwrap_or(Ok(()))?;

        let mut local_vars = Vars::annotate(&function.local_vars,
                                            &scope,
                                            SemanticVarsKind::Local)?;

        if let &Some(ref result_var_type) = &return_type {
            local_vars.decls.push(VarDecl {
                name: result_id,
                context: function.context.clone(),
                decl_type: result_var_type.clone(),
            });
        }

        let args = Vars::annotate(&function.args, scope, SemanticVarsKind::Local)?;

        let local_scope = scope.clone()
            .with_vars_local(args.decls.iter())
            .with_vars_local(local_vars.decls.iter());

        let body = Block::annotate(&function.body, &local_scope)?;

        let qualified_name = if function.name.namespace.len() == 0{
            local_scope.qualify_local_name(&function.name.name)
        } else {
            return Err(SemanticError::illegal_name(function.name.to_string(),
                                                   function.context.clone()));
        };

        Ok(Function {
            name: qualified_name,
            context: function.context.clone(),
            constructor: function.constructor,
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
        // make sure constructors return something constructible
        if self.constructor {
            match self.return_type.as_ref() {
                Some(DeclaredType::Record(decl)) if decl.kind == RecordKind::Class => {},
                _ => return Err(SemanticError::invalid_constructor_type(self.return_type.clone(),
                                                                        self.context.clone()))
            }
        }

        //todo: check args are all valid types

        self.body.type_check()
    }
}
