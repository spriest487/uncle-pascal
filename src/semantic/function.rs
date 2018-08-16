use node::{self, Identifier};
use syntax;
use semantic::*;
use types::{Type, FunctionSignature, RecordKind};

const RESULT_VAR_NAME: &str = "result";

pub type Function = node::FunctionDecl<ScopedSymbol>;
pub type FunctionDeclBody = node::FunctionDeclBody<ScopedSymbol>;

impl Function {
    pub fn annotate(function: &syntax::FunctionDecl,
                    scope: &Scope) -> Result<Self, SemanticError> {
        let return_type = match &function.return_type {
            Some(func_return_type) => {
                let found_type = scope.get_type(&func_return_type);

                let return_type = found_type
                    .ok_or_else(|| SemanticError::unknown_type(func_return_type.clone(),
                                                               function.context.clone()))?;

                Some(return_type)
            }

            None => None
        };

        let qualified_name = if function.name.namespace.len() == 0 {
            scope.qualify_local_name(&function.name.name)
        } else {
            return Err(SemanticError::illegal_name(function.name.to_string(),
                                                   function.context.clone()));
        };

        let args = Vars::annotate(&function.args, scope, SemanticVarsKind::Local)?;

        let body = match &function.body {
            Some(function_body) => {
                /* there can't already be a local symbol called "result" */
                let result_id = Identifier::from(RESULT_VAR_NAME);
                function_body.local_vars.decls.iter().find(|decl| decl.name == result_id)
                    .map(|_| Err(SemanticError::illegal_name(RESULT_VAR_NAME.to_owned(),
                                                             function.context.clone())))
                    .unwrap_or(Ok(()))?;

                let mut local_vars = Vars::annotate(&function_body.local_vars,
                                                    &scope,
                                                    SemanticVarsKind::Local)?;

                if let &Some(ref result_var_type) = &return_type {
                    local_vars.decls.push(VarDecl {
                        name: result_id,
                        context: function.context.clone(),
                        decl_type: result_var_type.clone(),
                    });
                }

                let local_scope = scope.clone()
                    .with_vars_local(args.decls.iter())
                    .with_vars_local(local_vars.decls.iter());

                let body_block = Block::annotate(&function_body.block, &local_scope)?;

                Some(FunctionDeclBody {
                    block: body_block,
                    local_vars,
                })
            }
            None => None,
        };

        Ok(Function {
            name: qualified_name,
            context: function.context.clone(),
            kind: function.kind,
            return_type,
            args,
            body,
        })
    }

    pub fn signature_type(&self) -> Type {
        let sig = FunctionSignature {
            return_type: self.return_type.clone(),
            name: self.name.clone(),
            arg_types: self.args.decls.iter().map(|arg| arg.decl_type.clone()).collect(),
        };

        Type::Function(Box::from(sig))
    }

    pub fn type_check(&self) -> Result<(), SemanticError> {
        // make sure constructors return something constructible
        if self.kind == node::FunctionKind::Constructor {
            match self.return_type.as_ref() {
                Some(Type::Record(decl)) if decl.kind == RecordKind::Class => {}
                _ => return Err(SemanticError::invalid_constructor_type(self.return_type.clone(),
                                                                        self.context.clone()))
            }
        }

        //make sure destructors take one arg of a type in their module, and return nothing
        if self.kind == node::FunctionKind::Destructor {
            if let Some(return_type) = &self.return_type {
                return Err(SemanticError::invalid_destructor_return(return_type.clone(),
                                                                    self.context.clone()));
            }

            let is_valid_destructed_type = |ty: &Type| {
                match ty {
                    Type::Record(record) => {
                        let is_class = record.kind == RecordKind::Class;
                        let is_in_same_unit = record.name.namespace == self.name.namespace;

                        is_class && is_in_same_unit
                    }

                    _ => false,
                }
            };

            if self.args.decls.len() != 1
                || !is_valid_destructed_type(&self.args.decls[0].decl_type) {
                let arg_types = self.args.decls.iter()
                    .map(|arg_decl| &arg_decl.decl_type)
                    .cloned();

                return Err(SemanticError::invalid_destructor_args(arg_types,
                                                                  self.context.clone()));
            }
        }

        //todo: check args are all valid types

        if let Some(body) = &self.body {
            body.block.type_check()?;
        }

        Ok(())
    }

    pub fn is_destructor_of(&self, class_type: &RecordDecl) -> bool {
        if self.kind != node::FunctionKind::Destructor {
            return false;
        }

        match self.args.decls.iter().next().map(|arg| &arg.decl_type) {
            Some(Type::Record(arg_record)) =>
                arg_record.name == class_type.name,
            _ =>
                false,
        }
    }
}
