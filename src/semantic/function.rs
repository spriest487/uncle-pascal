use std::{
    rc::Rc,
};
use node::{
    self,
    Identifier,
    VarModifier,
};
use syntax;
use semantic::*;
use types::{
    Type,
    FunctionSignature
};

const RESULT_VAR_NAME: &str = "result";

pub type FunctionDecl = node::FunctionDecl<ScopedSymbol, SemanticContext>;
pub type FunctionDeclBody = node::FunctionDeclBody<ScopedSymbol, SemanticContext>;

impl FunctionDecl {
    pub fn annotate(function: &syntax::FunctionDecl,
                    scope: Rc<Scope>) -> Result<Self, SemanticError> {
        let context = SemanticContext {
            token: function.context.token().clone(),
            scope: scope.clone(),
        };

        let return_type = match &function.return_type {
            Some(func_return_type) => {
                let found_type = scope.get_type(&func_return_type);

                let return_type = found_type.map_err(|not_found| {
                    SemanticError::unknown_type(not_found, context.clone())
                })?;

                Some(return_type)
            }

            None => None
        };

        let qualified_name = if function.name.namespace.len() == 0 {
            scope.qualify_local_name(&function.name.name)
        } else {
            return Err(SemanticError::illegal_name(function.name.to_string(), context));
        };

        let args = VarDecls::annotate(&function.args, scope.clone(), SemanticVarsKind::Local)?;

        let body = match &function.body {
            Some(function_body) => {
                /* there can't already be a local symbol called "result" */
                let result_id = Identifier::from(RESULT_VAR_NAME);
                function_body.local_vars.decls.iter().find(|decl| decl.name == result_id)
                    .map(|_| Err({
                        SemanticError::illegal_name(RESULT_VAR_NAME.to_owned(), context.clone())
                    }))
                    .unwrap_or(Ok(()))?;

                let mut local_vars = VarDecls::annotate(&function_body.local_vars,
                                                        scope.clone(),
                                                        SemanticVarsKind::Local)?;
                for local_var in local_vars.decls.iter() {
                    match local_var.modifier {
                        Some(bad_modifier @ VarModifier::Out) |
                        Some(bad_modifier @ VarModifier::Var) =>
                            return Err(SemanticError::invalid_var_modifier(
                                bad_modifier,
                                local_var.context.clone())),

                        Some(VarModifier::Const) |
                        None =>
                            ()
                    }
                }

                if let &Some(ref result_var_type) = &return_type {
                    local_vars.decls.push(VarDecl {
                        name: result_id,
                        context: context.clone(),
                        modifier: Some(VarModifier::Out),
                        decl_type: result_var_type.clone(),
                    });
                }

                let local_scope = Rc::new(
                    scope.as_ref().clone()
                        .with_vars_local(args.decls.iter())
                        .with_vars_local(local_vars.decls.iter())
                );

                let body_block = Block::annotate(&function_body.block, local_scope.clone())?;

                Some(FunctionDeclBody {
                    block: body_block,
                    local_vars,
                })
            }
            None => None,
        };

        Ok(FunctionDecl {
            name: qualified_name,
            context,
            kind: function.kind,
            return_type,
            modifiers: function.modifiers.clone(),
            args,
            body,
        })
    }

    pub fn type_check(&self) -> Result<(), SemanticError> {
        let returns_class = self.return_type.as_ref()
            .map(|ty| ty.is_class())
            .unwrap_or(false);

        // make sure constructors return something constructible
        if self.kind == node::FunctionKind::Constructor && !returns_class {
            return Err(SemanticError::invalid_constructor_type(self.return_type.clone(),
                                                               self.context.clone()));
        }

        //make sure destructors take one arg of a type in their module, and return nothing
        if self.kind == node::FunctionKind::Destructor {
            if let Some(return_type) = &self.return_type {
                return Err(SemanticError::invalid_destructor_return(return_type.clone(),
                                                                    self.context.clone()));
            }

            let is_valid_destructed_type = |ty: &Type| {
                match ty {
                    Type::Class(class_name) => class_name.namespace == self.name.namespace,
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
                arg_record.name == class_type.name.to_string(),
            _ =>
                false,
        }
    }

    pub fn scope(&self) -> &Scope {
        self.context.scope.as_ref()
    }

    pub fn signature(&self) -> FunctionSignature {
        FunctionSignature {
            arg_types: self.args.decls.iter()
                .map(|decl| decl.decl_type.clone())
                .collect(),
            return_type: self.return_type.clone(),
            modifiers: self.modifiers.clone(),
        }
    }
}
