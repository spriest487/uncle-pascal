use std::{
    rc::Rc,
};
use node::{
    self,
};
use syntax;
use semantic::*;
use types::{
    Type,
    FunctionSignature,
};

const RESULT_VAR_NAME: &str = "result";

pub type FunctionDecl = node::FunctionDecl<ScopedSymbol, SemanticContext>;
pub type FunctionDeclBody = node::FunctionDeclBody<ScopedSymbol, SemanticContext>;
pub type FunctionArg = node::FunctionArg<ScopedSymbol, SemanticContext>;

impl FunctionDecl {
    pub fn annotate(function: &syntax::FunctionDecl,
                    scope: Rc<Scope>) -> Result<Self, SemanticError> {
        let context = SemanticContext {
            token: function.context.token().clone(),
            scope: scope.clone(),
        };

        let return_type = match &function.return_type {
            Some(func_return_type) => {
                Some(func_return_type.resolve(scope.clone())?)
            }

            None => None
        };

        let qualified_name = if function.name.namespace.len() == 0 {
            scope.qualify_local_name(&function.name.name)
        } else {
            return Err(SemanticError::illegal_name(function.name.to_string(), context));
        };

        let args: Vec<FunctionArg> = function.args.iter()
            .map(|arg| {
                let arg_context = SemanticContext {
                    token: context.token().clone(),
                    scope: scope.clone(),
                };

                let arg_type = arg.decl_type.resolve(scope.clone())?;

                Ok(node::FunctionArg {
                    name: arg.name.clone(),
                    decl_type: arg_type,
                    context: arg_context,
                    modifier: arg.modifier.clone(),
                })
            })
            .collect::<SemanticResult<_>>()?;

        let body = match &function.body {
            Some(function_body) => {
                /* there can't already be a local symbol called "result" */
                let result_id = RESULT_VAR_NAME;
                function_body.local_vars.decls.iter().find(|decl| decl.name == result_id)
                    .map(|_| Err({
                        SemanticError::illegal_name(RESULT_VAR_NAME.to_owned(), context.clone())
                    }))
                    .unwrap_or(Ok(()))?;

                let mut local_vars = VarDecls::annotate(&function_body.local_vars, scope.clone())?;
//                let mut local_consts = ConstDecls::annotate(&function_body.local_consts, scope.clone())?;
                let mut local_consts = ConstDecls::default();

                if let &Some(ref result_var_type) = &return_type {
                    local_vars.decls.push(VarDecl {
                        name: result_id.to_string(),
                        context: context.clone(),
                        decl_type: result_var_type.clone(),
                        default_value: None,
                    });
                }

                let local_scope = Rc::new({
                    let mut local_scope = scope.as_ref().clone();
                    for arg in args.iter() {
                        let arg_type: Type = arg.decl_type.clone();
                        local_scope = local_scope.with_symbol_local(&arg.name, arg_type);
                    }
                    for parsed_const in function_body.local_consts.decls.iter() {
                        //consts can reference each other so we annotate them one by one
                        let (const_decl, new_scope) = ConstDecl::annotate(parsed_const,
                                                                          Rc::new(local_scope))?;
                        local_consts.decls.push(const_decl);
                        local_scope = new_scope;
                    }
                    for var in local_vars.decls.iter() {
                        local_scope = local_scope.with_symbol_local(&var.name,
                                                                    var.decl_type.clone());
                    }
                    local_scope
                });

                let body_block = Block::annotate(&function_body.block, local_scope.clone())?;

                Some(FunctionDeclBody {
                    block: body_block,
                    local_vars,
                    local_consts,
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

            if self.args.len() != 1
                || !is_valid_destructed_type(&self.args[0].decl_type) {
                let arg_types = self.args.iter()
                    .map(|arg_decl| &arg_decl.decl_type)
                    .cloned();

                return Err(SemanticError::invalid_destructor_args(arg_types, self.context.clone()));
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

        match self.args.iter().next().map(|arg| &arg.decl_type) {
            Some(Type::Record(arg_record)) =>
                arg_record.name == class_type.name,
            _ =>
                false,
        }
    }

    pub fn scope(&self) -> &Scope {
        self.context.scope.as_ref()
    }

    pub fn signature(&self) -> FunctionSignature {
        FunctionSignature {
            arg_types: self.args.iter()
                .map(|decl| decl.decl_type.clone())
                .collect(),
            return_type: self.return_type.clone(),
            modifiers: self.modifiers.clone(),
        }
    }
}

impl FunctionArg {
    pub fn scope(&self) -> &Scope {
        &self.context.scope
    }
}