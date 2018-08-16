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

pub type FunctionDecl = node::FunctionDecl<SemanticContext>;
pub type Function = node::Function<SemanticContext>;
pub type FunctionArg = node::FunctionArg<SemanticContext>;

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
                    default_value: match arg.default_value.as_ref() {
                        Some(default_expr) => {
                            let (val, _) = Expression::annotate(default_expr, scope.clone())?;
                            Some(val.into_const_expr()?)
                        }
                        None => None
                    },
                })
            })
            .collect::<SemanticResult<_>>()?;

        Ok(FunctionDecl {
            name: function.name.clone(),
            kind: function.kind.clone(),
            modifiers: function.modifiers.clone(),
            context,
            args,
            return_type,
        })
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
            args: self.args.iter()
                .map(|decl| FunctionArgSignature {
                    decl_type: decl.decl_type.clone(),
                    modifier: decl.modifier.clone(),
                })
                .collect(),
            return_type: self.return_type.clone(),
            modifiers: self.modifiers.clone(),
        }
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
                    Type::Class(class_name) => {
                        class_name.parent().as_ref() == self.scope().local_namespace()
                    }
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

        Ok(())
    }
}

impl Function {
    pub fn annotate(function: &syntax::Function, scope: Rc<Scope>) -> SemanticResult<Function> {
        let decl = FunctionDecl::annotate(&function.decl, scope.clone())?;

        /* there can't already be a local symbol called "result" */
        function.local_vars().find(|decl| decl.name == RESULT_VAR_NAME)
            .map(|_| Err({
                SemanticError::illegal_name(RESULT_VAR_NAME.to_owned(), decl.context.clone())
            }))
            .unwrap_or(Ok(()))?;

        let mut local_decls = Vec::new();

        let mut local_scope = scope.as_ref().clone();

        for local_decl in function.local_decls.iter() {
            match local_decl {
                node::FunctionLocalDecl::Vars(_) => {
                    /* local vars are skipped here, other decls can't refer to them
                    so it's easiest just to process them last */
                }

                node::FunctionLocalDecl::Consts(parsed_local_consts) => {
                    let mut local_consts = ConstDecls::default();

                    //consts can reference each other so we annotate them one by one
                    for parsed_const in parsed_local_consts.decls.iter() {
                        let (const_decl, new_scope) = ConstDecl::annotate(
                            parsed_const,
                            Rc::new(local_scope))?;

                        local_consts.decls.push(const_decl);
                        local_scope = new_scope;
                    }

                    local_decls.push(node::FunctionLocalDecl::Consts(local_consts))
                }

                node::FunctionLocalDecl::NestedFunction(parsed_local_func) => {
                    let func_scope = Rc::new(local_scope.clone());
                    let local_func = Function::annotate(&parsed_local_func, func_scope)?;
                    let local_func = Box::new(local_func);

                    let func_decl = node::FunctionLocalDecl::NestedFunction(local_func);
                    local_decls.push(func_decl);
                }
            }
        }

        /* include args in local scope before variables */
        for arg in decl.args.iter() {
            let arg_type: Type = arg.decl_type.clone();
            let binding_kind = match arg.modifier {
                Some(node::FunctionArgModifier::Const) => BindingKind::Immutable,
                Some(node::FunctionArgModifier::Var) => BindingKind::Mutable,
                Some(node::FunctionArgModifier::Out) => BindingKind::Uninitialized,
                None => BindingKind::Mutable,
            };
            local_scope = local_scope.with_binding(&arg.name, arg_type, binding_kind);
        }

        /* annotate variables and add them to the local scope */
        let mut all_local_vars = VarDecls::default();

        for parsed_local_var in function.local_vars() {
            let local_var = VarDecl::annotate(parsed_local_var, Rc::new(local_scope.clone()))?;
            all_local_vars.decls.push(local_var);
        }

        for local_var in all_local_vars.decls.iter() {
            let binding_kind = match local_var.decl_type {
                Type::Record(_) => BindingKind::Mutable,
                _ => BindingKind::Uninitialized,
            };

            local_scope = local_scope.with_binding(
                &local_var.name,
                local_var.decl_type.clone(),
                binding_kind,
            );
        }

        /* add a "result" var if this function returns something */
        if let &Some(ref result_var_type) = &decl.return_type {
            all_local_vars.decls.push(VarDecl {
                name: RESULT_VAR_NAME.to_string(),
                context: decl.context.clone(),
                default_value: None,
                decl_type: result_var_type.clone(),
            });

            /* todo: we should be able to mark `result` as uninitialized! */
            local_scope = local_scope.with_binding(
                RESULT_VAR_NAME,
                result_var_type.clone(),
                BindingKind::Mutable,
            );
        }

        /* add the "result" var */

        local_decls.push(node::FunctionLocalDecl::Vars(all_local_vars));

        /* we don't keep anything from the local scope after the function */
        let (block, _) = Block::annotate(&function.block, Rc::new(local_scope))?;

        Ok(Function {
            decl,
            block,
            local_decls,
        })
    }

    pub fn type_check(&self) -> Result<(), SemanticError> {
        self.decl.type_check()?;
        self.block.type_check()?;

        Ok(())
    }
}

impl Function {
    pub fn scope(&self) -> &Scope {
        self.decl.scope()
    }
}

impl FunctionArg {
    pub fn scope(&self) -> &Scope {
        &self.context.scope
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use tokenizer;
    use syntax::{TokenStream, Parse};
    use opts::CompileOptions;
    use node::FunctionArgModifier;

    fn parse_func(src: &str, scope: Scope) -> FunctionDecl {
        let tokens = tokenizer::tokenize("test", src, &CompileOptions::default())
            .unwrap();

        let decl = syntax::FunctionDecl::parse(&mut TokenStream::from(tokens))
            .unwrap();

        FunctionDecl::annotate(&decl, Rc::new(scope))
            .unwrap()
    }

    #[test]
    fn sig_of_func_includes_modifier() {
        let scope = Scope::default()
            .with_type_alias("int", Type::Int32);

        let fn_src = "procedure a(out y: int; var x: int; const z: int)";

        let func = parse_func(fn_src, scope);
        let sig = func.signature();

        assert_eq!(Some(FunctionArgModifier::Out), sig.args[0].modifier);
        assert_eq!(Some(FunctionArgModifier::Var), sig.args[1].modifier);
        assert_eq!(Some(FunctionArgModifier::Const), sig.args[2].modifier);
    }
}