use std::{
    rc::Rc,
};
use node::{
    self,
    FunctionArgSignature,
    FunctionArgModifier,
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
                    scope: Rc<Scope>)
                    -> SemanticResult<(Self, Rc<Scope>)> {
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

                let default_value = match arg.default_value.as_ref() {
                    Some(default_expr) => {
                        let (val, _) = Expression::annotate(
                            default_expr,
                            Some(&arg_type),
                            scope.clone(),
                        )?;
                        Some(val.into_const_expr()?)
                    }
                    None => None
                };

                Ok(node::FunctionArg {
                    name: arg.name.clone(),
                    decl_type: arg_type,
                    context: arg_context,
                    modifier: arg.modifier.clone(),
                    default_value,
                })
            })
            .collect::<SemanticResult<_>>()?;

        let func_decl = FunctionDecl {
            name: function.name.clone(),
            kind: function.kind.clone(),
            modifiers: function.modifiers.clone(),
            context,
            args,
            return_type,
        };

        let scope = scope.as_ref().clone()
            .with_function(func_decl.clone());

        Ok((func_decl, Rc::new(scope)))
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
                        class_name.parent().as_ref() == self.scope().unit_namespace()
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
    pub fn annotate(function: &syntax::Function, scope: Rc<Scope>) -> SemanticResult<(Function, Rc<Scope>)> {
        let (decl, scope) = FunctionDecl::annotate(&function.decl, scope)?;

        /* there can't already be a local symbol called "result" */
        function.local_vars().find(|decl| decl.name == RESULT_VAR_NAME)
            .map(|_| Err({
                SemanticError::illegal_name(RESULT_VAR_NAME.to_owned(), decl.context.clone())
            }))
            .unwrap_or(Ok(()))?;

        let mut local_decls = Vec::new();
        let mut local_scope = Rc::new(Scope::new_local(scope.as_ref()));

        for local_decl in function.local_decls.iter() {
            match local_decl {
                node::FunctionLocalDecl::Var(_) => {
                    /* local vars are skipped here, other decls can't refer to them
                    so it's easiest just to process them last */
                }

                node::FunctionLocalDecl::Const(parsed_const) => {
                    local_scope = ConstDecl::annotate(parsed_const, local_scope)?;
                }

                node::FunctionLocalDecl::NestedFunction(parsed_local_func) => {
                    let (local_func, new_scope) = Function::annotate(&parsed_local_func, local_scope)?;
                    local_scope = new_scope;

                    let func_decl = node::FunctionLocalDecl::NestedFunction(Box::new(local_func));
                    local_decls.push(func_decl);
                }
            }
        }

        /* include args in local scope before variables */
        for arg in decl.args.iter() {
            let arg_type: Type = arg.decl_type.clone();
            let binding_kind = match arg.modifier {
                Some(FunctionArgModifier::Const) => BindingKind::Immutable,
                Some(FunctionArgModifier::Var) => BindingKind::Mutable,
                Some(FunctionArgModifier::Out) => BindingKind::Uninitialized,
                None => BindingKind::Mutable,
            };
            local_scope = Rc::new(local_scope
                .as_ref().clone()
                .with_binding(&arg.name, arg_type, binding_kind));
        }

        /* annotate variables and add them to the local scope */
        let mut all_local_vars = Vec::new();

        for parsed_local_var in function.local_vars() {
            let (local_var, new_scope) = VarDecl::annotate(
                parsed_local_var,
                local_scope.clone(),
                BindingKind::Uninitialized,
            )?;

            all_local_vars.push(local_var);
            local_scope = new_scope;
        }

        /* add a "result" var if this function returns something */
        if let Some(ref result_var_type) = &decl.return_type {
            local_scope = Rc::new(local_scope.as_ref().clone().with_binding(
                RESULT_VAR_NAME,
                result_var_type.clone(),
                BindingKind::Uninitialized,
            ));
        }

        /* add the "result" var */

        local_decls.extend(all_local_vars
            .into_iter()
            .map(|var| node::FunctionLocalDecl::Var(var)));

        /* we don't keep anything from the local scope after the function */
        let (block, after_block_scope) = Block::annotate(&function.block, local_scope)?;

//        if decl.return_type.is_some() {
//            let result = after_block_scope
//                .get_symbol(&Identifier::from(RESULT_VAR_NAME))
//                .unwrap();
//
//            if !result.initialized() {
//                return Err(SemanticError::output_uninitialized(
//                    RESULT_VAR_NAME,
//                    block.context.clone()
//                ));
//            }
//        }

        let function = Function {
            decl,
            block,
            local_decls,
        };

        function.check_outputs_initialized(after_block_scope.as_ref())?;

        Ok((function, scope))
    }

    fn check_outputs_initialized(&self, scope: &Scope) -> SemanticResult<()> {
        let outputs = self.decl.args.iter()
            // all `out` parameters
            .filter_map(|arg| match arg.modifier {
                | Some(FunctionArgModifier::Out)
                => Some(arg.name.clone()),

                | Some(_)
                | None
                => None,
            });

        // the `result` var, if present
        let return_type = self.decl.return_type.as_ref()
            .map(|_| RESULT_VAR_NAME.to_string());

        for output_name in outputs.chain(return_type) {
            /* todo: it's currently permitted to write a let binding with the same name
            this would shadow the out var and break this check!! */
            let out_var = scope.get_symbol(&Identifier::from(&output_name))
                .unwrap();

            if !out_var.initialized() {
                return Err(SemanticError::output_uninitialized(
                    output_name,
                    self.block.context.clone()
                ));
            }
        }

        Ok(())
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
    use syntax::{
        test::try_parse,
        test::try_parse_record,
    };
    use node::FunctionArgModifier;

    fn parse_func(src: &str, scope: Scope) -> FunctionDecl {
        let decl: syntax::FunctionDecl = try_parse(src).unwrap();

        FunctionDecl::annotate(&decl, Rc::new(scope))
            .unwrap()
            .0
    }

    fn try_parse_func_def(src: &str, scope: Rc<Scope>) -> SemanticResult<(Function, Rc<Scope>)> {
        let func: syntax::Function = try_parse(src).unwrap();
        Function::annotate(&func, scope)
    }

    #[test]
    fn sig_of_func_includes_modifier() {
        let scope = Scope::new_root()
            .with_type_alias("int", Type::Int32);

        let fn_src = "procedure a(out y: int; var x: int; const z: int)";

        let func = parse_func(fn_src, scope);
        let sig = func.signature();

        assert_eq!(Some(FunctionArgModifier::Out), sig.args[0].modifier);
        assert_eq!(Some(FunctionArgModifier::Var), sig.args[1].modifier);
        assert_eq!(Some(FunctionArgModifier::Const), sig.args[2].modifier);
    }

    #[test]
    fn function_with_uninitialized_result_is_err() {
        let scope = Scope::new_root();
        let result = try_parse_func_def(
            r"function x(): System.Int32
            begin
            end",
            Rc::new(scope),
        );

        match result {
            Err(SemanticError { kind: SemanticErrorKind::OutputUninitialized(name), .. }) => {
                assert_eq!("result", name);
            }

            _ => panic!("expected OutputUninitialized error, got {:?}", result)
        }
    }

    #[test]
    fn function_with_uninitialized_record_result_is_err() {
        let scope = Scope::new_root();

        let record_decl: syntax::RecordDecl = try_parse_record(
            "Point1D",
            "record x: System.Int32 end")
            .unwrap();
        let (_, scope) = RecordDecl::annotate(&record_decl, Rc::new(scope))
            .unwrap();

        let result = try_parse_func_def(
            r"function x(): Point1D
            begin
            end",
            scope,
        );

        match result {
            Err(SemanticError { kind: SemanticErrorKind::OutputUninitialized(name), .. }) => {
                assert_eq!("result", name);
            }

            _ => panic!("expected OutputUninitialized error, got {:?}", result)
        }
    }

    #[test]
    fn function_with_unassigned_out_is_err() {
        let scope = Scope::new_root();
        let result = try_parse_func_def(
            r"function x(a: System.Int32; out b: System.Int32)
            begin
            end",
            Rc::new(scope),
        );

        match result {
            Err(SemanticError { kind: SemanticErrorKind::OutputUninitialized(name), .. }) => {
                assert_eq!("b", name);
            }

            _ => panic!("expected OutputUninitialized error, got {:?}", result)
        }
    }

    #[test]
    fn function_with_multiple_unassigned_out_is_err() {
        let scope = Scope::new_root();
        let result = try_parse_func_def(
            r"function x(out a: System.Int32; out b: System.Int32)
            begin
            end",
            Rc::new(scope),
        );

        match result {
            Err(SemanticError { kind: SemanticErrorKind::OutputUninitialized(name), .. }) => {
                assert_eq!("a", name);
            }

            _ => panic!("expected OutputUninitialized error, got {:?}", result)
        }
    }
}