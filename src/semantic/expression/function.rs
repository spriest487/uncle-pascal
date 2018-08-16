use std::rc::Rc;

use syntax;
use operators;
use semantic::{
    Expression,
    SemanticResult,
    SemanticContext,
    SemanticError,
    Scope,
    ScopedSymbol,
};
use types::{
    Type,
    FunctionSignature,
};
use node::{
    Context,
    Identifier,
    ExpressionValue,
    FunctionArgModifier,
};
use super::{
    ops::expect_valid,
    match_indirection,
    expect_initialized,
};

pub fn annotate_call(target: &syntax::Expression,
                     args: &Vec<syntax::Expression>,
                     context: SemanticContext)
                     -> SemanticResult<(Expression, Rc<Scope>)> {
    let mut scope = context.scope.clone();

    let mut typed_args = Vec::new();
    for arg in args.iter() {
        let (arg_expr, next_arg_scope) = Expression::annotate(arg, scope)?;
        scope = next_arg_scope;
        typed_args.push(arg_expr);
    }

    /* expressions of the form x.a() should first check if a is function in the same namespace as
    type A, taking an A as the first param, and invoke it using UFCS instead if so */
    let ufcs_call = match &target.value {
        &ExpressionValue::Member { ref of, ref name } => {
            let (base_expr, scope) = Expression::annotate(of, scope.clone())?;
            let base_type = base_expr.expr_type()?;

            base_type.and_then(|base_type|
                annotate_ufcs(&base_expr, &base_type, &name, &typed_args, scope))
        }

        &ExpressionValue::Identifier(ref name) => {
            name.parent().and_then(|base_id| {
                let base_sym = scope.get_symbol(&base_id)?;

                let func_name = &name.name;
                let base_expr = Expression::identifier(base_id, SemanticContext {
                    token: target.context.token().clone(),
                    scope: scope.clone(),
                });

                annotate_ufcs(&base_expr,
                              &base_sym.decl_type(),
                              func_name,
                              &typed_args,
                              scope.clone())
            })
        }

        _ => None,
    };

    match ufcs_call {
        Some(ufcs_expr) => Ok(ufcs_expr),
        None => {
            /*
                handle typecasting - if the func name is just an identifier,
                it might be a typecast instead if there's no function by that
                name and the arg list is exactly 1 long
           */
            if let ExpressionValue::Identifier(name) = &target.value {
                if scope.get_function(name).is_none() && typed_args.len() == 1 {
                    if let Some(target_type) = scope.get_type_alias(name) {
                        // we already checked there's exactly 1 arg
                        let from_value = typed_args.into_iter()
                            .next()
                            .unwrap();

                        let context = SemanticContext {
                            token: target.context.token().clone(),
                            scope: scope.clone(),
                        };

                        let type_cast = Expression::type_cast(target_type, from_value, context);
                        return Ok((type_cast, scope));
                    }
                }
            }

            let (target, scope) = Expression::annotate(target, scope)?;

            /* ordinary function call */
            let sig = match target.expr_type()? {
                Some(Type::Function(sig)) => {
                    *sig.clone()
                }

                invalid @ _ => {
                    return Err(SemanticError::invalid_function_type(invalid, target.context));
                }
            };

            let scope = scope_after_fn_call(&sig, &typed_args, scope);
            let func_call = Expression::function_call(target, typed_args);

            Ok((func_call, scope))
        }
    }
}

fn annotate_ufcs(target: &Expression,
                 target_type: &Type,
                 func_name: &str,
                 args: &Vec<Expression>,
                 scope: Rc<Scope>) -> Option<(Expression, Rc<Scope>)> {
    /* look for matching UFCS func in NS of target type */
    let ufcs_ns = ufcs_ns_of_type(target_type, scope.as_ref())?;

    let ufcs_name = ufcs_ns.child(func_name);
    let ufcs_function = scope.get_symbol(&ufcs_name)?;

    match ufcs_function {
        ScopedSymbol::Local { name: ufcs_func_name, decl_type: Type::Function(sig), .. } => {
            let first_arg = sig.args.first()?;

            if first_arg.decl_type.remove_indirection() != target_type.remove_indirection()
                || first_arg.modifier.is_some() {
                None
            } else {
                /* match target arg expression indirection level to level of expected first arg,
                 taking the address or derefing the pointer as necessary */
                let ufcs_target_arg = match_indirection(&target, target_type, &first_arg.decl_type);

                /* the target becomes the first arg */
                let mut ufcs_args = vec![ufcs_target_arg];
                ufcs_args.extend(args.iter().cloned());

                let ufcs_target_expr = Expression::identifier(ufcs_func_name.clone(),
                                                              target.context.clone());

                let func_call = Expression::function_call(ufcs_target_expr, ufcs_args);
                let scope_after = scope_after_fn_call(&sig, args, scope.clone());

                Some((func_call, scope_after))
            }
        }

        _ => None,
    }
}

pub fn call_type(target: &Expression,
                 actual_args: &Vec<Expression>,
                 context: &SemanticContext) -> SemanticResult<Option<Type>> {
    let target_type = target.expr_type()?;
    if let Some(Type::Function(sig)) = &target_type {
        if actual_args.len() != sig.args.len() {
            Err(SemanticError::wrong_num_args(sig.as_ref().clone(),
                                              actual_args.len(),
                                              context.clone()))
        } else {
            let wrong_args = || {
                let actual_arg_types: Vec<_> = actual_args.iter()
                    .map(|arg| arg.expr_type())
                    .collect::<SemanticResult<_>>()?;

                Err(SemanticError::wrong_arg_types(sig.as_ref().clone(),
                                                   actual_arg_types,
                                                   context.clone(),
                ))
            };

            for (arg_index, arg_expr) in actual_args.iter().enumerate() {
                let sig_arg = &sig.args[arg_index];

                match sig_arg.modifier {
                    /* by-value (and const-by-value) args: actual arg type may be
                    any type assignable to the sig type, and must be initialized */
                    | Some(FunctionArgModifier::Const)
                    | None => {
                        let check_arg_matches = expect_valid(
                            operators::Assignment,
                            Some(&sig_arg.decl_type),
                            &arg_expr,
                            &arg_expr.context,
                        );

                        if check_arg_matches.is_err() {
                            return wrong_args();
                        }

                        expect_initialized(arg_expr)?
                    }

                    /* by-ref var: must be initialized, must be exact type */
                    | Some(FunctionArgModifier::Var) => {
                        expect_initialized(arg_expr)?;

                        if arg_expr.expr_type()?.as_ref() != Some(&sig_arg.decl_type) {
                            return wrong_args();
                        }
                    }

                    /* by-ref out: may be uninitialized, must be exact type */
                    | Some(FunctionArgModifier::Out) => {
                        if arg_expr.expr_type()?.as_ref() != Some(&sig_arg.decl_type) {
                            return wrong_args();
                        }
                    }
                }
            }

            Ok(sig.return_type.clone())
        }
    } else {
        Err(SemanticError::invalid_function_type(target_type.clone(), context.clone()))
    }
}

fn ufcs_ns_of_type(ty: &Type, scope: &Scope) -> Option<Identifier> {
    let type_id = match ty.remove_indirection() {
        | Type::Class(name) => scope.get_class(&name)?.0,
        | Type::Record(name) => scope.get_record(&name)?.0,
        | Type::Set(name) => scope.get_set(&name)?.0,
        | Type::Enumeration(name) => scope.get_enumeration(&name)?.0,

        | Type::Array(array_type) =>
            return ufcs_ns_of_type(array_type.element.as_ref(), scope),
        | Type::DynamicArray(array_type) =>
            return ufcs_ns_of_type(array_type.element.as_ref(), scope),

        | Type::Boolean
        | Type::Byte
        | Type::Int32
        | Type::UInt32
        | Type::Int64
        | Type::UInt64
        | Type::Float64 =>
            return Some(Identifier::from("System")),

        _ =>
            return None,
    };

    type_id.parent()
}

fn scope_after_fn_call(sig: &FunctionSignature,
                       actual_args: &[Expression],
                       mut scope: Rc<Scope>) -> Rc<Scope> {
    for (sig_arg, arg_expr) in sig.args.iter().zip(actual_args.iter()) {
        if let Some(FunctionArgModifier::Out) = &sig_arg.modifier {
            /* if the out parameter references an identifier, that identifier must now be
              initialized */
            match &arg_expr.value {
                ExpressionValue::Identifier(name) => {
                    let initialized = scope.as_ref().clone().initialize_symbol(name);
                    scope = Rc::new(initialized);
                }
                _ => {}
            }
        }
    }

    scope
}