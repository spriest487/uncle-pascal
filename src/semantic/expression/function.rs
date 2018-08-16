use std::rc::Rc;

use syntax;
use operators;
use semantic::{
    Expression,
    SemanticResult,
    SemanticContext,
    SemanticError,
    Scope,
    FunctionDecl,
    FunctionSignature,
};
use types::Type;
use node::{
    self,
    Context,
    Identifier,
    ExpressionValue,
    FunctionArgModifier,
};
use super::{
    ops::{
        expect_valid,
        is_assignable,
    },
    match_indirection,
    expect_initialized,
};

pub type FunctionCall = node::FunctionCall<SemanticContext>;

pub fn annotate_args(args: &[syntax::Expression],
                     sig: &FunctionSignature,
                     context: &SemanticContext,
                     mut scope: Rc<Scope>)
                     -> SemanticResult<(Vec<Expression>, Rc<Scope>)> {
    if args.len() != sig.args.len() {
        return Err(SemanticError::wrong_num_args(sig.clone(), args.len(), context.clone()));
    }

    let mut typed_args = Vec::new();
    for (arg, sig_arg) in args.iter().zip(sig.args.iter()) {
        let expected_type = &sig_arg.decl_type;
        let (arg_expr, next_arg_scope) = Expression::annotate(arg, Some(expected_type), scope)?;

        scope = next_arg_scope;
        typed_args.push(arg_expr);
    }

    Ok((typed_args, scope))
}

pub fn annotate_call(call: &syntax::FunctionCall,
                     context: SemanticContext)
                     -> SemanticResult<(Expression, Rc<Scope>)> {
    let scope = context.scope.clone();
    let (target, args) = match call {
        node::FunctionCall::Function { target, args } =>
            (target, args),
        node::FunctionCall::Method { .. } =>
            unreachable!("interface calls can't be parsed"),
    };

    /*
        UFCS resolution:
        expressions of the form x.a() should first check if a is function in the same namespace as
        type A, taking x as the first param, or an interface method implemented for x named a()
    */
    let ufcs_candidate = find_ufcs_candidate(target, &context);
    if let Some(ufcs_call) = ufcs_candidate.as_ref().and_then(find_ufcs_call) {
        let (ufcs_call, scope) = annotate_ufcs(ufcs_call, args)?;
        return Ok((ufcs_call, scope));
    }

    if let Some(interface_call) = ufcs_candidate.as_ref().and_then(find_interface_call) {
        return annotate_method(interface_call, args);
    }

    /*
        handle typecasting - if the func name is just an identifier,
        it might be a typecast instead if there's no function by that
        name and the arg list is exactly 1 long
   */
    if let ExpressionValue::Identifier(name) = &target.value {
        if scope.get_function(name).is_none() && args.len() == 1 {
            if let Some(target_type) = scope.get_type_alias(name) {
                // we already checked there's exactly 1 arg
                let from_arg = args.into_iter()
                    .next()
                    .unwrap();
                let (from_value, scope) = Expression::annotate(
                    from_arg,
                    Some(&target_type),
                    scope,
                )?;

                let context = SemanticContext {
                    token: target.context.token().clone(),
                    scope: scope.clone(),
                };

                let type_cast = Expression::type_cast(target_type, from_value, context);
                return Ok((type_cast, scope));
            }
        }
    }

    let (target, scope) = Expression::annotate(target, None, scope)?;
    expect_initialized(&target)?;

    /* ordinary function call */
    let sig = match target.expr_type()? {
        Some(Type::Function(sig)) => {
            *sig.clone()
        }

        invalid @ _ => {
            return Err(SemanticError::invalid_function_type(invalid, target.context));
        }
    };

    let (typed_args, scope) = annotate_args(args, &sig, &context, scope)?;

    let scope = scope_after_fn_call(&sig, &typed_args, scope);
    let func_call = Expression::function_call(target, typed_args);

    Ok((func_call, scope))
}

fn annotate_method(call_site: InterfaceCallSite,
                   args: &[syntax::Expression])
                   -> SemanticResult<(Expression, Rc<Scope>)> {
    let self_type = call_site.self_arg.expr_type()?.unwrap();

    /* make a copy of the expected sig minus the first argument to compare against,
     because the first argument is already the target of this expression */
    let rest_args_sig = FunctionSignature {
        args: call_site.method_sig.args[1..].iter().cloned().collect(),
        ..call_site.method_sig.clone()
    };

    let scope = call_site.scope_after_target;
    let context = call_site.self_arg.context.clone();

    let (mut args, scope) = annotate_args(args, &rest_args_sig, &context, scope)?;
    args.insert(0, call_site.self_arg);

    let method_call = Expression::method_call(
        call_site.interface_id,
        call_site.method_name,
        self_type,
        args,
        context,
    );

    Ok((method_call, scope))
}

struct UfcsCandidate {
    target: Expression,
    scope_after_target: Rc<Scope>,
    func_name: String,
}

struct UfcsCallSite {
    target: Expression,
    function: FunctionDecl,
    function_id: Identifier,
    scope_after_target: Rc<Scope>,
}

struct InterfaceCallSite {
    self_arg: Expression,
    interface_id: Identifier,
    method_name: String,
    method_sig: FunctionSignature,
    scope_after_target: Rc<Scope>,
}

fn find_ufcs_candidate(target: &syntax::Expression,
                       context: &SemanticContext)
                       -> Option<UfcsCandidate> {
    /* find the function name and target expression name that this expression, if it's a qualified
    identifier or member-of expression, could possibly resolve to */
    match &target.value {
        ExpressionValue::Member { of, name } => {
            let (base_expr, scope) = Expression::annotate(of, None, context.scope.clone())
                .ok()?;

            Some(UfcsCandidate {
                target: base_expr,
                func_name: name.clone(),
                scope_after_target: scope,
            })
        }

        ExpressionValue::Identifier(name) => {
            let base_id = name.parent()?;
            let func_name = &name.name;
            let base_expr = Expression::identifier(base_id, SemanticContext {
                token: target.context.token().clone(),
                scope: context.scope.clone(),
            });

            Some(UfcsCandidate {
                target: base_expr,
                func_name: func_name.to_string(),
                scope_after_target: context.scope.clone(),
            })
        }

        _ => None,
    }
}

fn find_ufcs_call(candidate: &UfcsCandidate) -> Option<UfcsCallSite> {
    let target_type = candidate.target.expr_type().ok()??;

    let ufcs_ns = ufcs_ns_of_type(&target_type, candidate.scope_after_target.as_ref())?;

    /*
        first check if there's a function with this name and a matching self-arg
        todo: match the whole args list for better overload resolution
     */
    let ufcs_name = ufcs_ns.child(&candidate.func_name);

    let (ufcs_fn_id, ufcs_fn) = candidate.scope_after_target.get_function(&ufcs_name)?;
    let first_arg = ufcs_fn.args.first()?;

    if first_arg.decl_type.remove_indirection() == target_type.remove_indirection()
        && first_arg.modifier.is_none() {
        Some(UfcsCallSite {
            scope_after_target: candidate.scope_after_target.clone(),
            target: candidate.target.clone(),
            function: ufcs_fn.clone(),
            function_id: ufcs_fn_id.clone(),
        })
    } else {
        None
    }
}

fn find_interface_call(candidate: &UfcsCandidate) -> Option<InterfaceCallSite> {
    let target_type = candidate.target.expr_type().ok()??;
    let scope = &candidate.scope_after_target;

    /* interface types are easy, just look up the interface  */
    if let Type::AnyImplementation(interface_id) = &target_type {
        let (interface_id, interface) = match scope.get_interface(interface_id) {
            Some((interface_id, interface)) => (interface_id, interface),
            None => {
                eprintln!("unknown interface type passed to get_interface_impls: {}", interface_id);
                return None;
            }
        };

        let method = interface.decl.functions.get(&candidate.func_name)?;

        return Some(InterfaceCallSite {
            interface_id: interface_id.clone(),
            scope_after_target: scope.clone(),
            method_name: candidate.func_name.clone(),
            self_arg: candidate.target.clone(),
            method_sig: method.clone(),
        });
    }

    let interface_impls = scope.get_interface_impls(&target_type);
    let interface_funcs: Vec<_> = interface_impls.into_iter()
        .filter_map(|(iface_id, method)| {
            match method.name == candidate.func_name {
                true => Some((iface_id, method)),
                false => None,
            }
        })
        .collect();

    /* todo: disambiguate by signature, raise an error here instead of silently failing */
    if interface_funcs.len() > 1 {
        return None;
    }

    if interface_funcs.len() == 0 {
        return None;
    }

    let (interface_id, method) = interface_funcs[0];

    Some(InterfaceCallSite {
        self_arg: candidate.target.clone(),
        interface_id: interface_id.clone(),
        method_name: method.name.to_string(),
        method_sig: method.signature(),
        scope_after_target: scope.clone(),
    })
}

fn annotate_ufcs(ufcs_call: UfcsCallSite,
                 args: &[syntax::Expression]) -> SemanticResult<(Expression, Rc<Scope>)> {
    let target_type = ufcs_call.target.expr_type()?
        .expect("ufcs target must have a type");

    let first_arg = &ufcs_call.function.args[0];

    /* match target arg expression indirection level to level of expected first arg,
     taking the address or derefing the pointer as necessary */
    let ufcs_target_arg = match_indirection(
        &ufcs_call.target,
        &target_type,
        &first_arg.decl_type,
    );
    let ufcs_target_expr = Expression::identifier(
        ufcs_call.function_id,
        ufcs_call.target.context.clone(),
    );

    /* for arg typechecking, use a version of the signature without the first argument -
    the target has already been annotated, and will become the first arg of the actual
    function call */
    let full_sig = ufcs_call.function.signature();
    let mut ufcs_sig = full_sig.clone();
    ufcs_sig.args.remove(0);

    let (typed_args, scope) = annotate_args(
        args,
        &ufcs_sig,
        &ufcs_call.target.context,
        ufcs_call.scope_after_target,
    )?;

    let mut ufcs_args = vec![ufcs_target_arg];
    ufcs_args.extend(typed_args.iter().cloned());

    let func_call = Expression::function_call(ufcs_target_expr, ufcs_args);
    let scope_after = scope_after_fn_call(&full_sig, &typed_args, scope);

    Ok((func_call, scope_after))
}

pub fn call_type(call: &FunctionCall,
                 context: &SemanticContext)
                 -> SemanticResult<Option<Type>> {
    let actual_args = call.args();

    let sig = match call {
        node::FunctionCall::Function { target, .. } => {
            let target_type = target.expr_type()?;

            match &target_type {
                | Some(Type::Function(sig)) => sig.as_ref().clone(),
                | _ => {
                    return Err(SemanticError::invalid_function_type(target_type.clone(), context.clone()));
                }
            }
        }

        node::FunctionCall::Method { interface_id, func_name, for_type, .. } => {
            let (interface_id, interface) = context.scope.get_interface(interface_id)
                .ok_or_else(|| {
                    SemanticError::unknown_type(interface_id.clone(), context.clone())
                })?;

            match for_type {
                /* this must be the right type of pointer, or annotate() would have failed */
                Type::AnyImplementation(_) => {
                    interface.decl.functions.get(func_name).cloned().unwrap()
                }

                _ => {
                    /* already checked this is valid */
                    let self_type_name = context.scope.full_type_name(for_type).unwrap();

                    let impl_func = interface.get_impl(&self_type_name, func_name)
                        .ok_or_else(|| {
                            let missing = interface_id.child(func_name);
                            SemanticError::unknown_symbol(missing, context.clone())
                        })?;

                    impl_func.signature()
                }
            }
        }
    };

    let wrong_args = || -> SemanticResult<Option<Type>> {
        let actual_arg_types: Vec<_> = actual_args.iter()
            .map(|arg| arg.expr_type())
            .collect::<SemanticResult<_>>()?;

        Err(SemanticError::wrong_arg_types(
            sig.clone(),
            actual_arg_types,
            context.clone(),
        ))
    };

    /*
        all by-ref arg types: type in signature and actual type must be a 1:1 match,
        and the result must be assignable from the call context
    */
    let valid_byref_arg = |arg_expr: &Expression, sig_ty: &Type| {
        if !is_assignable(arg_expr) {
            return Err(SemanticError::value_not_assignable(arg_expr.clone()));
        }

        let ty = arg_expr.expr_type()?;
        if ty.as_ref() != Some(sig_ty) {
            return wrong_args();
        }

        Ok(ty)
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

            /* by-ref var: must be initialized */
            | Some(FunctionArgModifier::Var) => {
                expect_initialized(arg_expr)?;
                valid_byref_arg(arg_expr, &sig_arg.decl_type)?;
            }

            /* by-ref out: may be uninitialized */
            | Some(FunctionArgModifier::Out) => {
                valid_byref_arg(arg_expr, &sig_arg.decl_type)?;
            }
        }
    }

    Ok(sig.return_type.clone())
}

fn ufcs_ns_of_type(ty: &Type, scope: &Scope) -> Option<Identifier> {
    let type_id = match ty.remove_indirection() {
        | Type::Array(array_type) =>
            return ufcs_ns_of_type(array_type.element.as_ref(), scope),
        | Type::DynamicArray(array_type) =>
            return ufcs_ns_of_type(array_type.element.as_ref(), scope),

        ty @ _ => scope.full_type_name(ty),
    }?;

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