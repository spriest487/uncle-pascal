use std::{
    rc::Rc,
    iter,
};
use syntax;
use semantic::*;
use node::{
    self,
    Identifier,
    ExpressionValue,
    ConstantExpression,
    FunctionArgModifier,
};
use operators;
use types::{
    self,
    Type,
    FunctionSignature,
};
use consts::IntConstant;

pub type Expression = node::Expression<SemanticContext>;

fn expect_valid_operation(operator: operators::Operator,
                          target: Option<&Type>,
                          actual_expr: &Expression,
                          context: &SemanticContext) -> SemanticResult<()> {
    /* special case for assigning 0 to any numeric type, or assigning integers in
     the range 0...255 to bytes */
    match (operator, target, &actual_expr.value) {
        (
            operators::Assignment,
            Some(lhs_type),
            ExpressionValue::Constant(ConstantExpression::Integer(int))
        ) =>
            if let Some(u8_val) = int.as_u8() {
                let assigning_u8_to_byte = *lhs_type == Type::Byte;
                let assigning_zero_to_num = u8_val == 0 && lhs_type.is_numeric();

                if assigning_u8_to_byte || assigning_zero_to_num {
                    return Ok(());
                }
            },

        _ => {}
    };

    let actual = actual_expr.expr_type()?;

    if operator == operators::Equals || operator == operators::NotEquals {
        return expect_comparable(target, actual.as_ref(), context);
    }

    let string_class = Type::Class(Identifier::from("System.String"));

    match (target, actual) {
        (None, _) =>
            Err(SemanticError::type_not_assignable(None, context.clone())),
        (_, None) =>
            Err(SemanticError::unexpected_type(target.cloned(), None, context.clone())),

        (Some(a), Some(b)) => {
            let valid = match operator {
                operators::Assignment => a.assignable_from(&b),

                operators::Plus => {
                    /* special case for string concat sugar */
                    if *a == string_class && b == string_class {
                        true
                    } else {
                        a.can_offset_by(&b)
                    }
                }
                operators::Minus =>
                    a.can_offset_by(&b),

                operators::Multiply |
                operators::Divide =>
                    a.is_numeric() && b.promotes_to(a),

                operators::Gt |
                operators::Gte |
                operators::Lt |
                operators::Lte =>
                    a.has_ord_comparisons(&b),

                operators::And |
                operators::Or =>
                    *a == Type::Boolean && b == Type::Boolean,

                _ => false,
            };

            if valid {
                Ok(())
            } else {
                let err_types = vec![Some(a.clone()), Some(b.clone())];
                Err(SemanticError::invalid_operator(operator, err_types, context.clone()))
            }
        }
    }
}

fn expect_comparable(target: Option<&Type>,
                     actual: Option<&Type>,
                     context: &SemanticContext) -> SemanticResult<()> {
    match (target, actual) {
        (a @ None, b @ _) | (a @ _, b @ None) =>
            Err(SemanticError::types_not_comparable(a.cloned(),
                                                    b.cloned(),
                                                    context.clone())),

        (a @ Some(_), b @ Some(_)) =>
            if a.unwrap().comparable_to(b.unwrap()) {
                Ok(())
            } else {
                Err(SemanticError::types_not_comparable(a.cloned(),
                                                        b.cloned(),
                                                        context.clone()))
            }
    }
}

fn annotate_identifier(name: &Identifier,
                       scope: Rc<Scope>,
                       context: &syntax::ParsedContext)
                       -> SemanticResult<Expression> {
    let identifier_context = SemanticContext {
        token: context.token().clone(),
        scope: scope.clone(),
    };

    scope.get_symbol(name)
        .and_then(|symbol| match symbol {
            /* transform identifiers that reference record members into
            record member expressions */
            ScopedSymbol::RecordMember { record_id, name, .. } => {
                let base = Expression::identifier(record_id, identifier_context.clone());

                Some(Expression::member(base, &name))
            }

            ScopedSymbol::Local { name, .. } => {
                Some(Expression::identifier(name.clone(), identifier_context.clone()))
            }
        })
        .or_else(|| {
            let (_, const_val) = scope.get_const(name)?;

            Some(Expression::const_value(const_val.clone(), identifier_context.clone()))
        })
        .ok_or_else(|| {
            SemanticError::unknown_symbol(name.clone(), identifier_context)
        })
}

fn annotate_while_loop(condition: &syntax::Expression,
                       body: &syntax::Expression,
                       scope: Rc<Scope>,
                       context: &syntax::ParsedContext)
                       -> SemanticResult<(Expression, Rc<Scope>)> {
    let (cond_expr, scope) = Expression::annotate(condition, scope.clone())?;

    /* anything initialized or declared in the scope of the body is strictly
    confined to the body, because it may never execute */
    let (body_expr, _) = Expression::annotate(body, scope.clone())?;

    let context = SemanticContext {
        token: context.token().clone(),
        scope: scope.clone(),
    };

    let while_loop = Expression::while_loop(cond_expr, body_expr, context);

    Ok((while_loop, scope))
}

fn while_type(condition: &Expression, body: &Expression) -> SemanticResult<()> {
    let cond_type = condition.expr_type()?;
    body.expr_type()?;

    expect_comparable(Some(&Type::Boolean), cond_type.as_ref(), &condition.context)?;
    Ok(())
}


fn annotate_if(condition: &syntax::Expression,
               then_branch: &syntax::Expression,
               else_branch: Option<&syntax::Expression>,
               scope: Rc<Scope>,
               context: &syntax::ParsedContext) -> SemanticResult<(Expression, Rc<Scope>)>
{
    let (cond_expr, scope_after_condition) = Expression::annotate(condition, scope.clone())?;
    let (then_expr, scope_after_then) = Expression::annotate(then_branch, scope_after_condition.clone())?;
    let (else_expr, scope_after_else) = match else_branch {
        Some(expr) => {
            let (else_expr, scope_after_else) = Expression::annotate(expr, scope_after_condition.clone())?;
            (Some(else_expr), scope_after_else)
        }
        None => (None, scope_after_condition.clone())
    };

    let if_then_else = Expression::if_then_else(cond_expr, then_expr, else_expr, SemanticContext {
        token: context.token().clone(),
        scope: scope.clone(),
    });

    /*
        the scope for the condition, and each of the branches, doesn't live longer than the
        if-expression. however, variables initialized in the condition expr, or variables initialized
        in both of the branch expressions (if there is an else-branch), should still be considered
        initialized after this if-expression.

        ```
        { function GetThing(out thing: Thing): Boolean }
        if GetThing(thing) then
            x := 1
        else
            x := 2
        ```
        `thing` and and `x` should be initialized
     */
    let mut out_scope = scope.clone();
    for name in scope_after_condition.initialized_since(scope.as_ref()) {
        out_scope = Rc::new(scope.as_ref().clone()
            .initialize_symbol(name));
    }

    if else_branch.is_some() {
        let then_initialized = scope_after_then.initialized_since(scope_after_condition.as_ref());

        for name in scope_after_else.initialized_since(scope_after_condition.as_ref()) {
            if then_initialized.contains(&name) {
                out_scope = Rc::new(scope.as_ref().clone()
                    .initialize_symbol(&name));
            }
        }
    }

    Ok((if_then_else, out_scope))
}

fn if_type(condition: &Expression,
           then_branch: &Expression,
           else_branch: Option<&Expression>,
           context: &SemanticContext) -> SemanticResult<()> {
    let condition_type = condition.expr_type()?;

    expect_comparable(Some(&Type::Boolean),
                      condition_type.as_ref(),
                      context)?;

    let _then_type = then_branch.expr_type()?;

    if let Some(else_expr) = else_branch {
        else_expr.expr_type()?;
    }

    Ok(())
}

fn annotate_for_loop(from: &syntax::Expression,
                     to: &syntax::Expression,
                     body: &syntax::Expression,
                     scope: Rc<Scope>,
                     context: &syntax::ParsedContext)
                     -> SemanticResult<(Expression, Rc<Scope>)> {
    let (from_expr, to_scope) = Expression::annotate(from, scope.clone())?;
    let (to_expr, body_scope) = Expression::annotate(to, to_scope.clone())?;

    /*  we don't use the resulting scope of the loop body, because the loop
        may run 0 times
        todo: initializations in the from and to should be in the final scope?
    */
    let (do_expr, _) = Expression::annotate(body, body_scope.clone())?;

    let for_loop = Expression::for_loop(from_expr, to_expr, do_expr, SemanticContext {
        token: context.token().clone(),
        scope: scope.clone(),
    });

    let mut scope_out = scope.clone();
    for name in to_scope.initialized_since(scope.as_ref()) {
        scope_out = Rc::new(scope_out.as_ref().clone().initialize_symbol(name));
    }
    for name in body_scope.initialized_since(scope.as_ref()) {
        scope_out = Rc::new(scope_out.as_ref().clone().initialize_symbol(name));
    }

    Ok((for_loop, scope_out))
}

fn for_loop_type(from: &Expression,
                 to: &Expression,
                 body: &Expression,
                 context: &SemanticContext) -> SemanticResult<()> {
    let from_type = from.expr_type()?;
    let to_type = to.expr_type()?;

    expect_comparable(Some(&Type::Int32), to_type.as_ref(), context)?;

    let _body_type = body.expr_type()?;

    match &from.value {
        &ExpressionValue::BinaryOperator { ref op, ref lhs, .. }
        if *op == operators::Operator::Assignment => {
            let lhs_type = lhs.expr_type()?;

            expect_comparable(Some(&Type::Int32), lhs_type.as_ref(),
                              context)?;
            Ok(())
        }

        &ExpressionValue::LetBinding { ref value, .. } => {
            let value_type = value.expr_type()?;

            expect_comparable(Some(&Type::Int32), value_type.as_ref(),
                              context)?;
            Ok(())
        }

        //TODO better error
        _ => Err(SemanticError::unexpected_type(
            Some(types::Type::Int32),
            from_type,
            context.clone()))
    }
}

fn match_indirection(expr: &Expression,
                     expr_type: &Type,
                     target_type: &Type) -> Expression {
    let target_level = target_type.indirection_level();

    let mut current_level = expr_type.indirection_level();
    let mut result = expr.clone();

    while current_level < target_level {
        result = Expression::prefix_op(operators::AddressOf, result, expr.context.clone());
        current_level += 1;
    }

    while current_level > target_level {
        result = Expression::prefix_op(operators::Deref, result, expr.context.clone());
        current_level -= 1;
    }

    result
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

fn annotate_function_call(target: &syntax::Expression,
                          args: &Vec<syntax::Expression>,
                          mut scope: Rc<Scope>,
                          _context: &syntax::ParsedContext)
                          -> SemanticResult<(Expression, Rc<Scope>)> {
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

fn annotate_type_cast(target_type: &node::TypeName,
                      from_value: &syntax::Expression,
                      scope: Rc<Scope>,
                      context: &syntax::ParsedContext)
                      -> SemanticResult<(Expression, Rc<Scope>)> {
    let target_type = target_type.resolve(scope.clone())?;
    let (from_value, scope) = Expression::annotate(from_value, scope.clone())?;

    let context = SemanticContext {
        token: context.token().clone(),
        scope: scope.clone(),
    };

    let type_cast = Expression::type_cast(target_type, from_value, context);
    Ok((type_cast, scope))
}

fn let_binding_type(value: &Expression, context: &SemanticContext) -> SemanticResult<Option<Type>> {
    expect_initialized(value)?;

    let value_type = value.expr_type()?
        .ok_or_else(|| SemanticError::type_not_assignable(None, context.clone()))?;

    if !value_type.valid_lhs_type() {
        return Err(SemanticError::type_not_assignable(None, context.clone()));
    }

    Ok(None)
}

fn function_call_type(target: &Expression,
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
                        if let Err(_) = expect_valid_operation(operators::Assignment,
                                                               Some(&sig_arg.decl_type),
                                                               &arg_expr,
                                                               &arg_expr.context) {
                            return wrong_args();
                        }

                        expect_initialized(arg_expr)?;
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

fn type_cast_type(target_type: &Type,
                  from_value: &Expression,
                  context: &SemanticContext) -> SemanticResult<Option<Type>> {
    let from_expr_type = from_value.expr_type()?
        .ok_or_else(|| {
            // expr has no type, can't cast it
            SemanticError::invalid_typecast(target_type.clone(), None, context.clone())
        })?;

    match (from_expr_type, target_type) {
        (Type::UInt32, Type::Int32) => Ok(Some(Type::Int32)),
        (Type::Int32, Type::UInt32) => Ok(Some(Type::UInt32)),

        (Type::UInt64, Type::Int64) => Ok(Some(Type::Int64)),
        (Type::Int64, Type::UInt64) => Ok(Some(Type::UInt64)),

        (ref from, to) if from.promotes_to(to) => Ok(Some(to.clone())),

        // unsupported type of cast
        (from, to) => {
            Err(SemanticError::invalid_typecast(to.clone(), Some(from), context.clone()))
        }
    }
}

fn is_assignable(expr: &Expression) -> bool {
    match &expr.value {
        ExpressionValue::Identifier(name) => {
            /* we have to differentiate actual named functions
            (always immutable) from function-valued bindings */
            if expr.scope().get_function(&name).is_some() {
                return false;
            }

            match expr.scope().get_symbol(name) {
                Some(binding) => binding.mutable(expr.scope().local_namespace()),
                None => false,
            }
        }
        ExpressionValue::PrefixOperator { op, .. } => match op {
            operators::Deref => true,
            _ => false,
        },

        ExpressionValue::ArrayElement { of, .. } |
        ExpressionValue::Member { of, .. } => {
            is_assignable(of)
        }

        ExpressionValue::TypeCast { .. } |
        ExpressionValue::Raise(_) |
        ExpressionValue::With { .. } |
        ExpressionValue::SetConstructor(_) |
        ExpressionValue::FunctionCall { .. } |
        ExpressionValue::Constant(_) |
        ExpressionValue::BinaryOperator { .. } |
        ExpressionValue::Block(_) |
        ExpressionValue::ForLoop { .. } |
        ExpressionValue::If { .. } |
        ExpressionValue::While { .. } |
        ExpressionValue::LetBinding { .. } =>
            false
    }
}

fn binary_op_type(lhs: &Expression,
                  op: operators::Operator,
                  rhs: &Expression,
                  context: &SemanticContext) -> SemanticResult<Option<Type>> {
    let lhs_type = lhs.expr_type()?;

    match op {
        operators::In |
        operators::And |
        operators::Or |
        operators::NotEquals |
        operators::Gt |
        operators::Gte |
        operators::Lt |
        operators::Lte |
        operators::Equals => {
            expect_valid_operation(op, lhs_type.as_ref(), &rhs, context)?;
            Ok(Some(Type::Boolean))
        }

        operators::RangeInclusive |
        operators::Multiply |
        operators::Divide |
        operators::Plus |
        operators::Minus => {
            expect_valid_operation(op, lhs_type.as_ref(), &rhs, context)?;
            Ok(lhs_type)
        }

        operators::Assignment => {
            if !is_assignable(lhs) {
                Err(SemanticError::value_not_assignable(lhs.clone()))
            } else {
                expect_valid_operation(op, lhs_type.as_ref(), &rhs, context)?;
                Ok(None)
            }
        }

        operators::Not |
        operators::AddressOf |
        operators::Deref => {
            let operand_types = vec![lhs_type, rhs.expr_type()?];
            Err(SemanticError::invalid_operator(op, operand_types, context.clone()))
        }
    }
}

fn prefix_op_type(op: operators::Operator,
                  rhs: &Expression,
                  context: &SemanticContext)
                  -> SemanticResult<Option<Type>> {
    let rhs_type = rhs.expr_type()?;

    let invalid_op_err = ||
        Err(SemanticError::invalid_operator(op.clone(),
                                            vec![rhs_type.clone()],
                                            context.clone()));
    match op {
        operators::Deref => match &rhs_type {
            &Some(Type::Pointer(ref pointed_to)) =>
                Ok(Some(pointed_to.as_ref().clone())),
            _ =>
                invalid_op_err(),
        },
        operators::AddressOf => match &rhs_type {
            &Some(ref t) if t.valid_lhs_type() =>
                Ok(Some(t.clone().pointer())),
            _ => invalid_op_err()
        }

        operators::Plus |
        operators::Minus => match &rhs_type {
            &Some(Type::Int64) |
            &Some(Type::Byte) =>
                Ok(rhs_type.clone()),
            _ =>
                invalid_op_err(),
        }

        operators::Not => match &rhs_type {
            Some(Type::Boolean) => Ok(Some(Type::Boolean)),
            _ => invalid_op_err(),
        }

        operators::In |
        operators::RangeInclusive |
        operators::Multiply |
        operators::Divide |
        operators::And |
        operators::Or |
        operators::Assignment |
        operators::Equals |
        operators::Gt |
        operators::Gte |
        operators::Lt |
        operators::Lte |
        operators::NotEquals =>
            invalid_op_err(),
    }
}

fn member_type(of: &Expression, name: &str) -> SemanticResult<Option<Type>> {
    let base_type = of.expr_type()?
        .map(|dt| dt.remove_indirection().clone());

    /* treat records and classes the same for this purpose, except that
     class members are always private i.e. inaccessible outside the unit that
     the class is declared in */
    let (base_decl, private_members) = match &base_type {
        Some(Type::Record(name)) => (of.scope().get_record(name), false),
        Some(Type::Class(name)) => (of.scope().get_class(name), true),
        _ => (None, false),
    };

    match base_decl {
        Some((record_id, record)) => {
            if private_members {
                let from_ns = of.scope().local_namespace();
                if from_ns != record_id.parent().as_ref() {
                    return Err(SemanticError::private_member_access_forbidden(
                        record_id.clone(),
                        of.scope().local_namespace().cloned(),
                        name,
                        of.context.clone(),
                    ));
                }
            }

            match record.get_member(name) {
                Some(member) =>
                    Ok(Some(member.decl_type.clone())),
                None => {
                    let name_id = Identifier::from(name);
                    Err(SemanticError::unknown_symbol(name_id, of.context.clone()))
                }
            }
        }

        _ => {
            Err(SemanticError::member_of_non_record(base_type.clone(),
                                                    name.to_string(),
                                                    of.context.clone()))
        }
    }
}

fn expect_initialized(expr: &Expression) -> SemanticResult<()> {
    node::try_visit_expressions(expr, &mut |each_expr| {
        match &each_expr.value {
            ExpressionValue::Identifier(name) => {
                match expr.scope().get_symbol(name) {
                    | Some(ScopedSymbol::RecordMember { binding_kind, .. })
                    | Some(ScopedSymbol::Local { binding_kind, .. }) => {
                        match binding_kind {
                            BindingKind::Uninitialized => {
                                let context = expr.context.clone();
                                Err(SemanticError::uninitialized_symbol(name.clone(), context))
                            }

                            _ =>
                                Ok(())
                        }
                    }

                    _ => Err(SemanticError::unknown_symbol(name.clone(), expr.context.clone()))
                }
            }

            _ => Ok(())
        }
    })
}

impl Expression {
    pub fn annotate(expr: &syntax::Expression,
                    scope: Rc<Scope>)
                    -> SemanticResult<(Self, Rc<Scope>)> {
        let expr_context = SemanticContext {
            token: expr.context.token().clone(),
            scope: scope.clone(),
        };

        match &expr.value {
            ExpressionValue::Identifier(name) => {
                let identifier = annotate_identifier(name, scope.clone(), &expr.context)?;
                Ok((identifier, scope))
            }

            ExpressionValue::Block(block) => {
                let (block, scope) = Block::annotate(block, scope)?;
                Ok((Expression::block(block), scope))
            }

            ExpressionValue::LetBinding { name, value } => {
                let (typed_value, mut scope) = Expression::annotate(value, scope)?;
                let bound_type = typed_value.expr_type()?
                    .ok_or_else(|| {
                        SemanticError::type_not_assignable(None, expr_context.clone())
                    })?;

                scope = Rc::new(scope.as_ref().clone().with_binding(
                    name,
                    bound_type,
                    BindingKind::Immutable,
                ));

                Ok((Expression::let_binding(name, typed_value, expr_context), scope))
            }

            ExpressionValue::Constant(ConstantExpression::String(s)) => {
                Ok((Expression::literal_string(s, expr_context), scope))
            }

            ExpressionValue::Constant(ConstantExpression::Integer(i)) => {
                Ok((Expression::literal_int(*i, expr_context), scope))
            }

            ExpressionValue::Constant(ConstantExpression::Nil) => {
                Ok((Expression::literal_nil(expr_context), scope))
            }

            ExpressionValue::Constant(ConstantExpression::Boolean(b)) => {
                Ok((Expression::literal_bool(*b, expr_context), scope))
            }

            ExpressionValue::Constant(ConstantExpression::Float(f)) => {
                Ok((Expression::literal_float(*f, expr_context), scope))
            }

            ExpressionValue::Constant(ConstantExpression::Enum(e)) => {
                Ok((Expression::literal_enumeration(e.clone(), expr_context), scope))
            }

            ExpressionValue::Constant(ConstantExpression::Set(set_const)) => {
                Ok((Expression::literal_set(set_const.clone(), expr_context), scope))
            }

            ExpressionValue::If { condition, then_branch, else_branch } =>
                annotate_if(condition.as_ref(),
                            then_branch.as_ref(),
                            else_branch.as_ref().map(|else_expr| else_expr.as_ref()),
                            scope,
                            &expr.context),

            ExpressionValue::While { condition, body } =>
                annotate_while_loop(condition, body, scope, &expr.context),

            ExpressionValue::ForLoop { from, to, body } =>
                annotate_for_loop(from, to, body, scope, &expr.context),

            ExpressionValue::BinaryOperator { lhs, op, rhs } => {
                // rhs is evaluated first
                let (rhs, lhs_scope) = Expression::annotate(rhs, scope)?;
                let (lhs, scope_after) = Expression::annotate(lhs, lhs_scope)?;

                let scope_out = match (op, &lhs.value) {
                    (operators::Assignment, ExpressionValue::Identifier(name)) =>
                        Rc::new(scope_after.as_ref().clone().initialize_symbol(name)),

                    _ =>
                        scope_after,
                };

                Ok((Expression::binary_op(lhs, op.clone(), rhs, expr_context), scope_out))
            }

            ExpressionValue::PrefixOperator { op, rhs } => {
                let (rhs, scope) = Expression::annotate(rhs, scope)?;
                let op_expr = Expression::prefix_op(op.clone(), rhs, expr_context);
                Ok((op_expr, scope))
            }

            ExpressionValue::FunctionCall { target, args } =>
                annotate_function_call(target, args, scope, &expr.context),

            ExpressionValue::TypeCast { target_type, from_value } =>
                annotate_type_cast(target_type, from_value, scope, &expr.context),

            ExpressionValue::Member { of, name } => {
                let (typed_of, scope) = Expression::annotate(of, scope)?;
                Ok((Expression::member(typed_of, name), scope))
            }

            ExpressionValue::ArrayElement { of, index_expr } => {
                // index expr is evaluated first
                let (index_expr, scope) = Expression::annotate(index_expr.as_ref(), scope)?;
                let (of, scope) = Expression::annotate(of.as_ref(), scope)?;

                Ok((Expression::array_element(of, index_expr), scope))
            }

            ExpressionValue::SetConstructor(_members) => {
                unimplemented!("set constructor semantic analysis")
            }

            ExpressionValue::With { value, body } => {
                let parsed_value = value.as_ref();
                let (value, scope) = Expression::annotate(value.as_ref(), scope)?;
                let value_type: Option<Type> = value.expr_type()?;

                /* find the class or record decl of the type referred to by `value` */
                let (_record_id, record) = value_type.as_ref()
                    .and_then(|ty| {
                        let class_id = ty.unwrap_class()?;
                        scope.get_class(class_id)
                    })
                    .or_else(|| {
                        let record_id = value_type.as_ref()?.unwrap_record()?;
                        scope.get_record(record_id)
                    })
                    .ok_or_else(|| {
                        SemanticError::invalid_with_type(value_type, expr_context.clone())
                    })?;

                /* to keep the implementation simple, with-bindings get removed at this point -
                 we transform the "value" expression in the input ast into a block expression
                 with let-bindings for each member of the record */

                // turn all members of the record into let-bindings
                let bindings: Vec<_> = record.members.iter()
                    .map(|record_member| {
                        let member_of = parsed_value.clone();
                        let member = syntax::Expression::member(member_of, &record_member.name);
                        let context = expr.context.clone();

                        syntax::Expression::let_binding(&record_member.name, member, context)
                    })
                    .collect();

                let body_block = syntax::Expression::block(syntax::Block {
                    context: body.context.clone(),
                    statements: bindings.into_iter()
                        .chain(iter::once(body.as_ref().clone()))
                        .collect(),
                });

                // don't need to change the scope - block + let bindings will take care of it
                Expression::annotate(&body_block, scope.clone())
            }

            ExpressionValue::Raise(error) => {
                // the raise expr is isolated
                let (error, _) = Expression::annotate(error.as_ref(), scope.clone())?;

                Ok((Expression::raise(error, expr_context), scope))
            }
        }
    }

    pub fn expr_type(&self) -> Result<Option<Type>, SemanticError> {
        match &self.value {
            ExpressionValue::BinaryOperator { lhs, op, rhs } =>
                binary_op_type(lhs, *op, rhs, &self.context),

            ExpressionValue::PrefixOperator { op, rhs } =>
                prefix_op_type(*op, rhs, &self.context),

            ExpressionValue::Constant(const_val) =>
                Ok(Some(const_val.value_type())),

            ExpressionValue::LetBinding { value, .. } =>
                let_binding_type(value, &self.context),

            ExpressionValue::FunctionCall { target, args } =>
                function_call_type(target, args, &self.context),

            ExpressionValue::TypeCast { target_type, from_value } =>
                type_cast_type(target_type, from_value, &self.context),

            ExpressionValue::Identifier(id) => {
                self.scope().get_symbol(id)
                    .map(|sym| Some(sym.decl_type().clone()))
                    .ok_or_else(|| {
                        SemanticError::unknown_symbol(id.clone(), self.context.clone())
                    })
            }

            ExpressionValue::Block(block) => {
                for statement in block.statements.iter() {
                    statement.expr_type()?;
                }

                Ok(None)
            }

            ExpressionValue::With { value, body } => {
                let val_type = value.expr_type()?;
                val_type.as_ref()
                    .and_then(|ty| {
                        ty.unwrap_class()
                            .or_else(|| ty.unwrap_record())
                    })
                    .ok_or_else(|| {
                        SemanticError::invalid_with_type(val_type.clone(), value.context.clone())
                    })?;

                body.expr_type()?;
                Ok(None)
            }

            ExpressionValue::If { condition, then_branch, else_branch } =>
                if_type(condition.as_ref(),
                        then_branch.as_ref(),
                        else_branch.as_ref().map(|else_expr| else_expr.as_ref()),
                        &self.context)
                    .map(|_| None),

            ExpressionValue::ForLoop { from, to, body } =>
                for_loop_type(from.as_ref(), to.as_ref(), body.as_ref(), &self.context)
                    .map(|_| None),

            ExpressionValue::While { condition, body } =>
                while_type(condition, body)
                    .map(|_| None),

            ExpressionValue::Member { of, name } => {
                member_type(of, name)
            }

            ExpressionValue::ArrayElement { of, index_expr } => {
                let check_index_type_is_isize = expect_valid_operation(
                    operators::Assignment,
                    Some(&Type::NativeInt),
                    index_expr,
                    &self.context);

                if check_index_type_is_isize.is_err() {
                    return Err(SemanticError::invalid_array_index(index_expr.expr_type()?,
                                                                  self.context.clone()));
                }

                match of.expr_type()? {
                    Some(Type::Pointer(ptr_to)) => {
                        Ok(Some(*ptr_to))
                    }

                    Some(Type::DynamicArray(dyn_array_type)) => {
                        Ok(Some(*dyn_array_type.element))
                    }

                    Some(Type::Array(array_type)) => {
                        match array_type.next_rank() {
                            Some(next_array) => Ok(Some(Type::Array(next_array))),
                            None => Ok(Some(*array_type.element))
                        }
                    }

                    //pointers can also be dereferenced via indexing
                    invalid @ _ => return Err(SemanticError::invalid_array_type(
                        invalid,
                        self.context.clone(),
                    ))
                }
            }

            ExpressionValue::SetConstructor(_members) => {
                unimplemented!("set constructor typechecking")
            }

            ExpressionValue::Raise(error) => {
                error.expr_type()?;
                Ok(None)
            }
        }
    }

    pub fn type_check(&self) -> Result<(), SemanticError> {
        self.expr_type()?;
        Ok(())
    }

    pub fn class_type(&self) -> SemanticResult<Option<&RecordDecl>> {
        match self.expr_type()? {
            Some(Type::Class(name)) => {
                let (_class_id, class_decl) = self.context.scope.get_class(&name)
                    .expect("record must exist in scope of expression it's used in");

                Ok(Some(class_decl))
            }
            _ => Ok(None),
        }
    }

    pub fn function_type(&self) -> SemanticResult<Option<FunctionSignature>> {
        match self.expr_type()? {
            Some(Type::Function(sig)) => Ok(Some(sig.as_ref().clone())),
            _ => Ok(None),
        }
    }

    pub fn scope(&self) -> &Scope {
        self.context.scope.as_ref()
    }

    pub fn into_const_expr(self) -> SemanticResult<Self> {
        let const_value = self.to_const_value()?;

        Ok(Expression {
            context: self.context,
            value: ExpressionValue::Constant(const_value),
        })
    }

    pub fn to_const_value(&self) -> SemanticResult<ConstantExpression> {
        match &self.value {
            ExpressionValue::Constant(val) => {
                Ok(val.clone())
            }

            ExpressionValue::PrefixOperator { op, rhs } => {
                let rhs_val = rhs.to_const_value()?;
                match (*op, rhs_val) {
                    (operators::Minus, ConstantExpression::Integer(rhs_int)) => {
                        Ok(ConstantExpression::Integer(IntConstant::from(0) - rhs_int))
                    }

                    (_, rhs_val) => {
                        let operands = vec![Some(rhs_val.value_type())];
                        Err(SemanticError::invalid_operator(*op, operands, self.context.clone()))
                    }
                }
            }

            ExpressionValue::BinaryOperator { lhs, op, rhs } => {
                let lhs_val: ConstantExpression = lhs.to_const_value()?;
                let rhs_val: ConstantExpression = rhs.to_const_value()?;

                match (lhs_val, *op, rhs_val) {
                    /* const string concatenation */
                    (
                        ConstantExpression::String(lhs_str),
                        operators::Plus,
                        ConstantExpression::String(rhs_str)
                    ) => {
                        Ok(ConstantExpression::String(lhs_str + &rhs_str))
                    }

                    /* const int + */
                    (
                        ConstantExpression::Integer(lhs_int),
                        operators::Plus,
                        ConstantExpression::Integer(rhs_int)
                    ) => {
                        Ok(ConstantExpression::Integer(lhs_int + rhs_int))
                    }

                    /* const int - */
                    (
                        ConstantExpression::Integer(lhs_int),
                        operators::Minus,
                        ConstantExpression::Integer(rhs_int)
                    ) => {
                        Ok(ConstantExpression::Integer(lhs_int - rhs_int))
                    }

                    (lhs_val, _, rhs_val) => {
                        let operands = vec![
                            Some(lhs_val.value_type()),
                            Some(rhs_val.value_type()),
                        ];
                        Err(SemanticError::invalid_operator(*op, operands, self.context.clone()))
                    }
                }
            }

            ExpressionValue::Identifier(name) => {
                self.scope().get_const(name)
                    .map(|(_const_id, const_val)| const_val.clone())
                    .ok_or_else(|| {
                        SemanticError::invalid_const_value(self.clone())
                    })
            }

            _ => {
                Err(SemanticError::invalid_const_value(self.clone()))
            }
        }
    }
}

#[cfg(test)]
pub(crate) mod test {
    use std::{
        rc::Rc,
    };

    use tokenizer::*;
    use semantic::*;
    use syntax;
    use node::ExpressionValue;
    use operators;
    use types::Type;
    use opts::CompileOptions;

    pub fn parse_expr(src: &str, scope: Rc<Scope>) -> (Expression, Rc<Scope>) {
        let tokens = tokenize("test", src, &CompileOptions::default())
            .expect(&format!("test expr `{}` must not contain illegal tokens", src));

        let mut stream = syntax::TokenStream::from(tokens);

        let parsed: syntax::Expression = stream.parse()
            .expect(&format!("test expr `{}` must parse correctly", src));

        stream.finish().expect("expr must not contain trailing tokens");

        Expression::annotate(&parsed, scope)
            .expect(&format!("test expr `{}` must have valid types", src))
    }

    fn parse_func_decl(src: &str, scope: Rc<Scope>) -> FunctionDecl {
        let tokens = tokenize("test", src, &CompileOptions::default())
            .expect(&format!("test decl `{}` must not contain illegal tokens", src));

        let mut stream = syntax::TokenStream::from(tokens);
        let parsed: syntax::FunctionDecl = stream.parse()
            .expect(&format!("test decl `{}` must parse correctly", src));

        stream.finish().expect("expr must not contain trailing tokens");

        FunctionDecl::annotate(&parsed, scope)
            .expect(&format!("test expr `{}` must have valid types", src))
    }

    #[test]
    fn assignment_to_wrong_type_is_err() {
        let scope = Scope::default()
            .with_binding("x", Type::RawPointer, BindingKind::Uninitialized);

        let (expr, _) = parse_expr("x := 1", Rc::new(scope));

        match expr.type_check() {
            Err(SemanticError {
                    kind: SemanticErrorKind::InvalidOperator { op, args },
                    ..
                }) => {
                assert_eq!(operators::Assignment, op);
                assert_eq!(Some(Type::RawPointer), args[0]);
                assert_eq!(Some(Type::Int32), args[1]);
            }
            _ => panic!("expected invalid types in assignment")
        }
    }

    #[test]
    fn func_call_on_obj_uses_ufcs_from_target_ns() {
        let scope = Scope::default()
            .with_local_namespace("System");

        let scope = scope.clone()
            .with_function(
                parse_func_decl("function TestAdd(x: Int64): Int64",
                                Rc::new(scope))
            )
            .with_binding("a", Type::Int64, BindingKind::Mutable);

        let (expr, _) = parse_expr(r"a.TestAdd(1)", Rc::new(scope.clone()));

        match expr.value {
            ExpressionValue::FunctionCall { target, args } => {
                assert!(target.is_identifier(&Identifier::from("System.TestAdd")),
                        "expected identifier, found {:?}", target.to_source());

                assert_eq!(2, args.len());

                assert!(args[0].is_identifier(&Identifier::from("a")));

                assert!(args[1].is_literal_integer(1));
            }

            _ => panic!("result should be a function call")
        }
    }

    #[test]
    fn type_of_pointer_deref_is_pointer_minus_indirection() {
        let scope = Scope::default()
            .with_binding("x", Type::Byte.pointer(), BindingKind::Immutable);

        let (expr, _) = parse_expr("^x", Rc::new(scope));
        assert_eq!(Type::Byte, expr.expr_type().unwrap().unwrap());
    }

    #[test]
    fn type_of_pointer_plus_offset_is_pointer() {
        let scope = Scope::default()
            .with_binding("x", Type::Byte.pointer(), BindingKind::Immutable);

        let (expr, _) = parse_expr("x + 1", Rc::new(scope));
        assert_eq!(Type::Byte.pointer(), expr.expr_type().unwrap().unwrap());
    }

    #[test]
    fn out_param_initializes_value() {
        let mut scope = Scope::default()
            .with_binding("x", Type::Int32, BindingKind::Uninitialized);

        scope = scope.with_function(parse_func_decl(
            "procedure SetX(out xout: System.Int32)",
            Rc::new(Scope::default()),
        ));

        assert_eq!(false, scope.get_symbol(&Identifier::from("x")).unwrap().initialized());
        let (_, scope) = parse_expr("SetX(x)", Rc::new(scope));
        assert_eq!(true, scope.get_symbol(&Identifier::from("x")).unwrap().initialized());
    }

    #[test]
    fn assignment_initializes_value() {
        let scope = Scope::default()
            .with_binding("x", Type::Int32, BindingKind::Uninitialized);

        assert_eq!(false, scope.get_symbol(&Identifier::from("x")).unwrap().initialized());
        let (_, scope) = parse_expr("x := 1", Rc::new(scope));
        assert_eq!(true, scope.get_symbol(&Identifier::from("x")).unwrap().initialized());
    }

    #[test]
    fn initialization_in_both_branches_of_if_propagates() {
        let scope = Scope::default()
            .with_binding("x", Type::Int32, BindingKind::Uninitialized);

        assert_eq!(false, scope.get_symbol(&Identifier::from("x")).unwrap().initialized());
        let (_, scope) = parse_expr("if true then x := 1 else x := 2", Rc::new(scope));
        assert_eq!(true, scope.get_symbol(&Identifier::from("x")).unwrap().initialized());
    }

    #[test]
    fn initialization_in_one_branches_of_if_propagates() {
        let scope = Scope::default()
            .with_binding("x", Type::Int32, BindingKind::Uninitialized);

        assert_eq!(false, scope.get_symbol(&Identifier::from("x")).unwrap().initialized());
        let (_, scope) = parse_expr("if true then x := 1 else begin end", Rc::new(scope));
        assert_eq!(false, scope.get_symbol(&Identifier::from("x")).unwrap().initialized());
    }

    #[test]
    fn initialization_in_if_without_else_doesnt_propagate() {
        let scope = Scope::default()
            .with_binding("x", Type::Int32, BindingKind::Uninitialized);

        assert_eq!(false, scope.get_symbol(&Identifier::from("x")).unwrap().initialized());
        let (_, scope) = parse_expr("if true then x := 1", Rc::new(scope));
        assert_eq!(false, scope.get_symbol(&Identifier::from("x")).unwrap().initialized());
    }

    #[test]
    fn binding_in_single_branch_of_if_doesnt_propagate() {
        let scope = Scope::default();

        let (_, scope) = parse_expr("if true then let y = 1", Rc::new(scope));
        assert_eq!(None, scope.get_symbol(&Identifier::from("y")));
    }

    #[test]
    fn initialization_in_block_propagates() {
        let scope = Scope::default()
            .with_binding("x", Type::Int32, BindingKind::Uninitialized);

        assert_eq!(false, scope.get_symbol(&Identifier::from("x")).unwrap().initialized());
        let (_, scope) = parse_expr("begin x := 1 end", Rc::new(scope));
        assert_eq!(true, scope.get_symbol(&Identifier::from("x")).unwrap().initialized());
    }

    #[test]
    fn binding_in_block_doesnt_propagate() {
        let scope = Scope::default();

        let (_, scope) = parse_expr("begin let y = 0 end", Rc::new(scope));
        assert_eq!(None, scope.get_symbol(&Identifier::from("y")));
    }

    #[test]
    fn binding_in_while_body_doesnt_propagate() {
        let scope = Scope::default();

        let (_, scope) = parse_expr("while false do let y = 0", Rc::new(scope));
        assert_eq!(None, scope.get_symbol(&Identifier::from("y")));
    }

    #[test]
    fn initialization_in_while_body_doesnt_propagate() {
        let scope = Scope::default()
            .with_binding("x", Type::Int32, BindingKind::Uninitialized);

        assert_eq!(false, scope.get_symbol(&Identifier::from("x")).unwrap().initialized());
        let (_, scope) = parse_expr("while false do x := 1", Rc::new(scope));
        assert_eq!(false, scope.get_symbol(&Identifier::from("x")).unwrap().initialized());
    }

    #[test]
    fn initialization_in_while_condition_propagates() {
        let scope = Scope::default()
            .with_function(parse_func_decl(
                "function GetX(out xOut: System.Int32): System.Boolean",
                Rc::new(Scope::default()),
            ))
            .with_binding("x", Type::Int32, BindingKind::Uninitialized);

        assert_eq!(false, scope.get_symbol(&Identifier::from("x")).unwrap().initialized());
        let (_, scope) = parse_expr("while GetX(x) do begin end", Rc::new(scope));
        assert_eq!(true, scope.get_symbol(&Identifier::from("x")).unwrap().initialized());
    }

    #[test]
    fn initialization_in_for_from_propagates() {
        let scope = Scope::default()
            .with_function(parse_func_decl(
                "function GetX(out xOut: System.Int32): System.Int32",
                Rc::new(Scope::default()),
            ))
            .with_binding("x", Type::Int32, BindingKind::Uninitialized);

        assert_eq!(false, scope.get_symbol(&Identifier::from("x")).unwrap().initialized());
        let (_, scope) = parse_expr("for let i = GetX(x) to 10 do begin end", Rc::new(scope));
        assert_eq!(true, scope.get_symbol(&Identifier::from("x")).unwrap().initialized());
    }

    #[test]
    fn initialization_in_for_to_propagates() {
        let scope = Scope::default()
            .with_function(parse_func_decl(
                "function GetX(out xOut: System.Int32): System.Int32",
                Rc::new(Scope::default()),
            ))
            .with_binding("x", Type::Int32, BindingKind::Uninitialized);

        assert_eq!(false, scope.get_symbol(&Identifier::from("x")).unwrap().initialized());
        let (_, scope) = parse_expr("for let i = 0 to GetX(x) do begin end", Rc::new(scope));
        assert_eq!(true, scope.get_symbol(&Identifier::from("x")).unwrap().initialized());
    }

    #[test]
    fn initialization_in_for_body_doesnt_propagate() {
        let scope = Scope::default()
            .with_binding("x", Type::Int32, BindingKind::Uninitialized);

        assert_eq!(false, scope.get_symbol(&Identifier::from("x")).unwrap().initialized());
        let (_, scope) = parse_expr("for let i = 0 to 10 do x := 1", Rc::new(scope));
        assert_eq!(false, scope.get_symbol(&Identifier::from("x")).unwrap().initialized());
    }

    #[test]
    fn binding_in_for_range_doesnt_propagate() {
        let scope = Scope::default();

        let (_, scope) = parse_expr("for let i = 0 to 3 do begin end", Rc::new(scope));
        assert_eq!(None, scope.get_symbol(&Identifier::from("i")));
    }

    #[test]
    fn binding_in_for_body_doesnt_propagate() {
        let scope = Scope::default();

        let (_, scope) = parse_expr("for let i = 0 to 3 do let y = 1", Rc::new(scope));
        assert_eq!(None, scope.get_symbol(&Identifier::from("y")));
    }
}