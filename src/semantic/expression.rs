use std::rc::Rc;
use syntax;
use semantic::*;
use node::{
    self,
    Identifier,
    ExpressionValue,
    ConstantExpression,
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
                       context: &syntax::ParsedContext) -> SemanticResult<Expression> {
    let cond_expr = Expression::annotate(condition, scope.clone())?;
    let body_expr = Expression::annotate(body, scope.clone())?;

    let context = SemanticContext {
        token: context.token().clone(),
        scope,
    };

    Ok(Expression::while_loop(cond_expr, body_expr, context))
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
               context: &syntax::ParsedContext) -> SemanticResult<Expression>
{
    let cond_expr = Expression::annotate(condition, scope.clone())?;
    let then_expr = Expression::annotate(then_branch, scope.clone())?;
    let else_expr = match else_branch {
        Some(expr) => Some(Expression::annotate(expr, scope.clone())?),
        None => None
    };

    Ok(Expression::if_then_else(cond_expr, then_expr, else_expr, SemanticContext {
        token: context.token().clone(),
        scope,
    }))
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
                     context: &syntax::ParsedContext) -> SemanticResult<Expression> {
    let from_expr = Expression::annotate(from, scope.clone())?;
    let to_expr = Expression::annotate(to, scope.clone())?;

    let do_expr = if from_expr.is_let_binding() {
        let (from_name, from_value) = from_expr.clone().unwrap_let_binding();
        let from_type = from_value.expr_type()?
            .ok_or_else(|| SemanticError::type_not_assignable(
                None, from_expr.context.clone()))?;

        let body_scope = Rc::new(scope.as_ref().clone()
            .with_symbol_local(&from_name, from_type));

        Expression::annotate(body, body_scope)?
    } else {
        Expression::annotate(body, scope.clone())?
    };

    Ok(Expression::for_loop(from_expr, to_expr, do_expr, SemanticContext {
        token: context.token().clone(),
        scope: scope.clone(),
    }))
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

fn annotate_ufcs(target: &Expression,
                 target_type: &Type,
                 func_name: &str,
                 args: &Vec<Expression>,
                 scope: Rc<Scope>) -> Option<Expression> {
    let (base_target_id, _base_target_decl) = match target_type.remove_indirection() {
        Type::Class(name) => scope.get_class(name)?,
        Type::Record(name) => scope.get_record(name)?,
        _ => return None,
    };

    /* look for matching UFCS func in NS of target type */
    let ufcs_ns = base_target_id.parent()?;

    let ufcs_name = ufcs_ns.child(func_name);
    let ufcs_function = scope.get_symbol(&ufcs_name)?;

    match ufcs_function {
        ScopedSymbol::Local { name: ufcs_func_name, decl_type: Type::Function(sig) } => {
            let first_arg_type = sig.arg_types.first()?;

            if first_arg_type.remove_indirection() != target_type.remove_indirection() {
                None
            } else {
                /* match target arg expression indirection level to level of expected first arg,
                 taking the address or derefing the pointer as necessary */
                let ufcs_target_arg = match_indirection(&target, target_type, &first_arg_type);

                /* the target becomes the first arg */
                let mut ufcs_args = vec![ufcs_target_arg];
                ufcs_args.extend(args.iter().cloned());

                let ufcs_target_expr = Expression::identifier(ufcs_func_name.clone(),
                                                              target.context.clone());

                Some(Expression::function_call(ufcs_target_expr, ufcs_args))
            }
        }

        _ => None,
    }
}

fn annotate_function_call(target: &syntax::Expression,
                          args: &Vec<syntax::Expression>,
                          scope: Rc<Scope>,
                          _context: &syntax::ParsedContext) -> SemanticResult<Expression> {
    let typed_args = args.iter()
        .map(|arg| Expression::annotate(arg, scope.clone()))
        .collect::<Result<Vec<_>, _>>()?;

    /* expressions of the form x.a() should first check if a is function in the same namespace as
    type A, taking an A as the first param, and invoke it using UFCS instead if so */
    let ufcs_call = match &target.value {
        &ExpressionValue::Member { ref of, ref name } => {
            let base_expr = Expression::annotate(of, scope.clone())?;
            let base_type = base_expr.expr_type()?;

            base_type.and_then(|base_type|
                annotate_ufcs(&base_expr, &base_type, &name, &typed_args, scope.clone()))
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
            /* ordinary function call */
            let target = Expression::annotate(target, scope)?;

            Ok(Expression::function_call(target, typed_args))
        }
    }
}

fn let_binding_type(value: &Expression, context: &SemanticContext) -> SemanticResult<Option<Type>> {
    let value_type = value.expr_type()?
        .ok_or_else(|| SemanticError::type_not_assignable(None, context.clone()))?;

    if !value_type.valid_lhs_type() {
        return Err(SemanticError::type_not_assignable(None, context.clone()));
    }

    Ok(None)
}

fn function_call_type(target: &Expression,
                      args: &Vec<Expression>,
                      context: &SemanticContext) -> SemanticResult<Option<Type>> {
    let target_type = target.expr_type()?;
    if let Some(Type::Function(sig)) = &target_type {
        if args.len() != sig.arg_types.len() {
            Err(SemanticError::wrong_num_args(sig.as_ref().clone(),
                                              args.len(),
                                              context.clone()))
        } else {
            for (arg_index, arg_expr) in args.iter().enumerate() {
                let sig_type = &sig.arg_types[arg_index];

                expect_valid_operation(operators::Assignment,
                                       Some(sig_type),
                                       &arg_expr,
                                       &arg_expr.context)?;
            }

            Ok(sig.return_type.clone())
        }
    } else {
        Err(SemanticError::invalid_function_type(target_type.clone(), context.clone()))
    }
}

fn is_lvalue(expr: &Expression) -> bool {
    match &expr.value {
        ExpressionValue::Identifier(name) => {
            let is_declared_func = expr.scope().get_function(&name).is_some();
            !is_declared_func
        }
        ExpressionValue::PrefixOperator { op, .. } => match op {
            operators::Deref => true,
            _ => false,
        },

        ExpressionValue::ArrayElement { of, .. } |
        ExpressionValue::Member { of, .. } => {
            is_lvalue(of)
        }

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
            if !is_lvalue(lhs) {
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

    /* treat records and classes the same for this purpose */
    let base_decl = match &base_type {
        Some(Type::Record(name)) => of.scope().get_record(name),
        Some(Type::Class(name)) => of.scope().get_class(name),
        _ => None,
    };

    match base_decl {
        Some((_record_id, record)) => {
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

impl Expression {
    pub fn annotate(expr: &syntax::Expression, scope: Rc<Scope>) -> SemanticResult<Self> {
        let expr_context = SemanticContext {
            token: expr.context.token().clone(),
            scope: scope.clone(),
        };

        match &expr.value {
            ExpressionValue::Identifier(name) =>
                annotate_identifier(name, scope, &expr.context),

            ExpressionValue::Block(block) => {
                Ok(Expression::block(Block::annotate(block, scope)?))
            }

            ExpressionValue::LetBinding { name, value } => {
                /* it's not our job to update the scope, the enclosing block
                needs to do that (it's responsible for the scope passed to its
                enclosed statements) */
                let typed_value = Expression::annotate(value, scope)?;

                Ok(Expression::let_binding(name, typed_value, expr_context))
            }

            ExpressionValue::Constant(ConstantExpression::String(s)) => {
                Ok(Expression::literal_string(s, expr_context))
            }

            ExpressionValue::Constant(ConstantExpression::Integer(i)) => {
                Ok(Expression::literal_int(*i, expr_context))
            }

            ExpressionValue::Constant(ConstantExpression::Nil) => {
                Ok(Expression::literal_nil(expr_context))
            }

            ExpressionValue::Constant(ConstantExpression::Boolean(b)) => {
                Ok(Expression::literal_bool(*b, expr_context))
            }

            ExpressionValue::Constant(ConstantExpression::Float(f)) => {
                Ok(Expression::literal_float(*f, expr_context))
            }

            ExpressionValue::Constant(ConstantExpression::Enum(e)) => {
                Ok(Expression::literal_enumeration(e.clone(), expr_context))
            }

            ExpressionValue::Constant(ConstantExpression::Set(set_const)) => {
                Ok(Expression::literal_set(set_const.clone(), expr_context))
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
                let lhs = Expression::annotate(lhs, scope.clone())?;
                let rhs = Expression::annotate(rhs, scope.clone())?;

                Ok(Expression::binary_op(lhs, op.clone(), rhs, expr_context))
            }

            ExpressionValue::PrefixOperator { op, rhs } => {
                Ok(Expression::prefix_op(
                    op.clone(),
                    Expression::annotate(rhs, scope)?,
                    expr_context,
                ))
            }

            ExpressionValue::FunctionCall { target, args } =>
                annotate_function_call(target, args, scope, &expr.context),

            ExpressionValue::Member { of, name } => {
                let typed_of = Expression::annotate(of, scope)?;
                Ok(Expression::member(typed_of, name))
            }

            ExpressionValue::ArrayElement { of, index_expr } => {
                let of = Expression::annotate(of.as_ref(), scope.clone())?;
                let index_expr = Expression::annotate(index_expr.as_ref(), scope.clone())?;
                Ok(Expression::array_element(of, index_expr))
            }

            ExpressionValue::SetConstructor(_members) => {
                unimplemented!("set constructor semantic analysis")
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
                let _of_type = of.expr_type()?;
                let _index_type = index_expr.expr_type()?;

                unimplemented!("array element typechecking")
            }

            ExpressionValue::SetConstructor(_members) => {
                unimplemented!("set constructor typechecking")
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

    use tokens;
    use source;
    use keywords;
    use tokenizer::*;
    use semantic::*;
    use syntax;
    use node::{
        FunctionKind,
        ExpressionValue,
    };
    use operators;
    use types::Type;
    use opts::CompileOptions;

    fn empty_context(scope: &Scope) -> SemanticContext {
        SemanticContext {
            scope: Rc::new(scope.clone()),
            token: source::Token::new(tokens::Keyword(keywords::Program),
                                      source::Location::new("test", 0, 0)),
        }
    }

    pub fn parse_expr(src: &str, scope: Rc<Scope>) -> Expression {
        let tokens = tokenize("test", src, &CompileOptions::default())
            .expect(&format!("test expr `{}` must not contain illegal tokens", src));

        let mut stream = syntax::TokenStream::from(tokens);

        let parsed: syntax::Expression = stream.parse()
            .expect(&format!("test expr `{}` must parse correctly", src));

        stream.finish().expect("expr must not contain trailing tokens");

        Expression::annotate(&parsed, scope)
            .expect(&format!("test expr `{}` must have valid types", src))
    }

    #[test]
    fn assignment_to_wrong_type_is_err() {
        let scope = Scope::default()
            .with_symbol_local("x", Type::RawPointer);

        let expr = parse_expr("x := 1", Rc::new(scope));

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
        let test_func_name = Identifier::from("System.TestAdd");

        let default_scope = Scope::default();

        let scope = default_scope.clone()
            .with_function(FunctionDecl {
                name: test_func_name.name.clone(),
                return_type: Some(Type::Int64),
                modifiers: Vec::new(),
                args: vec![
                    FunctionArg {
                        context: empty_context(&default_scope),
                        modifier: None,
                        name: "x".to_string(),
                        decl_type: Type::Int64,
                        default_value: None,
                    }
                ],
                kind: FunctionKind::Function,
                context: empty_context(&default_scope),
            })
            .with_symbol_local("a", Type::Int64);

        let expr = parse_expr(r"a.TestAdd(1)", Rc::new(scope.clone()));

        match expr.value {
            ExpressionValue::FunctionCall { target, args } => {
                assert!(target.is_identifier(&test_func_name));
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
            .with_symbol_local("x", Type::Byte.pointer());

        let expr = parse_expr("^x", Rc::new(scope));
        assert_eq!(Type::Byte, expr.expr_type().unwrap().unwrap());
    }

    #[test]
    fn type_of_pointer_plus_offset_is_pointer() {
        let scope = Scope::default()
            .with_symbol_local("x", Type::Byte.pointer());

        let expr = parse_expr("x + 1", Rc::new(scope));
        assert_eq!(Type::Byte.pointer(), expr.expr_type().unwrap().unwrap());
    }
}