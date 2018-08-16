use syntax;
use semantic::*;
use node::{self, Identifier};
use operators;
use types;
use source;
use types::DeclaredType;

pub type Expression = node::Expression<ScopedSymbol>;

fn expect_valid_operation(operator: operators::Operator,
                          target: Option<&DeclaredType>,
                          actual: Option<&DeclaredType>,
                          context: &source::Token) -> SemanticResult<()> {
    if operator == operators::Equals || operator == operators::NotEquals {
        return expect_comparable(target, actual, context);
    }

    match (target, actual) {
        (None, _) =>
            Err(SemanticError::type_not_assignable(None, context.clone())),
        (_, None) =>
            Err(SemanticError::unexpected_type(target.cloned(), None, context.clone())),

        (Some(a), Some(b)) => {
            let valid = match operator {
                operators::Assignment => a.assignable_from(b),

                operators::Plus |
                operators::Minus => a.can_offset_by(b),

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

fn expect_comparable(target: Option<&DeclaredType>,
                     actual: Option<&DeclaredType>,
                     context: &source::Token) -> SemanticResult<()> {
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
                       scope: &Scope,
                       context: &source::Token)
                       -> SemanticResult<Expression> {
    scope.get_symbol(name)
        .map(|symbol| match symbol {
            /* transform identifiers that reference record members into
            record member expressions */
            ScopedSymbol::RecordMember { record_id, name, .. } => {
                let record_sym = scope.get_symbol(&record_id).unwrap();

                let base = Expression::identifier(record_sym, context.clone());

                Expression::member(base, &name)
            }

            symbol @ ScopedSymbol::Local { .. } => {
                Expression::identifier(symbol, context.clone())
            }
        })
        .ok_or_else(|| {
            SemanticError::unknown_symbol(name.clone(), context.clone())
        })
}

fn annotate_if(condition: &syntax::Expression,
               then_branch: &syntax::Expression,
               else_branch: Option<&syntax::Expression>,
               scope: &Scope,
               context: &source::Token) -> SemanticResult<Expression>
{
    let cond_expr = Expression::annotate(condition, scope)?;
    let then_expr = Expression::annotate(then_branch, scope)?;
    let else_expr = match else_branch {
        Some(expr) => Some(Expression::annotate(expr, scope)?),
        None => None
    };

    Ok(Expression::if_then_else(cond_expr, then_expr, else_expr, context.clone()))
}

fn if_type(condition: &Expression,
           then_branch: &Expression,
           else_branch: Option<&Expression>,
           context: &source::Token) -> SemanticResult<Option<DeclaredType>> {
    let condition_type = condition.expr_type()?;

    expect_comparable(Some(&DeclaredType::Boolean),
                      condition_type.as_ref(),
                      context)?;

    let _then_type = then_branch.expr_type()?;

    if let Some(else_expr) = else_branch {
        else_expr.expr_type()?;
    }

    Ok(None)
}

fn annotate_for_loop(from: &syntax::Expression,
                     to: &syntax::Expression,
                     body: &syntax::Expression,
                     scope: &Scope,
                     context: &source::Token) -> SemanticResult<Expression> {
    let from_expr = Expression::annotate(from, scope)?;
    let to_expr = Expression::annotate(to, scope)?;

    let do_expr = if from_expr.is_let_binding() {
        let (from_name, from_value) = from_expr.clone().unwrap_let_binding();
        let from_type = from_value.expr_type()?
            .ok_or_else(|| SemanticError::type_not_assignable(
                None, from_expr.context.clone()))?;

        let body_scope = scope.clone()
            .with_symbol_local(&from_name, from_type);

        Expression::annotate(body, &body_scope)?
    } else {
        Expression::annotate(body, scope)?
    };

    Ok(Expression::for_loop(from_expr, to_expr, do_expr, context.clone()))
}

fn for_loop_type(from: &Expression,
                 to: &Expression,
                 body: &Expression,
                 context: &source::Token) -> SemanticResult<Option<DeclaredType>> {
    let from_type = from.expr_type()?;
    let to_type = to.expr_type()?;

    expect_comparable(Some(&DeclaredType::Integer), to_type.as_ref(),
                      context)?;

    let _body_type = body.expr_type()?;

    match &from.value {
        &node::ExpressionValue::BinaryOperator { ref op, ref lhs, .. }
        if *op == operators::Operator::Assignment => {
            let lhs_type = lhs.expr_type()?;

            expect_comparable(Some(&DeclaredType::Integer), lhs_type.as_ref(),
                              context)?;
            Ok(None)
        }

        &node::ExpressionValue::LetBinding { ref value, .. } => {
            let value_type = value.expr_type()?;

            expect_comparable(Some(&DeclaredType::Integer), value_type.as_ref(),
                              context)?;
            Ok(None)
        }

        //TODO better error
        _ => Err(SemanticError::unexpected_type(
            Some(types::DeclaredType::Integer),
            from_type,
            context.clone()))
    }
}

fn match_indirection(expr: &Expression,
                     expr_type: &DeclaredType,
                     target_type: &DeclaredType) -> Expression {
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
                 target_type: &DeclaredType,
                 func_name: &str,
                 args: &Vec<Expression>,
                 scope: &scope::Scope) -> Option<Expression> {
    let target_type_name = DeclaredType::name(Some(target_type.remove_indirection()));

    /* look for matching UFCS func in NS of target type */
    let ufcs_ns = Identifier::from(&target_type_name).parent()?;

    let ufcs_name = ufcs_ns.child(func_name);
    let ufcs_function = scope.get_symbol(&ufcs_name)?;

    match ufcs_function.decl_type() {
        DeclaredType::Function(sig) => {
            let first_arg = sig.arg_types.first()?;

            if first_arg.remove_indirection() != target_type.remove_indirection() {
                None
            } else {
                /* match target arg expression indirection level to level of expected first arg,
                 taking the address or derefing the pointer as necessary */
                let ufcs_target_arg = match_indirection(&target, target_type, &first_arg);

                /* the target becomes the first arg */
                let mut ufcs_args = vec![ufcs_target_arg];
                ufcs_args.extend(args.iter().cloned());

                let ufcs_target_expr = Expression::identifier(ufcs_function,
                                                              target.context.clone());

                Some(Expression::function_call(ufcs_target_expr, ufcs_args))
            }
        }

        _ => None
    }
}

fn annotate_function_call(target: &syntax::Expression,
                          args: &Vec<syntax::Expression>,
                          scope: &Scope,
                          _context: &source::Token) -> SemanticResult<Expression> {
    let typed_args = args.iter()
        .map(|arg| Expression::annotate(arg, scope))
        .collect::<Result<Vec<_>, _>>()?;

    /* expressions of the form x.a() should first check if a is function in the same namespace as
    type A, taking an A as the first param, and invoke it using UFCS instead if so */
    let ufcs_call = match &target.value {
        &node::ExpressionValue::Member { ref of, ref name } => {
            let base_expr = Expression::annotate(of, scope)?;
            let base_type = base_expr.expr_type()?;

            base_type.and_then(|base_type|
                annotate_ufcs(&base_expr, &base_type, &name, &typed_args, scope))
        }

        &node::ExpressionValue::Identifier(ref name) => {
            let base_id: Option<Identifier> = name.0.parent();

            base_id.and_then(|base_id| scope.get_symbol(&base_id))
                .and_then(|base_sym| {
                    let func_name = &name.0.name;
                    let base_expr = Expression::identifier(base_sym.clone(), target.context.clone());

                    annotate_ufcs(&base_expr, &base_sym.decl_type(), func_name, &typed_args, scope)
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

fn let_binding_type(value: &Expression,
                    context: &source::Token)
                    -> SemanticResult<Option<DeclaredType>> {
    let value_type = value.expr_type()?
        .ok_or_else(|| SemanticError::type_not_assignable(None, context.clone()))?;

    if !value_type.valid_lhs_type() {
        return Err(SemanticError::type_not_assignable(None, context.clone()));
    }

    Ok(None)
}

fn function_call_type(target: &Expression,
                      args: &Vec<Expression>,
                      context: &source::Token) -> SemanticResult<Option<DeclaredType>> {
    let target_type = target.expr_type()?;
    if let Some(DeclaredType::Function(ref sig)) = target_type {
        if args.len() != sig.arg_types.len() {
            Err(SemanticError::wrong_num_args(sig.as_ref().clone(),
                                              args.len(),
                                              context.clone()))
        } else {
            for (arg_index, arg_expr) in args.iter().enumerate() {
                arg_expr.expr_type()?;

                let sig_type = sig.arg_types[arg_index].clone();
                let actual_type = arg_expr.expr_type()?;

                expect_valid_operation(operators::Assignment,
                                       Some(&sig_type),
                                       actual_type.as_ref(),
                                       &arg_expr.context)?
            }

            Ok(sig.return_type.clone())
        }
    } else {
        Err(SemanticError::invalid_function_type(target_type,
                                                 context.clone()))
    }
}

fn binary_op_type(lhs: &Expression,
                  op: operators::Operator,
                  rhs: &Expression,
                  context: &source::Token) -> SemanticResult<Option<DeclaredType>> {
    let lhs_type = lhs.expr_type()?;
    let rhs_type = rhs.expr_type()?;

    match op {
        operators::NotEquals |
        operators::Equals => {
            expect_valid_operation(op, lhs_type.as_ref(), rhs_type.as_ref(), context)?;
            Ok(Some(DeclaredType::Boolean))
        }
        operators::Plus |
        operators::Minus => {
            expect_valid_operation(op, lhs_type.as_ref(), rhs_type.as_ref(), context)?;
            Ok(lhs_type)
        }

        operators::Assignment => {
            expect_valid_operation(op, lhs_type.as_ref(), rhs_type.as_ref(), context)?;
            Ok(None)
        }

        operators::AddressOf |
        operators::Deref => {
            Err(SemanticError::invalid_operator(op,
                                                vec![lhs_type, rhs_type],
                                                context.clone()))
        }
    }
}

fn prefix_op_type(op: operators::Operator,
                  rhs: &Expression,
                  context: &source::Token)
                  -> SemanticResult<Option<DeclaredType>> {
    let rhs_type = rhs.expr_type()?;

    let invalid_op_err = ||
        Err(SemanticError::invalid_operator(op.clone(),
                                            vec![rhs_type.clone()],
                                            context.clone()));
    match op {
        operators::Deref => match &rhs_type {
            &Some(DeclaredType::Pointer(ref pointed_to)) =>
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
            &Some(DeclaredType::Integer) |
            &Some(DeclaredType::Byte) =>
                Ok(rhs_type.clone()),
            _ =>
                invalid_op_err(),
        }

        operators::Assignment |
        operators::Equals |
        operators::NotEquals =>
            invalid_op_err(),
    }
}

fn member_type(of: &Expression,
               name: &str,
               _context: &source::Token) -> SemanticResult<Option<DeclaredType>> {
    let base_type = of.expr_type()?
        .map(|dt| dt.remove_indirection().clone());

    match base_type {
        Some(DeclaredType::Record(ref record)) => {
            match record.get_member(name) {
                Some(member) =>
                    Ok(Some(member.decl_type.clone())),
                None => {
                    let name_id = Identifier::from(name);
                    Err(SemanticError::unknown_symbol(name_id, of.context.clone()))
                }
            }
        }

        bad @ _ => {
            Err(SemanticError::member_of_non_record(bad,
                                                    name.to_owned(),
                                                    of.context.clone()))
        }
    }
}

impl Expression {
    pub fn annotate(expr: &syntax::Expression, scope: &Scope) -> SemanticResult<Self> {
        match &expr.value {
            &node::ExpressionValue::Identifier(ref name) =>
                annotate_identifier(&name.0, scope, &expr.context),

            &node::ExpressionValue::Block(ref block) => {
                Ok(Expression::block(Block::annotate(block, scope)?))
            }

            &node::ExpressionValue::LetBinding { ref name, ref value } => {
                /* it's not our job to update the scope, the enclosing block
                needs to do that (it's responsible for the scope passed to its
                enclosed statements) */
                let typed_value = Expression::annotate(value, scope)?;

                Ok(Expression::let_binding(expr.context.clone(), name, typed_value))
            }

            &node::ExpressionValue::LiteralString(ref s) => {
                Ok(Expression::literal_string(s, expr.context.clone()))
            }

            &node::ExpressionValue::LiteralInteger(i) => {
                Ok(Expression::literal_int(i, expr.context.clone()))
            }

            &node::ExpressionValue::LiteralNil => {
                Ok(Expression::literal_nil(&expr.context))
            }

            &node::ExpressionValue::If { ref condition, ref then_branch, ref else_branch } =>
                annotate_if(condition.as_ref(),
                            then_branch.as_ref(),
                            else_branch.as_ref().map(|else_expr| else_expr.as_ref()),
                            scope,
                            &expr.context),

            &node::ExpressionValue::ForLoop { ref from, ref to, ref body } =>
                annotate_for_loop(from, to, body, scope, &expr.context),

            &node::ExpressionValue::BinaryOperator { ref lhs, op, ref rhs } => {
                let lhs = Expression::annotate(lhs, scope)?;
                let rhs = Expression::annotate(rhs, scope)?;

                let lhs_type = lhs.expr_type()?;
                let rhs_type = rhs.expr_type()?;

                let string_ptr_type = scope.get_type(&Identifier::from("System.String"))
                    .unwrap()
                    .pointer();

                let lhs_is_string = lhs_type.map(|ty| ty == string_ptr_type).unwrap_or(false);
                let rhs_is_string = rhs_type.map(|ty| ty == string_ptr_type).unwrap_or(false);

                if lhs_is_string && rhs_is_string && op == operators::Plus {
                    //desugar string concatenation
                    let strcat_sym = scope.get_symbol(&Identifier::from("System.StringConcat")).unwrap();
                    let strcat = Expression::identifier(strcat_sym, expr.context.clone());

                    Ok(Expression::function_call(strcat, vec![lhs, rhs]))
                } else {
                    Ok(Expression::binary_op(lhs, op.clone(), rhs, expr.context.clone()))
                }
            }

            &node::ExpressionValue::PrefixOperator { ref op, ref rhs } => {
                Ok(Expression::prefix_op(
                    op.clone(),
                    Expression::annotate(rhs, scope)?,
                    expr.context.clone(),
                ))
            }

            &node::ExpressionValue::FunctionCall { ref target, ref args } =>
                annotate_function_call(target, args, scope, &expr.context),

            &node::ExpressionValue::Member { ref of, ref name } => {
                let typed_of = Expression::annotate(of, scope)?;
                Ok(Expression::member(typed_of, name))
            }
        }
    }

    pub fn expr_type(&self) -> Result<Option<DeclaredType>, SemanticError> {
        match &self.value {
            &node::ExpressionValue::BinaryOperator { ref lhs, ref op, ref rhs } =>
                binary_op_type(lhs, *op, rhs, &self.context),

            &node::ExpressionValue::PrefixOperator { ref op, ref rhs } =>
                prefix_op_type(*op, rhs, &self.context),

            &node::ExpressionValue::LiteralString(_) =>
                Ok(Some(DeclaredType::Byte.pointer())),

            &node::ExpressionValue::LiteralInteger(_) =>
                Ok(Some(DeclaredType::Integer)),

            &node::ExpressionValue::LiteralNil =>
                Ok(Some(DeclaredType::Nil)),

            &node::ExpressionValue::LetBinding { ref value, .. } =>
                let_binding_type(value, &self.context),

            &node::ExpressionValue::FunctionCall { ref target, ref args } =>
                function_call_type(target, args, &self.context),

            &node::ExpressionValue::Identifier(ref id) =>
                Ok(Some(id.decl_type().clone())),

            &node::ExpressionValue::Block(ref block) => {
                for statement in block.statements.iter() {
                    statement.expr_type()?;
                }

                Ok(None)
            }

            &node::ExpressionValue::If { ref condition, ref then_branch, ref else_branch } =>
                if_type(condition.as_ref(),
                        then_branch.as_ref(),
                        else_branch.as_ref().map(|else_expr| else_expr.as_ref()),
                        &self.context),

            &node::ExpressionValue::ForLoop { ref from, ref to, ref body } =>
                for_loop_type(from.as_ref(), to.as_ref(), body.as_ref(), &self.context),

            &node::ExpressionValue::Member { ref of, ref name } =>
                member_type(of, name, &self.context)
        }
    }

    pub fn type_check(&self) -> Result<(), SemanticError> {
        self.expr_type()?;
        Ok(())
    }
}

#[cfg(test)]
mod test {
    use tokenizer::*;
    use semantic::*;
    use syntax;
    use node;
    use operators;
    use source;
    use types::{DeclaredType, FunctionSignature};

    fn parse_expr(src: &str, scope: &Scope) -> Expression {
        let tokens = tokenize("test", src)
            .expect(&format!("test expr `{}` must not contain illegal tokens", src));

        let parsed = syntax::Expression::parse(tokens, &source::test::empty_context())
            .and_then(|expr_out| expr_out.finish())
            .expect(&format!("test expr `{}` must parse correctly", src));

        Expression::annotate(&parsed, scope)
            .expect(&format!("test expr `{}` must have valid types", src))
    }

    #[test]
    fn assignment_to_wrong_type_is_err() {
        let scope = Scope::default()
            .with_symbol_local("x", DeclaredType::RawPointer);

        let expr = parse_expr("x := 1", &scope);

        match expr.type_check() {
            Err(SemanticError {
                    kind: SemanticErrorKind::InvalidOperator { op, args },
                    ..
                }) => {
                assert_eq!(operators::Assignment, op);
                assert_eq!(Some(DeclaredType::RawPointer), args[0]);
                assert_eq!(Some(DeclaredType::Integer), args[1]);
            }
            _ => panic!("expected invalid types in assignment")
        }
    }

    #[test]
    fn func_call_on_obj_uses_ufcs_from_target_ns() {
        let test_func_name = Identifier::from("System.TestAdd");

        let scope = Scope::default()
            .with_symbol_absolute(test_func_name.clone(),
                                  DeclaredType::Function(Box::from(FunctionSignature {
                                      name: test_func_name.clone(),
                                      return_type: Some(DeclaredType::Integer),
                                      arg_types: vec![
                                          DeclaredType::Integer
                                      ],
                                  })))
            .with_symbol_local("a", DeclaredType::Integer);

        let expr = parse_expr(r"a.TestAdd(1)", &scope);

        match expr.value {
            node::ExpressionValue::FunctionCall { target, args } => {
                let expected_target = scope.get_symbol(&test_func_name).unwrap();

                assert!(target.is_identifier(&expected_target));
                assert_eq!(2, args.len());

                let expected_first_arg = scope.get_symbol(&Identifier::from("a")).unwrap();
                assert!(args[0].is_identifier(&expected_first_arg));

                assert!(args[1].is_literal_integer(1));
            }

            _ => panic!("result should be a function call")
        }
    }

    #[test]
    fn type_of_pointer_deref_is_pointer_minus_indirection() {
        let scope = Scope::default()
            .with_symbol_local("x", DeclaredType::Byte.pointer());

        let expr = parse_expr("^x", &scope);
        assert_eq!(DeclaredType::Byte, expr.expr_type().unwrap().unwrap());
    }

    #[test]
    fn type_of_pointer_plus_offset_is_pointer() {
        let scope = Scope::default()
            .with_symbol_local("x", DeclaredType::Byte.pointer());

        let expr = parse_expr("x + 1", &scope);
        assert_eq!(DeclaredType::Byte.pointer(), expr.expr_type().unwrap().unwrap());
    }
}