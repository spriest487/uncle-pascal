use syntax;
use semantic::*;
use node;
use operators;
use types;
use source;
use types::DeclaredType;

pub type Expression = node::Expression<ScopedSymbol>;

fn expect_assignable(target: Option<&DeclaredType>,
                     actual: Option<&DeclaredType>,
                     context: &source::Token) -> Result<(), SemanticError> {
    match (target, actual) {
        (None, _) =>
            Err(SemanticError::type_not_assignable(None, context.clone())),
        (_, None) =>
            Err(SemanticError::unexpected_type(target.cloned(), None, context.clone())),

        (a @ Some(_), b @ Some(_)) =>
            if a.unwrap().assignable_from(b.unwrap()) {
                Ok(())
            } else {
                Err(SemanticError::unexpected_type(a.cloned(),
                                                   b.cloned(),
                                                   context.clone()))
            },
    }
}

fn expect_comparable(target: Option<&DeclaredType>,
                     actual: Option<&DeclaredType>,
                     context: &source::Token) -> Result<(), SemanticError> {
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

impl Expression {
    pub fn annotate(expr: &syntax::Expression, scope: &Scope)
                    -> Result<Self, SemanticError> {
        match &expr.value {
            &node::ExpressionValue::Identifier(ref name) => {
                scope.get_symbol(name)
                    .map(|symbol| match symbol {
                        /* transform identifiers that reference record members into
                        record member expressions */
                        ScopedSymbol::RecordMember { record_id, name, .. } => {
                            let record_sym = scope.get_symbol(&record_id).unwrap();

                            let base = Expression::identifier(record_sym, expr.context.clone());

                            Expression::member(base, &name)
                        }

                        symbol @ ScopedSymbol::Child { .. } |
                        symbol @ ScopedSymbol::Local { .. } => {
                            Expression::identifier(symbol, expr.context.clone())
                        }
                    })
                    .ok_or_else(|| {
                        SemanticError::unknown_symbol(name.clone(), expr.context.clone())
                    })
            }

            &node::ExpressionValue::Block(ref block) => {
                Ok(Expression::block(Block::annotate(block, scope)?))
            }

            &node::ExpressionValue::LiteralString(ref s) => {
                Ok(Expression::literal_string(s, expr.context.clone()))
            }

            &node::ExpressionValue::LiteralInteger(i) => {
                Ok(Expression::literal_int(i, expr.context.clone()))
            }

            &node::ExpressionValue::If { ref condition, ref then_branch, ref else_branch } => {
                let cond_expr = Expression::annotate(condition, scope)?;
                let then_expr = Expression::annotate(then_branch, scope)?;
                let else_expr = match else_branch {
                    &None => None,
                    &Some(ref expr) => Some(Expression::annotate(expr.as_ref(), scope)?),
                };

                Ok(Expression::if_then_else(cond_expr, then_expr, else_expr, expr.context.clone()))
            }

            &node::ExpressionValue::ForLoop { ref from, ref to, ref body } => {
                let from_expr = Expression::annotate(from, scope)?;
                let to_expr = Expression::annotate(to, scope)?;
                let do_expr = Expression::annotate(body, scope)?;

                Ok(Expression::for_loop(from_expr, to_expr, do_expr, expr.context.clone()))
            }

            &node::ExpressionValue::BinaryOperator { ref lhs, ref op, ref rhs } => {
                Ok(Expression::binary_op(
                    Expression::annotate(lhs, scope)?,
                    op.clone(),
                    Expression::annotate(rhs, scope)?,
                    expr.context.clone(),
                ))
            }

            &node::ExpressionValue::PrefixOperator { ref op, ref rhs } => {
                Ok(Expression::prefix_op(
                    op.clone(),
                    Expression::annotate(rhs, scope)?,
                    expr.context.clone(),
                ))
            }

            &node::ExpressionValue::FunctionCall { ref target, ref args } => {
                let typed_args = args.iter()
                    .map(|arg| Expression::annotate(arg, scope))
                    .collect::<Result<Vec<_>, _>>()?;

                let target = Expression::annotate(target, scope)?;

                Ok(Expression::function_call(target, typed_args))
            }

            &node::ExpressionValue::Member { ref of, ref name } => {
                let typed_of = Expression::annotate(of, scope)?;
                Ok(Expression::member(typed_of, name))
            }
        }
    }

    pub fn expr_type(&self) -> Result<Option<DeclaredType>, SemanticError> {
        match &self.value {
            &node::ExpressionValue::BinaryOperator { ref lhs, ref op, ref rhs } => {
                let lhs_type = lhs.expr_type()?;
                let rhs_type = rhs.expr_type()?;

                match op {
                    &operators::NotEquals |
                    &operators::Equals => {
                        expect_comparable(lhs_type.as_ref(), rhs_type.as_ref(), &self.context)?;
                        Ok(Some(DeclaredType::Boolean))
                    }

                    &operators::Assignment => {
                        expect_assignable(lhs_type.as_ref(), rhs_type.as_ref(), &self.context)?;
                        Ok(None)
                    }

                    &operators::Plus |
                    &operators::Minus => {
                        expect_assignable(lhs_type.as_ref(), rhs_type.as_ref(), &self.context)?;
                        Ok(lhs_type)
                    }

                    &operators::AddressOf |
                    &operators::Deref => {
                        Err(SemanticError::invalid_operator(*op,
                                                            vec![lhs_type, rhs_type],
                                                            self.context.clone()))
                    }
                }
            }

            &node::ExpressionValue::PrefixOperator { ref op, ref rhs } => {
                let rhs_type = rhs.expr_type()?;

                let invalid_op_err = ||
                    Err(SemanticError::invalid_operator(op.clone(),
                                                        vec![rhs_type.clone()],
                                                        self.context.clone()));
                match op {
                    &operators::Deref => match &rhs_type {
                        &Some(DeclaredType::Pointer(ref pointed_to)) =>
                            Ok(Some(pointed_to.as_ref().clone())),
                        _ =>
                            invalid_op_err(),
                    },

                    &operators::AddressOf => match &rhs_type {
                        &Some(ref t) if t.addressable() =>
                            Ok(Some(t.clone().pointer())),

                        _ => invalid_op_err()
                    }

                    &operators::Plus |
                    &operators::Minus => match &rhs_type {
                        &Some(DeclaredType::Integer) |
                        &Some(DeclaredType::Byte) =>
                            Ok(rhs_type.clone()),
                        _ =>
                            invalid_op_err(),
                    }

                    &operators::Assignment |
                    &operators::Equals |
                    &operators::NotEquals =>
                        invalid_op_err(),
                }
            }
            &node::ExpressionValue::LiteralString(_) =>
                Ok(Some(DeclaredType::String)),
            &node::ExpressionValue::LiteralInteger(_) =>
                Ok(Some(DeclaredType::Integer)),

            &node::ExpressionValue::FunctionCall { ref target, ref args } => {
                let target_type = target.expr_type()?;
                if let Some(DeclaredType::Function(ref sig)) = target_type {
                    if args.len() != sig.arg_types.len() {
                        Err(SemanticError::wrong_num_args(sig.as_ref().clone(),
                                                          args.len(),
                                                          self.context.clone()))
                    } else {
                        for (arg_index, arg_expr) in args.iter().enumerate() {
                            arg_expr.expr_type()?;

                            let sig_type = sig.arg_types[arg_index].clone();
                            let actual_type = arg_expr.expr_type()?;

                            expect_assignable(Some(&sig_type),
                                              actual_type.as_ref(),
                                              &arg_expr.context)?
                        }

                        Ok(sig.return_type.clone())
                    }
                } else {
                    Err(SemanticError::invalid_function_type(target_type,
                                                             self.context.clone()))
                }
            }
            &node::ExpressionValue::Identifier(ref id) =>
                Ok(Some(id.decl_type().clone())),

            &node::ExpressionValue::Block(ref block) => {
                for statement in block.statements.iter() {
                    statement.expr_type()?;
                }

                Ok(None)
            }

            &node::ExpressionValue::If { ref condition, ref then_branch, ref else_branch } => {
                let condition_type = condition.expr_type()?;

                expect_comparable(Some(&DeclaredType::Boolean),
                                  condition_type.as_ref(),
                                  &self.context)?;

                let _then_type = then_branch.expr_type()?;

                if let &Some(ref else_expr) = else_branch {
                    else_expr.expr_type()?;
                }

                Ok(None)
            }

            &node::ExpressionValue::ForLoop { ref from, ref to, .. } => {
                let from_type = from.expr_type()?;
                let to_type = to.expr_type()?;

                match &from.value {
                    &node::ExpressionValue::BinaryOperator { ref op, ref lhs, .. }
                    if *op == operators::Operator::Assignment => {
                        let lhs_type = lhs.expr_type()?;

                        expect_comparable(Some(&DeclaredType::Integer), lhs_type.as_ref(),
                                       &self.context)?;
                        expect_comparable(Some(&DeclaredType::Integer), to_type.as_ref(),
                                       &self.context)?;

                        Ok(None)
                    }

                    //TODO better error
                    _ => Err(SemanticError::unexpected_type(
                        Some(types::DeclaredType::Integer),
                        from_type,
                        self.context.clone()))
                }
            }

            &node::ExpressionValue::Member { ref of, ref name } => {
                let base_type = of.expr_type()?
                    .map(|dt| dt.remove_indirection().clone());
                match base_type {
                    Some(DeclaredType::Record(ref record)) => {
                        match record.get_member(&name) {
                            Some(member) => Ok(Some(member.decl_type.clone())),
                            None => {
                                let name_id = node::Identifier::from(name.as_str());
                                Err(SemanticError::unknown_symbol(name_id, of.context.clone()))
                            }
                        }
                    }

                    bad @ _ => {
                        Err(SemanticError::member_of_non_record(bad,
                                                                name.clone(),
                                                                of.context.clone()))
                    }
                }
            }
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
    use source;

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
            .with_symbol("x".to_owned(), DeclaredType::String);

        let expr = parse_expr("x := 1", &scope);

        match expr.type_check() {
            Err(SemanticError {
                    kind: SemanticErrorKind::UnexpectedType { expected, actual },
                    ..
                }) => {
                assert_eq!(Some(DeclaredType::String), expected);
                assert_eq!(Some(DeclaredType::Integer), actual);
            }
            _ => panic!("expected invalid types in assignment")
        }
    }
}