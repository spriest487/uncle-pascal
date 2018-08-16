use syntax;
use semantic::*;
use node;
use operators;
use types;
use source;

pub type Expression = node::Expression<ScopedSymbol>;

fn expect_assignable(target: Option<DeclaredType>,
                     actual: Option<DeclaredType>,
                     context: &source::Token) -> Result<(), SemanticError> {
    match (target, actual) {
        //TODO: this just allows all int->ptr conversions which is bad
        (Some(DeclaredType::Pointer), Some(DeclaredType::Integer)) => Ok(()),

        (ref x, ref y) if x == y => Ok(()),

        (x, y) => Err(SemanticError::unexpected_type(x, y, context.clone()))
    }
}

fn expect_type_eq(expected: Option<DeclaredType>,
                  actual: Option<DeclaredType>,
                  context: source::Token) -> Result<(), SemanticError> {
    if expected != actual {
        Err(SemanticError::unexpected_type(expected, actual, context))
    } else {
        Ok(())
    }
}

impl Expression {
    pub fn annotate(expr: &syntax::Expression, scope: &Scope)
                    -> Result<Self, SemanticError> {
        match &expr.value {
            &node::ExpressionValue::Identifier(ref name) => {
                scope.get_symbol(name)
                    .map(|symbol| {
                        Expression::identifier(symbol, expr.context.clone())
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

            &node::ExpressionValue::FunctionCall { ref target, ref args } => {
                let typed_args = args.iter()
                    .map(|arg| Expression::annotate(arg, scope))
                    .collect::<Result<Vec<_>, _>>()?;

                let target_symbol = scope.get_symbol(target)
                    .ok_or_else(|| SemanticError::unknown_symbol(target.clone(),
                                                                 expr.context.clone()))?;

                Ok(Expression::function_call(target_symbol, typed_args, expr.context.clone()))
            }
        }
    }

    pub fn expr_type(&self) -> Option<DeclaredType> {
        match &self.value {
            &node::ExpressionValue::BinaryOperator { ref lhs, ref op, .. } => match op {
                &operators::NotEquals |
                &operators::Equals => Some(DeclaredType::Boolean),

                &operators::Assignment |
                &operators::Plus |
                &operators::Minus => lhs.expr_type()
            },
            &node::ExpressionValue::LiteralString(_) => Some(DeclaredType::String),
            &node::ExpressionValue::LiteralInteger(_) => Some(DeclaredType::Integer),
            &node::ExpressionValue::FunctionCall { ref target, .. } => {
                match target.decl_type() {
                    types::DeclaredType::Function(sig) => sig.return_type,
                    _ => panic!("function call target ({}) wasn't a function", self),
                }
            },
            &node::ExpressionValue::Identifier(ref id) => Some(id.decl_type().clone()),

            &node::ExpressionValue::Block(_) |
            &node::ExpressionValue::ForLoop { .. } |
            &node::ExpressionValue::If { .. } => None,
        }
    }

    pub fn type_check(&self) -> Result<(), SemanticError> {
        match &self.value {
            &node::ExpressionValue::BinaryOperator { ref lhs, ref rhs, .. } => {
                lhs.type_check()?;
                rhs.type_check()?;

                let lhs_type = lhs.expr_type();
                let rhs_type = rhs.expr_type();

                expect_assignable(lhs_type, rhs_type, &self.context)
            }

            &node::ExpressionValue::Identifier(_) |
            &node::ExpressionValue::LiteralString(_) |
            &node::ExpressionValue::LiteralInteger(_) => Ok(()),

            &node::ExpressionValue::FunctionCall { ref target, ref args } => {
                if let DeclaredType::Function(ref sig) = target.decl_type() {
                    if args.len() != sig.arg_types.len() {
                        Err(SemanticError::wrong_num_args(target.clone(),
                                                          sig.arg_types.len(),
                                                          args.len(),
                                                          self.context.clone()))
                    } else {
                        for (arg_index, arg_expr) in args.iter().enumerate() {
                            arg_expr.type_check()?;

                            let sig_type = sig.arg_types[arg_index].clone();
                            let actual_type = arg_expr.expr_type();

                            expect_type_eq(Some(sig_type), actual_type, arg_expr.context.clone())?;
                        }

                        Ok(())
                    }
                } else {
                    Err(SemanticError::invalid_function_type(target.clone(),
                                                             self.context.clone()))
                }
            }

            &node::ExpressionValue::Block(ref block) => {
                for statement in block.statements.iter() {
                    statement.type_check()?;
                }
                Ok(())
            }

            &node::ExpressionValue::If { ref condition, ref then_branch, ref else_branch } => {
                expect_type_eq(Some(DeclaredType::Boolean),
                               condition.expr_type(),
                               self.context.clone())?;

                condition.type_check()?;
                then_branch.type_check()?;

                if let &Some(ref else_expr) = else_branch {
                    else_expr.type_check()?;
                }

                Ok(())
            }

            &node::ExpressionValue::ForLoop { ref from, ref to, .. } => {
                match &from.value {
                    &node::ExpressionValue::BinaryOperator { ref op, ref lhs, .. }
                    if *op == operators::BinaryOperator::Assignment => {
                        expect_type_eq(Some(DeclaredType::Integer), lhs.expr_type(),
                                       self.context.clone())?;
                        expect_type_eq(Some(DeclaredType::Integer), to.expr_type(),
                                       self.context.clone())?;

                        Ok(())
                    }

                    //TODO better error
                    _ => Err(SemanticError::unexpected_type(
                        Some(types::DeclaredType::Integer),
                        from.expr_type(),
                        self.context.clone()))
                }
            }
        }
    }
}

#[cfg(test)]
mod test {
    use tokenizer::*;
    use semantic::*;
    use syntax;
    use tokens;
    use keywords;

    static NO_CONTEXT: SourceToken = SourceToken {
        token: tokens::Keyword(keywords::Begin),
        line: 0,
        col: 0,
        file: Rc::from(String::from("test")),
    };

    fn parse_expr(src: &str, scope: &Scope) -> Expression {
        let tokens = tokenize("test", src)
            .expect(&format!("test expr `{}` must not contain illegal tokens", src));

        let parsed = syntax::Expression::parse(tokens, &NO_CONTEXT)
            .expect(&format!("test expr `{}` must parse correctly", src));

        Expression::annotate(&parsed, scope)
            .expect(&format!("test expr `{}` must have valid types", src))
    }

    #[test]
    fn assignment_to_wrong_type_is_err() {
        let scope = Scope::default()
            .with_symbol("x".to_owned(), DeclaredType::String);

        let expr = parse_expr("x := 1", &scope);

        match type_check_expr(&expr) {
            Err(SemanticErrorKind::UnexpectedType { expected, actual }) => {
                assert_eq!(expected, DeclaredType::String);
                assert_eq!(actual, DeclaredType::Integer);
            }
            _ => panic!("expected invalid types in assignment")
        }
    }
}