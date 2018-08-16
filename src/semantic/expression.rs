use syntax;
use semantic::*;
use node;
use operators;
use types;

pub type Expression = node::Expression<Symbol>;

fn expect_type_eq(expected: DeclaredType, actual: DeclaredType) -> Result<(), SemanticError> {
    if expected != actual {
        Err(SemanticError::UnexpectedType { expected, actual })
    } else {
        Ok(())
    }
}

impl Expression {
    pub fn annotate(expr: &syntax::Expression, scope: &Scope) -> Result<Self, SemanticError> {
        match expr {
            &node::Expression::Identifier(ref name) => {
                scope.get_symbol(name)
                    .map(|symbol| {
                        Expression::identifier(symbol)
                    })
                    .ok_or_else(|| SemanticError::UnknownSymbol(name.clone()))
            }

            &node::Expression::Block(ref block) => {
                Ok(Expression::block(Block::annotate(block, scope)?))
            }

            &node::Expression::LiteralString(ref s) => {
                Ok(Expression::literal_string(s))
            }

            &node::Expression::LiteralInteger(i) => {
                Ok(Expression::literal_int(i))
            }

            &node::Expression::If { ref condition, ref then_branch, ref else_branch } => {
                let cond_expr = Expression::annotate(condition, scope)?;
                let then_expr = Expression::annotate(then_branch, scope)?;
                let else_expr = match else_branch {
                    &None => None,
                    &Some(ref expr) => Some(Expression::annotate(expr.as_ref(), scope)?),
                };

                Ok(Expression::if_then_else(cond_expr, then_expr, else_expr))
            }

            &node::Expression::ForLoop { ref from, ref to, ref body } => {
                let from_expr = Expression::annotate(from, scope)?;
                let to_expr = Expression::annotate(to, scope)?;
                let do_expr = Expression::annotate(body, scope)?;

                Ok(Expression::for_loop(from_expr, to_expr, do_expr))
            }

            &node::Expression::BinaryOperator { ref lhs, ref op, ref rhs } => {
                Ok(Expression::binary_op(
                    Expression::annotate(lhs, scope)?,
                    op.clone(),
                    Expression::annotate(rhs, scope)?,
                ))
            }

            &node::Expression::FunctionCall { ref target, ref args } => {
                let typed_args = args.iter()
                    .map(|arg| Expression::annotate(arg, scope))
                    .collect::<Result<Vec<_>, _>>()?;

                let target_symbol = scope.get_symbol(target)
                    .ok_or_else(|| SemanticError::UnknownSymbol(target.clone()))?;

                Ok(Expression::function_call(target_symbol, typed_args))
            }
        }
    }

    pub fn expr_type(&self) -> DeclaredType {
        match self {
            &node::Expression::BinaryOperator { ref lhs, ref op, .. } => match op {
                &operators::NotEquals |
                &operators::Equals => DeclaredType::Boolean,

                &operators::Assignment |
                &operators::Plus |
                &operators::Minus => lhs.expr_type()
            },
            &node::Expression::LiteralString(_) => DeclaredType::String,
            &node::Expression::LiteralInteger(_) => DeclaredType::Integer,
            &node::Expression::FunctionCall { ref target, .. } => target.decl_type.clone(),
            &node::Expression::Identifier(ref id) => id.decl_type.clone(),

            &node::Expression::Block(_) |
            &node::Expression::ForLoop { .. } |
            &node::Expression::If { .. } => DeclaredType::None,
        }
    }

    pub fn type_check(&self) -> Result<(), SemanticError> {
        match self {
            &node::Expression::BinaryOperator { ref lhs, ref rhs, .. } => {
                lhs.type_check()?;
                rhs.type_check()?;

                let lhs_type = lhs.expr_type();
                let rhs_type = rhs.expr_type();

                expect_type_eq(lhs_type, rhs_type)
            }

            &node::Expression::Identifier(_) |
            &node::Expression::LiteralString(_) |
            &node::Expression::LiteralInteger(_) => Ok(()),

            &node::Expression::FunctionCall { ref target, ref args } => {
                if let DeclaredType::Function(ref sig) = target.decl_type {
                    if args.len() != sig.arg_types.len() {
                        Err(SemanticError::WrongNumberOfArgs {
                            target: target.name.clone(),
                            expected: sig.arg_types.len(),
                            actual: args.len(),
                        })
                    } else {
                        for (arg_index, arg_expr) in args.iter().enumerate() {
                            arg_expr.type_check()?;

                            let sig_type = sig.arg_types[arg_index].clone();
                            let actual_type = arg_expr.expr_type();

                            expect_type_eq(sig_type, actual_type)?;
                        }

                        Ok(())
                    }
                } else {
                    Err(SemanticError::InvalidFunctionType(target.name.clone()))
                }
            }

            &node::Expression::Block(ref block) => {
                for statement in block.statements.iter() {
                    statement.type_check()?;
                }
                Ok(())
            }
            &node::Expression::If { ref condition, ref then_branch, ref else_branch } => {
                expect_type_eq(DeclaredType::Boolean, condition.expr_type())?;

                condition.type_check()?;
                then_branch.type_check()?;

                if let &Some(ref else_expr) = else_branch {
                    else_expr.type_check()?;
                }

                Ok(())
            }

            &node::Expression::ForLoop { ref from, ref to, .. } => {
                match from.as_ref() {
                    &node::Expression::BinaryOperator { ref op, ref lhs, .. }
                    if *op == operators::BinaryOperator::Assignment => {
                        expect_type_eq(DeclaredType::Integer, lhs.expr_type())?;
                        expect_type_eq(DeclaredType::Integer, to.expr_type())?;

                        Ok(())
                    }

                    //TODO better error
                    _ => Err(SemanticError::UnexpectedType {
                        expected: types::DeclaredType::Integer,
                        actual: from.expr_type(),
                    })
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
    };

    fn parse_expr(src: &str, scope: &Scope) -> Expression {
        let tokens = tokenize(src)
            .expect(&format!("test expr `{}` must not contain illegal tokens", src));

        let parsed = syntax::Expression::parse(tokens, &NO_CONTEXT)
            .expect(&format!("test expr `{}` must parse correctly", src));

        Expression::annotate(&parsed, scope)
            .expect(&format!("test expr `{}` must have valid types", src))
    }

    #[test]
    fn assignment_to_wrong_type_is_err() {
        let scope = Scope::default()
            .with_symbol(Symbol::new(node::Identifier::parse("x"), DeclaredType::String));

        let expr = parse_expr("x := 1", &scope);

        match type_check_expr(&expr) {
            Err(SemanticError::UnexpectedType { expected, actual }) => {
                assert_eq!(expected, DeclaredType::String);
                assert_eq!(actual, DeclaredType::Integer);
            }
            _ => panic!("expected invalid types in assignment")
        }
    }
}