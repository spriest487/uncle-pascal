use std::collections::HashMap;
use node;
use syntax;
use semantic::*;
use types::{DeclaredType, RecordKind};

pub type Block = node::Block<ScopedSymbol>;

impl Block {
    pub fn annotate(block: &syntax::Block, scope: &Scope)
                    -> Result<Self, SemanticError> {
        let mut statements = Vec::new();
        let mut local_scope = scope.clone();

        for src_statement in block.statements.iter() {
            let statement = Expression::annotate(src_statement, &local_scope)?;

            /* after each let binding, the scope for following statements
            includes that let binding */
            if statement.is_let_binding() {
                let (name, value) = statement.clone().unwrap_let_binding();

                let inferred_type = value.expr_type()?
                    .ok_or_else(|| {
                        SemanticError::type_not_assignable(None, statement.context.clone())
                    })?;

                local_scope = local_scope.with_symbol_local(&name, inferred_type);
            }
            statements.push(statement);
        }

        let block =
            Block {
                statements,
                context: block.context.clone(),
            }.extract_rc_subexprs();

        Ok(block)
    }

    fn subexprs_to_rc_block(expr: Expression,
                            bindings: &mut HashMap<String, Expression>,
                            next_binding: &mut usize)
                            -> Expression {
        node::transform_expressions(expr, &mut |subexpr: Expression| {
            //find subexpressions which call a function...
            let called_fn = match subexpr.value.clone() {
                node::ExpressionValue::FunctionCall { target, .. } => target,
                _ => return subexpr,
            };

            //...where the call target is a function (it has to be, but even if it isn't
            // we just fail this silently because this isn't the place we should report
            // that error if it happens)
            let called_fn_sig = match called_fn.expr_type() {
                Ok(Some(DeclaredType::Function(sig))) => sig,
                _ => return subexpr,
            };

            // which returns a rc type
            if let Some(DeclaredType::Record(class_decl)) = called_fn_sig.return_type {
                if class_decl.kind == RecordKind::Class {
                    let name = format!("internal_{}_rc", next_binding);
                    *next_binding += 1;

                    let binding_sym = ScopedSymbol::Local {
                        name: node::Identifier::from(&name),
                        decl_type: DeclaredType::Record(class_decl),
                    };

                    bindings.insert(name, subexpr.clone());
                    Expression::identifier(binding_sym, subexpr.context)
                } else {
                    subexpr
                }
            } else {
                subexpr
            }
        })
    }

    /* for every statement that constructs an RC value somewhere as a subexpr, wrap that
    statement in a nested block and extract the constructor call into a separate binding to make
    it easier for backends to manage ref counting */
    fn extract_rc_subexprs(mut self) -> Self {
        self.statements = self.statements.into_iter()
            .map(|stmt| {
                let context = stmt.context.clone();

                if stmt.is_block() {
                    /* recurse into nested blocks */
                    match stmt.value {
                        node::ExpressionValue::Block(block) =>
                            Expression::block(block.extract_rc_subexprs()),
                        _ =>
                            unreachable!(),
                    }
                } else {
                    let mut next_binding = 1;
                    let mut bindings = HashMap::new();

                    let stmt_extracted = Self::subexprs_to_rc_block(stmt, &mut bindings, &mut next_binding);

                    /* in the case that the expression was just a function call, we're
                        left with a useless identifier on its own, so eliminate that*/
                    let remove_extracted_stmt = match &stmt_extracted.value {
                        node::ExpressionValue::Identifier(ScopedSymbol::Local { name, .. }) =>
                            bindings.contains_key(&name.name),
                        _ => false,
                    };

                    if bindings.len() > 0 {
                        let mut inner_block = Vec::new();
                        for (name, bound_value) in bindings {
                            inner_block.push(Expression::let_binding(context.clone(), &name, bound_value));
                        }

                        if !remove_extracted_stmt {
                            inner_block.push(stmt_extracted);
                        }

                        Expression::block(Block {
                            context,
                            statements: inner_block,
                        })
                    } else {
                        stmt_extracted
                    }
                }
            })
            .collect();

        self
    }

    pub fn type_check(&self) -> Result<(), SemanticError> {
        self.statements.iter()
            .map(|statement| statement.type_check())
            .collect()
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use semantic::expression::test::parse_expr;
    use types::FunctionSignature;
    use node::{Identifier, ExpressionValue, ToSource};
    use operators;

    fn test_scope() -> Scope {
        let scope = Scope::system();

        let string_type = scope.get_type(&Identifier::from("System.String")).unwrap();

        scope.with_symbol_local("HelloWorld", DeclaredType::function(FunctionSignature {
                name: Identifier::from("HelloWorld"),
                return_type: Some(string_type.clone()),
                arg_types: vec![],
            }))
            .with_symbol_local("x", string_type)
    }

    fn parse_block(source: &str) -> Block {
        let src_in_block = format!("begin\n{}\nend", source);

        let scope = test_scope();
        if let ExpressionValue::Block(block) = parse_expr(&src_in_block, &scope).value {
            block
        } else {
            panic!("expected block test expression to parse as a block")
        }
    }

    #[test]
    fn binds_fn_call_returning_rc_into_block() {
        let block = parse_block("HelloWorld()");

        assert_eq!(1, block.statements.len());

        match &block.statements[0].value {
            ExpressionValue::Block(inner_block) => {
                println!("{:?}", inner_block.to_source());
                assert_eq!(1, inner_block.statements.len());

                match &inner_block.statements[0].value {
                    ExpressionValue::LetBinding { name: _name, value } => {
                        assert!(value.is_function_call());
                    }

                    _ => panic!("expression in inner block should be a let binding")
                }
            }

            _ => panic!("HelloWorld() returns a rc type and should have a block for the scope of its return (was: {:?})",
                        block.statements[0].value)
        }
    }

    #[test]
    fn binds_assignment_returning_rc_into_block() {
        let block = parse_block("x := HelloWorld()");

        assert_eq!(1, block.statements.len());

        match &block.statements[0].value {
            ExpressionValue::Block(inner_block) => {
                println!("{:?}", inner_block.to_source());
                assert_eq!(2, inner_block.statements.len());

                match &inner_block.statements[0].value {
                    ExpressionValue::LetBinding { name: _name, value } => {
                        assert!(value.is_function_call());
                    }

                    _ => panic!("1st expression in inner block should be a let binding")
                }

                match &inner_block.statements[1].value {
                    ExpressionValue::BinaryOperator { lhs, op: operators::Assignment, rhs } => {
                        assert!(lhs.is_any_identifier());
                        assert!(rhs.is_any_identifier());
                    }

                    _ => panic!("2nd expression in inner block should be an assignment")
                }
            }

            _ => panic!("HelloWorld() returns a rc type and should have a block for the scope of its return (was: {:?})",
                        block.statements[0].value)
        }
    }
}