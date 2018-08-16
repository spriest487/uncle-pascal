use std::collections::HashMap;
use node;
use syntax;
use semantic::*;
use types::{DeclaredType, FunctionKind};

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
            }.extract_rc_subexprs(&local_scope);

        Ok(block)
    }

    fn subexpr_to_rc_block(subexpr: Expression) -> Expression {
        let mut next_binding = 1;
        let mut bindings = HashMap::new();
        let mut scope = scope.clone();

        node::transform_expressions(lhs, &mut |subexpr| {
            if let Some(Ok(subexpr_type)) = lhs.expr_type() {
                match &subexpr.value {
                    node::ExpressionValue::FunctionCall { target, args } => {
                        if let Ok(Some(DeclaredType::Function(fn_sig))) = target.value.expr_type() {
                            if fn_sig.kind == FunctionKind::Constructor {
                                let name = format!("internal_{}_rc", nest_binding);
                                next_binding += 1;

                                bindings.insert(name, subexpr);
                                scope = scope.with_symbol_local(&name, subexpr_type);

                                let binding_sym = scope.get_symbol(&name).unwrap();
                                Expression::identifier(binding_sym, subexpr.context)
                            } else {
                                subexpr
                            }
                        }
                    }
                }
            }
        })
    }

    /* for every statement that constructs an RC value somewhere as a subexpr, wrap that
    statement in a nested block and extract the constructor call into a separate binding to make
    it easier for backends to manage ref counting */
    fn extract_rc_subexprs(mut self, scope: &Scope) -> Self {
        self.statements = self.statements.into_iter()
            .map(|stmt| {
                match stmt.value {
                    /* recurse into nested blocks */
                    node::ExpressionValue::Block(block) => {
                        Expression::block(block.extract_rc_subexprs())
                    }

                    node::ExpressionValue::BinaryOperator { lhs, rhs, op } => {

                    }

                    _ => stmt,
                }
            })
            .collect();

        self
    }

    fn block_with_rc_subexprs(exprs: impl IntoIterator<Item = Expression>) -> Block {

    }

    pub fn type_check(&self) -> Result<(), SemanticError> {
        self.statements.iter()
            .map(|statement| statement.type_check())
            .collect()
    }
}
