use std::rc::Rc;
use syntax;
use node::ConstExpression;
use types::Type;
use semantic::{
    SemanticResult,
    SemanticError,
    Scope,
    Expression,
};

#[derive(PartialEq, Clone, Debug, Hash, Eq)]
pub struct IndexRange {
    pub from: i64,
    pub to: i64,
}

impl IndexRange {
    fn const_to_array_dim(expr: Expression) -> SemanticResult<i64> {
        match expr.to_const_value()? {
            ConstExpression::Integer(int) => {
                match int.as_i64() {
                    Some(i) => Ok(i),
                    None => Err(SemanticError::invalid_const_value(expr.clone())),
                }
            }

            _ => Err(SemanticError::invalid_const_value(expr.clone()))
        }
    }

    pub fn annotate(index_range: &syntax::IndexRange, scope: Rc<Scope>) -> SemanticResult<Self> {
        let index_type = Some(Type::Int64);

        let from = Expression::annotate(&index_range.from, index_type.as_ref(), scope.clone())
            .and_then(|(expr, _)| Self::const_to_array_dim(expr))?;
        let to = Expression::annotate(&index_range.to, index_type.as_ref(), scope.clone())
            .and_then(|(expr, _)| Self::const_to_array_dim(expr))?;

        Ok(IndexRange {
            from,
            to
        })
    }

    pub fn elements(&self) -> u32 {
        assert!(self.to >= self.from, "array upper bound must be >= lower bound");

        self.to.wrapping_sub(self.from) as u32 + 1
    }
}