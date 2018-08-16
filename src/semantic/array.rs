use std::rc::Rc;
use syntax;
use node::ConstantExpression;
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
            ConstantExpression::Integer(int) => {
                match int.as_i64() {
                    Some(i) => Ok(i),
                    None => Err(SemanticError::invalid_const_value(expr.clone())),
                }
            }

            _ => Err(SemanticError::invalid_const_value(expr.clone()))
        }
    }

    pub fn annotate(index_range: &syntax::IndexRange, scope: Rc<Scope>) -> SemanticResult<Self> {
        let from = Expression::annotate(&index_range.from, scope.clone())
            .and_then(Self::const_to_array_dim)?;
        let to = Expression::annotate(&index_range.to, scope.clone())
            .and_then(Self::const_to_array_dim)?;

        Ok(IndexRange {
            from,
            to
        })
    }

    pub fn elements(&self) -> u32 {
        assert!(self.to >= self.from, "array upper bound must be >= lower bound");

        self.to.wrapping_sub(self.from) as u32
    }
}