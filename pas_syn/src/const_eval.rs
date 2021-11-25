use pas_common::span::Span;
use crate::ast::{Expression, Literal, TypeName};

pub trait ConstEval {
    fn const_eval(&self) -> Option<Literal<TypeName>>;
}

impl ConstEval for Literal<TypeName> {
    fn const_eval(&self) -> Option<Literal<TypeName>> {
        Some(self.clone())
    }
}

impl ConstEval for Expression<Span> {
    fn const_eval(&self) -> Option<Literal<TypeName>> {
        match self {
            Expression::Literal(lit, ..) => Some(lit.clone()),
            _ => None,
        }
    }
}