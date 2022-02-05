use crate::ast::{Annotation, Expression};
use std::fmt;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Cast<A: Annotation> {
    pub expr: Expression<A>,
    pub ty: A::Type,
    pub annotation: A,
}

impl<A: Annotation> fmt::Display for Cast<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} as {}", self.expr, self.ty)
    }
}
