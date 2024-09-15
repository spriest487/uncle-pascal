use crate::ast::{Annotation, Expr};
use std::fmt;
use common::span::{Span, Spanned};

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Cast<A: Annotation> {
    pub expr: Expr<A>,
    pub ty: A::Type,
    pub annotation: A,
}

impl<A: Annotation> fmt::Display for Cast<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} as {}", self.expr, self.ty)
    }
}

impl<A: Annotation> Spanned for Cast<A> {
    fn span(&self) -> &Span {
        self.annotation.span()
    }
}
