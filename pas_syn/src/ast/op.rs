use crate::{
    ast::ExpressionNode,
    parse::prelude::*,
};

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct UnaryOp<A: Annotation> {
    pub op: Operator,
    pub operand: ExpressionNode<A>,
    pub annotation: A,
}

impl<A: Annotation> fmt::Display for UnaryOp<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // operators are only valid in either prefix or postfix position, never both
        if self.op.is_valid_in_pos(Position::Prefix) {
            write!(f, "{}{}", self.op, self.operand)
        } else {
            write!(f, "{}{}", self.operand, self.op)
        }
    }
}

impl<A: Annotation> Spanned for UnaryOp<A> {
    fn span(&self) -> &Span {
        self.annotation.span()
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct BinOp<A: Annotation> {
    pub lhs: ExpressionNode<A>,
    pub op: Operator,
    pub rhs: ExpressionNode<A>,
    pub annotation: A,
}

impl<A: Annotation> fmt::Display for BinOp<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.op {
            Operator::RangeInclusive | Operator::Member => {
                write!(f, "{}{}{}", self.lhs, self.op, self.rhs)
            },

            _ => write!(f, "{} {} {}", self.lhs, self.op, self.rhs),
        }
    }
}

impl<A: Annotation> Spanned for BinOp<A> {
    fn span(&self) -> &Span {
        self.annotation.span()
    }
}
