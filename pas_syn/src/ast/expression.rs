mod parse;
#[cfg(test)]
pub(crate) mod test;

pub use self::parse::match_operand_start;
use crate::ast::expression::parse::CompoundExpressionParser;
use crate::ast::SizeOf;
use crate::{
    ast::{Annotation, BinOp, Block, Call, CollectionCtor, IfCond, ObjectCtor, Raise, UnaryOp},
    consts::*,
    ident::*,
    parse::*,
};
use pas_common::span::*;
use std::fmt;

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum Literal {
    Nil,
    Integer(IntConstant),
    Real(RealConstant),
    String(String),
    Boolean(bool),
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Literal::Nil => write!(f, "nil"),
            Literal::Integer(x) => write!(f, "{}", x),
            Literal::Real(x) => write!(f, "{}", x),
            Literal::String(s) => write!(f, "'{}'", s),
            Literal::Boolean(b) => write!(f, "{}", b),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Expression<A: Annotation> {
    BinOp(Box<BinOp<A>>),
    UnaryOp(Box<UnaryOp<A>>),
    Literal(Literal, A),
    Ident(Ident, A),
    Call(Box<Call<A>>),
    ObjectCtor(Box<ObjectCtor<A>>),
    CollectionCtor(Box<CollectionCtor<A>>),
    IfCond(Box<IfCond<A>>),
    Block(Box<Block<A>>),
    Raise(Box<Raise<A>>),
    SizeOf(Box<SizeOf<A>>),
}

impl<A: Annotation + From<Span>> From<Ident> for Expression<A> {
    fn from(ident: Ident) -> Self {
        let annotation = ident.span().clone().into();
        Expression::Ident(ident, annotation)
    }
}

impl<A: Annotation> From<BinOp<A>> for Expression<A> {
    fn from(bin_op: BinOp<A>) -> Self {
        Expression::BinOp(Box::new(bin_op))
    }
}

impl<A: Annotation> From<UnaryOp<A>> for Expression<A> {
    fn from(unary_op: UnaryOp<A>) -> Self {
        Expression::UnaryOp(Box::new(unary_op))
    }
}

impl<A: Annotation> From<Call<A>> for Expression<A> {
    fn from(call: Call<A>) -> Self {
        Expression::Call(Box::new(call))
    }
}

impl<A: Annotation> From<ObjectCtor<A>> for Expression<A> {
    fn from(ctor: ObjectCtor<A>) -> Self {
        Expression::ObjectCtor(Box::new(ctor))
    }
}

impl<A: Annotation> From<CollectionCtor<A>> for Expression<A> {
    fn from(ctor: CollectionCtor<A>) -> Self {
        Expression::CollectionCtor(Box::new(ctor))
    }
}

impl<A: Annotation> From<IfCond<A>> for Expression<A> {
    fn from(cond: IfCond<A>) -> Self {
        Expression::IfCond(Box::new(cond))
    }
}

impl<A: Annotation> From<Block<A>> for Expression<A> {
    fn from(block: Block<A>) -> Self {
        Expression::Block(Box::new(block))
    }
}

impl<A: Annotation> From<Raise<A>> for Expression<A> {
    fn from(raise: Raise<A>) -> Self {
        Expression::Raise(Box::new(raise))
    }
}

impl<A: Annotation> From<SizeOf<A>> for Expression<A> {
    fn from(size_of: SizeOf<A>) -> Self {
        Expression::SizeOf(Box::new(size_of))
    }
}

impl<A: Annotation> Expression<A> {
    pub fn annotation(&self) -> &A {
        match self {
            Expression::BinOp(bin_op) => &bin_op.annotation,
            Expression::UnaryOp(unary_op) => &unary_op.annotation,
            Expression::Literal(_, annotation) => annotation,
            Expression::Ident(_, annotation) => annotation,
            Expression::Call(call) => &call.annotation(),
            Expression::IfCond(cond) => &cond.annotation,
            Expression::Block(block) => &block.annotation,
            Expression::CollectionCtor(ctor) => &ctor.annotation,
            Expression::ObjectCtor(ctor) => &ctor.annotation,
            Expression::Raise(raise) => &raise.annotation,
            Expression::SizeOf(size_of) => &size_of.annotation,
        }
    }

    pub fn annotation_mut(&mut self) -> &mut A {
        match self {
            Expression::BinOp(bin_op) => &mut bin_op.annotation,
            Expression::UnaryOp(unary_op) => &mut unary_op.annotation,
            Expression::Literal(_, annotation) => annotation,
            Expression::Ident(_, annotation) => annotation,
            Expression::Call(call) => call.annotation_mut(),
            Expression::IfCond(cond) => &mut cond.annotation,
            Expression::Block(block) => &mut block.annotation,
            Expression::CollectionCtor(ctor) => &mut ctor.annotation,
            Expression::ObjectCtor(ctor) => &mut ctor.annotation,
            Expression::Raise(raise) => &mut raise.annotation,
            Expression::SizeOf(size_of) => &mut size_of.annotation,
        }
    }

    pub fn as_ident(&self) -> Option<&Ident> {
        match self {
            Expression::Ident(ident, _) => Some(ident),
            _ => None,
        }
    }

    pub fn into_ident(self) -> Option<Ident> {
        match self {
            Expression::Ident(ident, _) => Some(ident),
            _ => None,
        }
    }

    pub fn as_object_ctor(&self) -> Option<&ObjectCtor<A>> {
        match self {
            Expression::ObjectCtor(ctor) => Some(ctor),
            _ => None,
        }
    }

    pub fn as_bin_op(&self) -> Option<&BinOp<A>> {
        match self {
            Expression::BinOp(bin_op) => Some(bin_op.as_ref()),
            _ => None,
        }
    }

    pub fn as_call(&self) -> Option<&Call<A>> {
        match self {
            Expression::Call(call) => Some(call),
            _ => None,
        }
    }

    pub fn as_literal(&self) -> Option<&Literal> {
        match self {
            Expression::Literal(lit, _) => Some(lit),
            _ => None,
        }
    }
}

impl Expression<Span> {
    pub fn parse(tokens: &mut TokenStream) -> ParseResult<Self> {
        let parser = CompoundExpressionParser::new(tokens);
        parser.parse()
    }
}

impl<A: Annotation> fmt::Display for Expression<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expression::Ident(ident, _) => write!(f, "{}", ident),
            Expression::Literal(lit, _) => write!(f, "{}", lit),
            Expression::BinOp(op) => write!(f, "{}", op),
            Expression::Call(call) => write!(f, "{}", call),
            Expression::ObjectCtor(ctor) => write!(f, "{}", ctor),
            Expression::CollectionCtor(ctor) => write!(f, "{}", ctor),
            Expression::IfCond(if_cond) => write!(f, "{}", if_cond),
            Expression::Block(block) => write!(f, "{}", block),
            Expression::UnaryOp(op) => write!(f, "{}", op),
            Expression::Raise(raise) => write!(f, "{}", raise),
            Expression::SizeOf(size_of) => write!(f, "{}", size_of),
        }
    }
}

impl<A: Annotation> Spanned for Expression<A> {
    fn span(&self) -> &Span {
        self.annotation().span()
    }
}
