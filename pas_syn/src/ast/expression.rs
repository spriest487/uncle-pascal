mod parse;
#[cfg(test)]
pub(crate) mod test;

pub use self::parse::match_operand_start;
use crate::{
    ast::{
        expression::parse::CompoundExpressionParser, Annotation, BinOp, Block, Call, CaseExpr,
        Cast, CollectionCtor, Exit, IfCond, ObjectCtor, Raise, Typed, UnaryOp,
    },
    consts::*,
    ident::*,
    parse::*,
};
use pas_common::span::*;
use std::fmt;
use std::rc::Rc;
use crate::ast::match_block::MatchExpr;

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum Literal<T: Typed> {
    Nil,
    Integer(IntConstant),
    Real(RealConstant),
    String(Rc<String>),
    Boolean(bool),
    SizeOf(Box<T>),
}

impl<T: Typed> fmt::Display for Literal<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Literal::Nil => write!(f, "nil"),
            Literal::Integer(x) => write!(f, "{}", x),
            Literal::Real(x) => write!(f, "{}", x),
            Literal::String(s) => write!(f, "'{}'", s),
            Literal::Boolean(b) => write!(f, "{}", b),
            Literal::SizeOf(ty) => write!(f, "sizeof({})", ty),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Expression<A: Annotation> {
    BinOp(Box<BinOp<A>>),
    UnaryOp(Box<UnaryOp<A>>),
    Literal(Literal<A::Type>, A),
    Ident(Ident, A),
    Call(Box<Call<A>>),
    ObjectCtor(Box<ObjectCtor<A>>),
    CollectionCtor(Box<CollectionCtor<A>>),
    IfCond(Box<IfCond<A, Expression<A>>>),
    Block(Box<Block<A>>),
    Raise(Box<Raise<A>>),
    Exit(Box<Exit<A>>),
    Case(Box<CaseExpr<A>>),
    Match(Box<MatchExpr<A>>),
    Cast(Box<Cast<A>>),
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

impl<A: Annotation> From<IfCond<A, Expression<A>>> for Expression<A> {
    fn from(cond: IfCond<A, Expression<A>>) -> Self {
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

impl<A: Annotation> From<CaseExpr<A>> for Expression<A> {
    fn from(case: CaseExpr<A>) -> Self {
        Expression::Case(Box::new(case))
    }
}

impl<A: Annotation> From<Exit<A>> for Expression<A> {
    fn from(exit: Exit<A>) -> Self {
        Expression::Exit(Box::new(exit))
    }
}

impl<A: Annotation> From<Cast<A>> for Expression<A> {
    fn from(cast: Cast<A>) -> Self {
        Expression::Cast(Box::new(cast))
    }
}

impl<A: Annotation> From<MatchExpr<A>> for Expression<A> {
    fn from(match_expr: MatchExpr<A>) -> Self {
        Expression::Match(Box::new(match_expr))
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
            Expression::Case(case) => &case.annotation,
            Expression::Match(match_expr) => &match_expr.annotation,
            Expression::Exit(exit) => exit.annotation(),
            Expression::Cast(cast) => &cast.annotation,
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
            Expression::Case(case) => &mut case.annotation,
            Expression::Match(match_expr) => &mut match_expr.annotation,
            Expression::Exit(exit) => exit.annotation_mut(),
            Expression::Cast(cast) => &mut cast.annotation,
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

    pub fn as_unary_op(&self) -> Option<&UnaryOp<A>> {
        match self {
            Expression::UnaryOp(unary_op) => Some(unary_op.as_ref()),
            _ => None,
        }
    }

    pub fn as_call(&self) -> Option<&Call<A>> {
        match self {
            Expression::Call(call) => Some(call),
            _ => None,
        }
    }

    pub fn as_literal(&self) -> Option<&Literal<A::Type>> {
        match self {
            Expression::Literal(lit, _) => Some(lit),
            _ => None,
        }
    }
}

impl Parse for Expression<Span> {
    fn parse(tokens: &mut TokenStream) -> ParseResult<Self> {
        Expression::parse(tokens)
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
            Expression::Case(case) => write!(f, "{}", case),
            Expression::Match(match_expr) => write!(f, "{}", match_expr),
            Expression::Exit(exit) => write!(f, "{}", exit),
            Expression::Cast(cast) => write!(f, "{}", cast),
        }
    }
}

impl<A: Annotation> Spanned for Expression<A> {
    fn span(&self) -> &Span {
        self.annotation().span()
    }
}
