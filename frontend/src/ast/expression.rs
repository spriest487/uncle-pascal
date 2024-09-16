mod parse;

#[cfg(test)]
pub(crate) mod test;

use crate::ast::expression::parse::CompoundExpressionParser;
use crate::ast::match_block::MatchExpr;
use crate::ast::Annotation;
use crate::ast::AnonymousFunctionDef;
use crate::ast::BinOp;
use crate::ast::Block;
use crate::ast::Call;
use crate::ast::CaseExpr;
use crate::ast::Cast;
use crate::ast::CollectionCtor;
use crate::ast::Exit;
use crate::ast::IfCond;
use crate::ast::ObjectCtor;
use crate::ast::Raise;
use crate::ast::TypeAnnotation;
use crate::ast::UnaryOp;
use crate::consts::*;
use crate::ast::ident::*;
use crate::parse::*;
use common::span::*;
use std::fmt;
use std::rc::Rc;

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum Literal<T: TypeAnnotation> {
    Nil,
    Integer(IntConstant),
    Real(RealConstant),
    String(Rc<String>),
    Boolean(bool),
    SizeOf(Box<T>),
}

impl<T: TypeAnnotation> fmt::Display for Literal<T> {
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
pub enum Expr<A: Annotation> {
    BinOp(Box<BinOp<A>>),
    UnaryOp(Box<UnaryOp<A>>),
    Literal(Literal<A::Type>, A),
    Ident(Ident, A),
    Call(Box<Call<A>>),
    ObjectCtor(Box<ObjectCtor<A>>),
    CollectionCtor(Box<CollectionCtor<A>>),
    IfCond(Box<IfCond<A, Expr<A>>>),
    Block(Box<Block<A>>),
    Raise(Box<Raise<A>>),
    Exit(Box<Exit<A>>),
    Case(Box<CaseExpr<A>>),
    Match(Box<MatchExpr<A>>),
    Cast(Box<Cast<A>>),
    AnonymousFunction(Box<AnonymousFunctionDef<A>>),
}

impl<A: Annotation + From<Span>> From<Ident> for Expr<A> {
    fn from(ident: Ident) -> Self {
        let annotation = ident.span().clone().into();
        Expr::Ident(ident, annotation)
    }
}

impl<A: Annotation> From<BinOp<A>> for Expr<A> {
    fn from(bin_op: BinOp<A>) -> Self {
        Expr::BinOp(Box::new(bin_op))
    }
}

impl<A: Annotation> From<UnaryOp<A>> for Expr<A> {
    fn from(unary_op: UnaryOp<A>) -> Self {
        Expr::UnaryOp(Box::new(unary_op))
    }
}

impl<A: Annotation> From<Call<A>> for Expr<A> {
    fn from(call: Call<A>) -> Self {
        Expr::Call(Box::new(call))
    }
}

impl<A: Annotation> From<ObjectCtor<A>> for Expr<A> {
    fn from(ctor: ObjectCtor<A>) -> Self {
        Expr::ObjectCtor(Box::new(ctor))
    }
}

impl<A: Annotation> From<CollectionCtor<A>> for Expr<A> {
    fn from(ctor: CollectionCtor<A>) -> Self {
        Expr::CollectionCtor(Box::new(ctor))
    }
}

impl<A: Annotation> From<IfCond<A, Expr<A>>> for Expr<A> {
    fn from(cond: IfCond<A, Expr<A>>) -> Self {
        Expr::IfCond(Box::new(cond))
    }
}

impl<A: Annotation> From<Block<A>> for Expr<A> {
    fn from(block: Block<A>) -> Self {
        Expr::Block(Box::new(block))
    }
}

impl<A: Annotation> From<Raise<A>> for Expr<A> {
    fn from(raise: Raise<A>) -> Self {
        Expr::Raise(Box::new(raise))
    }
}

impl<A: Annotation> From<CaseExpr<A>> for Expr<A> {
    fn from(case: CaseExpr<A>) -> Self {
        Expr::Case(Box::new(case))
    }
}

impl<A: Annotation> From<Exit<A>> for Expr<A> {
    fn from(exit: Exit<A>) -> Self {
        Expr::Exit(Box::new(exit))
    }
}

impl<A: Annotation> From<Cast<A>> for Expr<A> {
    fn from(cast: Cast<A>) -> Self {
        Expr::Cast(Box::new(cast))
    }
}

impl<A: Annotation> From<MatchExpr<A>> for Expr<A> {
    fn from(match_expr: MatchExpr<A>) -> Self {
        Expr::Match(Box::new(match_expr))
    }
}

impl<A: Annotation> From<AnonymousFunctionDef<A>> for Expr<A> {
    fn from(def: AnonymousFunctionDef<A>) -> Self {
        Expr::AnonymousFunction(Box::new(def))
    }
}

impl<A: Annotation> Expr<A> {
    pub fn name(&self) -> &str {
        match self {
            Expr::BinOp(_) => "binary operator",
            Expr::UnaryOp(_) => "unary operator",
            Expr::Literal(_, _) => "literal",
            Expr::Ident(_, _) => "identifier",
            Expr::Call(call) => call.name(),
            Expr::ObjectCtor(_) => "object constructor",
            Expr::CollectionCtor(_) => "collection constructor",
            Expr::IfCond(_) => "if expr",
            Expr::Block(_) => "block expr",
            Expr::Raise(_) => "raise expr",
            Expr::Exit(_) => "exit expr",
            Expr::Case(_) => "case expr",
            Expr::Match(_) => "match expr",
            Expr::Cast(_) => "cast",
            Expr::AnonymousFunction(_) => "anonymous function",
        }
    }
    
    pub fn annotation(&self) -> &A {
        match self {
            Expr::BinOp(bin_op) => &bin_op.annotation,
            Expr::UnaryOp(unary_op) => &unary_op.annotation,
            Expr::Literal(_, annotation) => annotation,
            Expr::Ident(_, annotation) => annotation,
            Expr::Call(call) => &call.annotation(),
            Expr::IfCond(cond) => &cond.annotation,
            Expr::Block(block) => &block.annotation,
            Expr::CollectionCtor(ctor) => &ctor.annotation,
            Expr::ObjectCtor(ctor) => &ctor.annotation,
            Expr::Raise(raise) => &raise.annotation,
            Expr::Case(case) => &case.annotation,
            Expr::Match(match_expr) => &match_expr.annotation,
            Expr::Exit(exit) => exit.annotation(),
            Expr::Cast(cast) => &cast.annotation,
            Expr::AnonymousFunction(def) => &def.annotation,
        }
    }

    pub fn annotation_mut(&mut self) -> &mut A {
        match self {
            Expr::BinOp(bin_op) => &mut bin_op.annotation,
            Expr::UnaryOp(unary_op) => &mut unary_op.annotation,
            Expr::Literal(_, annotation) => annotation,
            Expr::Ident(_, annotation) => annotation,
            Expr::Call(call) => call.annotation_mut(),
            Expr::IfCond(cond) => &mut cond.annotation,
            Expr::Block(block) => &mut block.annotation,
            Expr::CollectionCtor(ctor) => &mut ctor.annotation,
            Expr::ObjectCtor(ctor) => &mut ctor.annotation,
            Expr::Raise(raise) => &mut raise.annotation,
            Expr::Case(case) => &mut case.annotation,
            Expr::Match(match_expr) => &mut match_expr.annotation,
            Expr::Exit(exit) => exit.annotation_mut(),
            Expr::Cast(cast) => &mut cast.annotation,
            Expr::AnonymousFunction(def) => &mut def.annotation,
        }
    }

    pub fn as_ident(&self) -> Option<&Ident> {
        match self {
            Expr::Ident(ident, _) => Some(ident),
            _ => None,
        }
    }

    pub fn into_ident(self) -> Option<Ident> {
        match self {
            Expr::Ident(ident, _) => Some(ident),
            _ => None,
        }
    }

    pub fn as_object_ctor(&self) -> Option<&ObjectCtor<A>> {
        match self {
            Expr::ObjectCtor(ctor) => Some(ctor),
            _ => None,
        }
    }

    pub fn as_bin_op(&self) -> Option<&BinOp<A>> {
        match self {
            Expr::BinOp(bin_op) => Some(bin_op.as_ref()),
            _ => None,
        }
    }

    pub fn as_unary_op(&self) -> Option<&UnaryOp<A>> {
        match self {
            Expr::UnaryOp(unary_op) => Some(unary_op.as_ref()),
            _ => None,
        }
    }

    pub fn as_call(&self) -> Option<&Call<A>> {
        match self {
            Expr::Call(call) => Some(call),
            _ => None,
        }
    }

    pub fn as_literal(&self) -> Option<&Literal<A::Type>> {
        match self {
            Expr::Literal(lit, _) => Some(lit),
            _ => None,
        }
    }
}

impl Parse for Expr<Span> {
    fn parse(tokens: &mut TokenStream) -> ParseResult<Self> {
        Expr::parse(tokens)
    }
}

impl Expr<Span> {
    pub fn parse(tokens: &mut TokenStream) -> ParseResult<Self> {
        let parser = CompoundExpressionParser::new(tokens);
        parser.parse()
    }
}

impl<A: Annotation> fmt::Display for Expr<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expr::Ident(ident, _) => write!(f, "{}", ident),
            Expr::Literal(lit, _) => write!(f, "{}", lit),
            Expr::BinOp(op) => write!(f, "{}", op),
            Expr::Call(call) => write!(f, "{}", call),
            Expr::ObjectCtor(ctor) => write!(f, "{}", ctor),
            Expr::CollectionCtor(ctor) => write!(f, "{}", ctor),
            Expr::IfCond(if_cond) => write!(f, "{}", if_cond),
            Expr::Block(block) => write!(f, "{}", block),
            Expr::UnaryOp(op) => write!(f, "{}", op),
            Expr::Raise(raise) => write!(f, "{}", raise),
            Expr::Case(case) => write!(f, "{}", case),
            Expr::Match(match_expr) => write!(f, "{}", match_expr),
            Expr::Exit(exit) => write!(f, "{}", exit),
            Expr::Cast(cast) => write!(f, "{}", cast),
            Expr::AnonymousFunction(def) => write!(f, "{}", def),
        }
    }
}

impl<A: Annotation> Spanned for Expr<A> {
    fn span(&self) -> &Span {
        self.annotation().span()
    }
}