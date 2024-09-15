use crate::{
    ast::{Annotation, Expr, LocalBinding, Stmt},
    parse::{Parse, ParseError, ParseResult, TokenStream},
    Keyword,
};
use pas_common::{
    span::{Span, Spanned},
    TracedError,
};
use std::fmt;

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum ForLoopInit<A: Annotation> {
    Binding(Box<LocalBinding<A>>),
    Assignment {
        counter: Box<Expr<A>>,
        value: Box<Expr<A>>,
    },
}

impl<A: Annotation> fmt::Display for ForLoopInit<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ForLoopInit::Binding(b) => write!(f, "{}", b),
            ForLoopInit::Assignment { counter, value } => write!(f, "{} := {}", counter, value),
        }
    }
}

impl<A: Annotation> Spanned for ForLoopInit<A> {
    fn span(&self) -> &Span {
        match self {
            ForLoopInit::Binding(b) => b.span(),
            ForLoopInit::Assignment { counter, .. } => counter.span(),
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct ForLoop<A: Annotation> {
    pub init: ForLoopInit<A>,
    pub to_expr: Expr<A>,
    pub body: Box<Stmt<A>>,
    pub annotation: A,
}

impl<A: Annotation> fmt::Display for ForLoop<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "for {} to {} do {}", self.init, self.to_expr, self.body)
    }
}

impl<A: Annotation> Spanned for ForLoop<A> {
    fn span(&self) -> &Span {
        self.annotation.span()
    }
}

impl ForLoop<Span> {
    pub fn parse(tokens: &mut TokenStream) -> ParseResult<Self> {
        let for_kw = tokens.match_one(Keyword::For)?;

        let init = match Stmt::parse(tokens)? {
            Stmt::LocalBinding(binding) => ForLoopInit::Binding(binding),

            Stmt::Assignment(assignment) => ForLoopInit::Assignment {
                counter: Box::new(assignment.lhs),
                value: Box::new(assignment.rhs),
            },

            stmt => return Err(TracedError::trace(ParseError::InvalidForLoopInit(stmt))),
        };

        tokens.match_one(Keyword::To)?;
        let to_expr = Expr::parse(tokens)?;

        tokens.match_one(Keyword::Do)?;
        let body = Stmt::parse(tokens)?;

        let span = for_kw.span().to(body.annotation().span());

        Ok(Self {
            init,
            to_expr,
            body: Box::new(body),
            annotation: span,
        })
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct WhileLoop<A: Annotation> {
    pub condition: Expr<A>,
    pub body: Box<Stmt<A>>,

    pub annotation: A,
}

impl<A: Annotation> Spanned for WhileLoop<A> {
    fn span(&self) -> &Span {
        self.annotation.span()
    }
}

impl<A: Annotation> fmt::Display for WhileLoop<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "while {} do {}", self.condition, self.body)
    }
}

impl WhileLoop<Span> {
    pub fn parse(tokens: &mut TokenStream) -> ParseResult<Self> {
        let kw = tokens.match_one(Keyword::While)?;

        let condition = Expr::parse(tokens)?;

        let _do = tokens.match_one(Keyword::Do)?;

        let body = Stmt::parse(tokens)?;

        let span = kw.span().to(body.annotation().span());

        Ok(WhileLoop {
            condition,
            body: Box::new(body),
            annotation: span,
        })
    }
}
