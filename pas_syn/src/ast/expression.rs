use {
    std::fmt,
    crate::{
        IntConstant,
        TokenTree,
        Ident,
        span::*,
        TokenStream,
        Operator,
        Matcher,
        ast::{
            Annotation,
            ParseResult,
        },
    },
};

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct BinOp<A: Annotation> {
    pub lhs: ExpressionNode<A>,
    pub op: Operator,
    pub rhs: ExpressionNode<A>,
}

impl<A: Annotation> fmt::Display for BinOp<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result{
        write!(f, "{} {} {}", self.lhs, self.op, self.rhs)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct ExpressionNode<A: Annotation> {
    pub expr: Box<Expression<A>>,
    pub annotation: A,
}

impl<A: Annotation> ExpressionNode<A> {
    pub fn new(expr: impl Into<Expression<A>>, annotation: impl Into<A>) -> Self {
        Self {
            expr: Box::new(expr.into()),
            annotation: annotation.into(),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Expression<A: Annotation> {
    BinOp(BinOp<A>),
    LiteralInt(IntConstant),
    Ident(Ident),
}

impl ExpressionNode<Span> {
    pub fn parse(tokens: &mut TokenStream) -> ParseResult<Self> {
        let match_expr_start = Matcher::AnyLiteralInteger
            .or(Matcher::AnyIdent);

        match tokens.match_one(match_expr_start.clone())? {
            TokenTree::IntNumber { value, span } => {
                let expr = Expression::LiteralInt(value);
                Ok(ExpressionNode::new(expr, span))
            }

            TokenTree::Ident(ident) => {
                let span = ident.span().clone();
                let expr = Expression::Ident(ident);
                Ok(ExpressionNode::new(expr, span))
            }

            unhandled => unreachable!("{} not covered by matcher {}", unhandled, match_expr_start),
        }
    }
}

impl<A: Annotation> fmt::Display for Expression<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expression::Ident(ident) => write!(f, "{}", ident),
            Expression::LiteralInt(i) => write!(f, "{}", i),
            Expression::BinOp(op) => write!(f, "{}", op),
        }
    }
}

impl<A: Annotation> fmt::Display for ExpressionNode<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.expr)
    }
}