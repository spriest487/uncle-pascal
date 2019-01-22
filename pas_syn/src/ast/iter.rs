use {
    crate::{
        parse::prelude::*,
        ast::{
            LocalBinding,
            ExpressionNode,
            Statement,
        },
    },
};

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct ForLoop<A: Annotation> {
    pub init_binding: LocalBinding<A>,
    pub to_expr: ExpressionNode<A>,
    pub body: Box<Statement<A>>,
    pub annotation: A,
}

impl<A: Annotation> fmt::Display for ForLoop<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "for {} to {} do {}", self.init_binding, self.to_expr, self.body)
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
        let init_binding = LocalBinding::parse(tokens, false)?;

        tokens.match_one(Keyword::To)?;
        let to_expr  = ExpressionNode::parse(tokens)?;

        tokens.match_one(Keyword::Do)?;
        let body = Statement::parse(tokens)?;

        let span = for_kw.span().to(body.annotation().span());

        Ok(Self {
            init_binding,
            to_expr,
            body: Box::new(body),
            annotation: span,
        })
    }
}