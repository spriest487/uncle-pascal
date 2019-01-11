use {
    std::fmt,
    crate::{
        ast::{
            Annotation,
            Statement,
            ExpressionNode,
            ParseResult,
            statement_start_matcher,
        },
        Generate,
        Separator,
        Span,
        Spanned,
        TokenTree,
        TokenStream,
        DelimiterPair,
    }
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Block<A: Annotation> {
    statements: Vec<Statement<A>>,
    annotation: A,
    output: Option<ExpressionNode<A>>,
}

impl Block<Span> {
    pub fn parse(tokens: &mut TokenStream) -> ParseResult<Self> {
        let body_tt = tokens.match_one(DelimiterPair::BeginEnd)?;
        let span = body_tt.span().clone();

        let mut body_tokens = match body_tt {
            TokenTree::Delimited { inner, open, .. } => {
                TokenStream::new(inner, open)
            }
            _ => unreachable!(),
        };

        let statements = body_tokens.match_separated(Separator::Semicolon, |_, tokens| {
            if tokens.look_ahead().match_one(statement_start_matcher()).is_none() {
                Ok(Generate::Break)
            } else {
                let stmt = Statement::parse(tokens)?;
                Ok(Generate::Yield(stmt))
            }
        })?;

        let output = match body_tokens.look_ahead().next() {
            // there is something that doesn't look like a statement following
            // the body statements, it must be the terminal expression
            Some(_) => Some(ExpressionNode::parse(&mut body_tokens)?),
            None => None,
        };
        body_tokens.finish()?;

        Ok(Self {
            statements,
            annotation: span,

            // we don't know until typechecking
            output,
        })
    }
}

impl<A: Annotation> fmt::Display for Block<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "begin")?;
        for (i, stmt) in self.statements.iter().enumerate() {
            write!(f, "{}", stmt)?;

            // only add a ; on the last statement if there's also an output expr
            if i != self.statements.len() - 1 || self.output.is_some() {
                write!(f, ";")?;
            }
        }
        if let Some(output) = &self.output {
            write!(f, "{}", output)?;
        }
        write!(f, "end")
    }
}

impl<A: Annotation> Spanned for Block<A> {
    fn span(&self) -> &Span {
        self.annotation.span()
    }
}
