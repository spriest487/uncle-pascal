use {
    std::fmt,
    crate::{
        ast::{
            Annotation,
            ExpressionNode,
        },
        parse::*,
        token_tree::*,
        span::*,
    },
};

#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub struct Call<A: Annotation> {
    pub target: ExpressionNode<A>,
    pub args: Vec<ExpressionNode<A>>,
    pub annotation: A,
}

impl<A: Annotation> fmt::Display for Call<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}(", self.target)?;
        for (i, arg) in self.args.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
                ;
            }
            write!(f, "{}", arg)?;
        }
        write!(f, ")")
    }
}

impl Call<Span> {
    pub fn parse_arg_list(tokens: &mut TokenStream) -> ParseResult<Vec<ExpressionNode<Span>>> {
        let brackets = tokens.match_one(Matcher::Delimited(DelimiterPair::Bracket))?;
        match brackets {
            TokenTree::Delimited { delim: DelimiterPair::Bracket, inner, span, .. } => {
                let mut args_tokens = TokenStream::new(inner, span);

                let args = args_tokens.match_separated(Separator::Comma, |_, tokens| {
                    let arg_expr = ExpressionNode::parse(tokens)?;
                    Ok(Generate::Yield(arg_expr))
                })?;

                args_tokens.finish()?;

                Ok(args)
            }

            _ => unreachable!(),
        }
    }
}
