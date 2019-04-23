use crate::{
    ast::{
        Annotation,
        ExpressionNode,
    },
    parse::prelude::*,
};
use std::fmt;

#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub struct MethodCall<A: Annotation> {
    pub of_type: A::Type,
    pub self_type: A::Type,

    pub ident: Ident,

    pub args: Vec<ExpressionNode<A>>,
    pub annotation: A,
}

impl<A: Annotation> Spanned for MethodCall<A> {
    fn span(&self) -> &Span {
        self.annotation.span()
    }
}

impl<A: Annotation> fmt::Display for MethodCall<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}.{}(", self.of_type, self.ident)?;
        for (i, arg) in self.args.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", arg)?;
        }
        write!(f, ")")
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub struct FunctionCall<A: Annotation> {
    pub target: ExpressionNode<A>,
    pub args: Vec<ExpressionNode<A>>,

    pub annotation: A,
}

impl<A: Annotation> fmt::Display for FunctionCall<A> {
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

impl<A: Annotation> Spanned for FunctionCall<A> {
    fn span(&self) -> &Span {
        self.annotation.span()
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub enum Call<A: Annotation> {
    Function(FunctionCall<A>),
    Method(MethodCall<A>),
}

impl<A: Annotation> fmt::Display for Call<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Call::Function(func_call) => write!(f, "{}", func_call),
            Call::Method(method_call) => write!(f, "{}", method_call),
        }
    }
}

impl<A: Annotation> Spanned for Call<A> {
    fn span(&self) -> &Span {
        match self {
            Call::Function(call) => call.span(),
            Call::Method(call) => call.span(),
        }
    }
}

impl<A: Annotation> Call<A> {
    pub fn annotation(&self) -> &A {
        match self {
            Call::Function(call) => &call.annotation,
            Call::Method(call) => &call.annotation,
        }
    }
}

impl Call<Span> {
    pub fn parse_arg_list(tokens: &mut TokenStream) -> ParseResult<Vec<ExpressionNode<Span>>> {
        let brackets = tokens.match_one(Matcher::Delimited(DelimiterPair::Bracket))?;
        match brackets {
            TokenTree::Delimited {
                delim: DelimiterPair::Bracket,
                inner,
                span,
                ..
            } => {
                let mut args_tokens = TokenStream::new(inner, span);

                let args = args_tokens.match_separated(Separator::Comma, |_, tokens| {
                    let arg_expr = ExpressionNode::parse(tokens)?;
                    Ok(Generate::Yield(arg_expr))
                })?;

                args_tokens.finish()?;

                Ok(args)
            },

            _ => unreachable!(),
        }
    }
}
