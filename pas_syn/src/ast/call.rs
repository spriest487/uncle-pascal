use crate::{
    ast::{Annotation, Expression, TypeList},
    parse::prelude::*,
};
use std::{fmt};

#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub struct MethodCall<A: Annotation> {
    pub iface_type: A::Type,
    pub self_type: A::Type,

    pub func_type: A::Type,

    pub ident: Ident,

    pub args: Vec<Expression<A>>,
    pub type_args: Option<TypeList<A::Type>>,

    pub annotation: A,

    pub args_brackets: (Span, Span),
}

impl<A: Annotation> Spanned for MethodCall<A> {
    fn span(&self) -> &Span {
        self.annotation.span()
    }
}

impl<A: Annotation> fmt::Display for MethodCall<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}.{}(", self.iface_type, self.ident)?;
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
    pub target: Expression<A>,
    pub args: Vec<Expression<A>>,

    pub type_args: Option<TypeList<A::Type>>,

    pub annotation: A,
    pub args_brackets: (Span, Span),
}

impl<A: Annotation> fmt::Display for FunctionCall<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.target)?;

        if let Some(type_args) = self.type_args.as_ref() {
            write!(f, "{}", type_args)?;
        }

        write!(f, "(")?;
        for (i, arg) in self.args.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
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
pub struct VariantCtorCall<A: Annotation> {
    pub variant: A::Name,
    pub case: Ident,

    pub arg: Option<Expression<A>>,
    pub annotation: A,
}

impl<A: Annotation> fmt::Display for VariantCtorCall<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}.{}(", self.variant, self.case)?;
        if let Some(arg) = &self.arg {
            write!(f, "{}", arg)?;
        }
        write!(f, ")")
    }
}

impl<A: Annotation> Spanned for VariantCtorCall<A> {
    fn span(&self) -> &Span {
        self.annotation.span()
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub enum Call<A: Annotation> {
    Function(FunctionCall<A>),
    Method(MethodCall<A>),
    VariantCtor(VariantCtorCall<A>),
}

impl<A: Annotation> fmt::Display for Call<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Call::Function(func_call) => write!(f, "{}", func_call),
            Call::Method(method_call) => write!(f, "{}", method_call),
            Call::VariantCtor(var_ctor_call) => write!(f, "{}", var_ctor_call),
        }
    }
}

impl<A: Annotation> Spanned for Call<A> {
    fn span(&self) -> &Span {
        match self {
            Call::Function(call) => call.span(),
            Call::Method(call) => call.span(),
            Call::VariantCtor(call) => call.span(),
        }
    }
}

impl<A: Annotation> Call<A> {
    pub fn annotation(&self) -> &A {
        match self {
            Call::Function(call) => &call.annotation,
            Call::Method(call) => &call.annotation,
            Call::VariantCtor(call) => &call.annotation,
        }
    }

    pub fn annotation_mut(&mut self) -> &mut A {
        match self {
            Call::Function(call) => &mut call.annotation,
            Call::Method(call) => &mut call.annotation,
            Call::VariantCtor(call) => &mut call.annotation,
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct ArgList<A: Annotation> {
    pub open: Span,
    pub close: Span,
    pub args: Vec<Expression<A>>,
}

impl ArgList<Span> {
    pub fn parse(tokens: &mut TokenStream) -> ParseResult<Self> {
        let brackets = tokens.match_one(Matcher::Delimited(DelimiterPair::Bracket))?;

        let (inner, span, open, close) = match brackets {
            TokenTree::Delimited {
                delim: DelimiterPair::Bracket,
                inner,
                span,
                open,
                close,
            } => (inner, span, open, close),

            _ => unreachable!(),
        };

        let mut args_tokens = TokenStream::new(inner, span);

        let args = args_tokens.match_separated(Separator::Comma, |_, tokens| {
            let arg_expr = Expression::parse(tokens)?;
            Ok(Generate::Yield(arg_expr))
        })?;

        args_tokens.finish()?;

        Ok(ArgList { args, open, close })
    }

    pub fn list_span(&self) -> Span {
        self.open.to(&self.close)
    }
}
