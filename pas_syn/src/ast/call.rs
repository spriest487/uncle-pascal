use crate::{ast::{Annotation, Expr, TypeList}, DelimiterPair, Ident, Separator, TokenTree};
use std::{fmt};
use pas_common::span::{Span, Spanned};
use crate::parse::{LookAheadTokenStream, Matcher, ParseResult, ParseSeq, TokenStream};
use crate::token_tree::DelimitedGroup;
use derivative::*;

#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub struct MethodCall<A: Annotation> {
    pub iface_type: A::Type,
    pub self_type: A::Type,

    pub func_type: A::Type,

    pub ident: Ident,

    pub args: Vec<Expr<A>>,
    pub type_args: Option<TypeList<A::Type>>,

    pub annotation: A,

    pub args_span: Span,
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

#[derive(Eq, Clone, Derivative)]
#[derivative(Hash, Debug, PartialEq)]
pub struct FunctionCall<A: Annotation> {
    pub target: Expr<A>,
    pub args: Vec<Expr<A>>,

    pub type_args: Option<TypeList<A::Type>>,

    pub annotation: A,

    #[derivative(Debug = "ignore")]
    #[derivative(Hash = "ignore")]
    #[derivative(PartialEq = "ignore")]
    pub args_span: Span,
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

#[derive(Eq, Clone, Derivative)]
#[derivative(Hash, Debug, PartialEq)]
pub struct FunctionCallNoArgs<A: Annotation> {
    pub target: Expr<A>,

    pub annotation: A,
}

impl<A: Annotation> fmt::Display for FunctionCallNoArgs<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.target)
    }
}

impl<A: Annotation> Spanned for FunctionCallNoArgs<A> {
    fn span(&self) -> &Span {
        self.annotation.span()
    }
}


#[derive(Eq, Clone, Derivative)]
#[derivative(Hash, Debug, PartialEq)]
pub struct MethodCallNoArgs<A: Annotation> {
    pub target: Expr<A>,

    pub self_arg: Expr<A>,

    pub annotation: A,
}

impl<A: Annotation> fmt::Display for MethodCallNoArgs<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.target)
    }
}

impl<A: Annotation> Spanned for MethodCallNoArgs<A> {
    fn span(&self) -> &Span {
        self.annotation.span()
    }
}


#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub struct VariantCtorCall<A: Annotation> {
    pub variant: A::Name,
    pub case: Ident,

    pub arg: Option<Expr<A>>,
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
    // call to a function (method or standalone) without an argument list
    // this needs to be a distinct kind of function because if it appears as the target of a function
    // call, we turn it into a function call with arguments (rather than attempting to call the result value)
    FunctionNoArgs(FunctionCallNoArgs<A>),

    MethodNoArgs(MethodCallNoArgs<A>),

    // call to a standalone function
    Function(FunctionCall<A>),

    // call to an interface method function
    Method(MethodCall<A>),

    // call to a variant constructor
    VariantCtor(VariantCtorCall<A>),
}

impl<A: Annotation> fmt::Display for Call<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Call::Function(call) => write!(f, "{}", call),
            Call::FunctionNoArgs(call) => write!(f, "{}", call),
            Call::Method(call) => write!(f, "{}", call),
            Call::MethodNoArgs(call) => write!(f, "{}", call),
            Call::VariantCtor(call) => write!(f, "{}", call),
        }
    }
}

impl<A: Annotation> Spanned for Call<A> {
    fn span(&self) -> &Span {
        match self {
            Call::FunctionNoArgs(call) => call.span(),
            Call::Function(call) => call.span(),
            Call::Method(call) => call.span(),
            Call::MethodNoArgs(call) => call.span(),
            Call::VariantCtor(call) => call.span(),
        }
    }
}

impl<A: Annotation> Call<A> {
    pub fn annotation(&self) -> &A {
        match self {
            Call::FunctionNoArgs(call) => &call.annotation,
            Call::Function(call) => &call.annotation,
            Call::Method(call) => &call.annotation,
            Call::MethodNoArgs(call) => &call.annotation,
            Call::VariantCtor(call) => &call.annotation,
        }
    }

    pub fn annotation_mut(&mut self) -> &mut A {
        match self {
            Call::FunctionNoArgs(call) => &mut call.annotation,
            Call::Function(call) => &mut call.annotation,
            Call::Method(call) => &mut call.annotation,
            Call::MethodNoArgs(call) => &mut call.annotation,
            Call::VariantCtor(call) => &mut call.annotation,
        }
    }
}

struct ArgListItem(Expr<Span>);

impl ParseSeq for ArgListItem {
    fn parse_group(prev: &[Self], tokens: &mut TokenStream) -> ParseResult<Self> {
        if !prev.is_empty() {
            tokens.match_one(Separator::Comma)?;
        }

        let arg = Expr::parse(tokens)?;
        Ok(ArgListItem(arg))
    }

    fn has_more(prev: &[Self], tokens: &mut LookAheadTokenStream) -> bool {
        if !prev.is_empty() && tokens.match_one(Separator::Comma).is_none() {
            return false;
        }

        tokens.match_one(Matcher::ExprOperandStart).is_some()
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct ArgList<A: Annotation> {
    pub open: Span,
    pub close: Span,
    pub args: Vec<Expr<A>>,
}

impl ArgList<Span> {
    pub fn parse(tokens: &mut TokenStream) -> ParseResult<Self> {
        let brackets = tokens.match_one(Matcher::Delimited(DelimiterPair::Bracket))?;

        let args_group = match brackets {
            TokenTree::Delimited(group @ DelimitedGroup { delim: DelimiterPair::Bracket, .. }) => group,
            _ => unreachable!(),
        };

        let open = args_group.open.clone();
        let close = args_group.close.clone();
        let mut args_tokens = args_group.to_inner_tokens();

        let args = ArgListItem::parse_seq(&mut args_tokens)?
            .into_iter()
            .map(|a| a.0)
            .collect();

        args_tokens.finish()?;

        Ok(ArgList { args, open, close })
    }

    pub fn list_span(&self) -> Span {
        self.open.to(&self.close)
    }
}
