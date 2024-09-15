use crate::ast::Expr;
use crate::ast::FunctionParamMod;
use crate::ast::IdentPath;
use crate::ast::TypeAnnotation;
use crate::ast::TypeList;
use crate::parse::LookAheadTokenStream;
use crate::parse::Match;
use crate::parse::MatchOneOf;
use crate::parse::Matcher;
use crate::parse::Parse;
use crate::parse::ParseResult;
use crate::parse::ParseSeq;
use crate::parse::TokenStream;
use crate::DelimiterPair;
use crate::Keyword;
use crate::Operator;
use crate::Separator;
use crate::TokenTree;
use common::span::Span;
use common::span::Spanned;
use std::fmt;
use std::hash::Hash;
use std::hash::Hasher;

#[derive(Clone, Debug, Eq)]
pub struct IdentTypeName {
    pub ident: IdentPath,
    pub type_args: Option<TypeList<TypeName>>,
    pub indirection: usize,
    pub span: Span,
}

impl PartialEq for IdentTypeName {
    fn eq(&self, other: &Self) -> bool {
        self.ident == other.ident
            && self.type_args == other.type_args
            && self.indirection == other.indirection
    }
}

impl Hash for IdentTypeName {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.ident.hash(state);
        self.type_args.hash(state);
        self.indirection.hash(state);
    }
}

impl Spanned for IdentTypeName {
    fn span(&self) -> &Span {
        &self.span
    }
}

impl fmt::Display for IdentTypeName {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for _ in 0..self.indirection {
            write!(f, "^")?;
        }
        write!(f, "{}", self.ident)?;

        if let Some(type_args) = &self.type_args {
            write!(f, "[")?;
            for (i, arg) in type_args.items.iter().enumerate() {
                if i > 0 {
                    write!(f, ", ")?;
                }
                write!(f, "{}", arg)?;
            }
            write!(f, "]")?;
        }

        Ok(())
    }
}

#[derive(Clone, Debug, Eq)]
pub struct ArrayTypeName {
    pub element: Box<TypeName>,
    pub dim: Option<Box<Expr<Span>>>,
    pub indirection: usize,
    pub span: Span,
}

impl PartialEq for ArrayTypeName {
    fn eq(&self, other: &Self) -> bool {
        self.element == other.element
            && self.dim == other.dim
            && self.indirection == other.indirection
    }
}

impl Hash for ArrayTypeName {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.element.hash(state);
        self.dim.hash(state);
        self.indirection.hash(state);
    }
}

impl Spanned for ArrayTypeName {
    fn span(&self) -> &Span {
        &self.span
    }
}

impl fmt::Display for ArrayTypeName {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.dim {
            Some(dim) => write!(f, "array[{}] of {}", dim, self.element),
            None => write!(f, "array of {}", self.element),
        }
    }
}

#[derive(Clone, Debug, Eq)]
pub struct FunctionTypeName {
    pub params: Vec<FunctionTypeNameParam>,
    pub return_ty: Option<Box<TypeName>>,

    pub indirection: usize,

    pub span: Span,
}

impl PartialEq for FunctionTypeName {
    fn eq(&self, other: &Self) -> bool {
        self.params == other.params && self.return_ty == other.return_ty
    }
}

impl Hash for FunctionTypeName {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.params.hash(state);
        self.return_ty.hash(state);
    }
}

impl Spanned for FunctionTypeName {
    fn span(&self) -> &Span {
        &self.span
    }
}

impl fmt::Display for FunctionTypeName {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "function(")?;
        for (i, param_ty) in self.params.iter().enumerate() {
            if i > 0 {
                write!(f, "; ")?;
            }
            write!(f, "{}", param_ty)?;
        }
        write!(f, ")")?;

        if let Some(return_ty) = &self.return_ty {
            write!(f, ": {}", return_ty)?;
        }

        Ok(())
    }
}

#[derive(Clone, Debug, Eq)]
pub struct FunctionTypeNameParam {
    pub ty: TypeName,
    pub modifier: Option<FunctionParamMod>,
}

impl PartialEq for FunctionTypeNameParam {
    fn eq(&self, other: &Self) -> bool {
        self.ty == other.ty && self.modifier == other.modifier
    }
}

impl ParseSeq for FunctionTypeNameParam {
    fn parse_group(prev: &[Self], tokens: &mut TokenStream) -> ParseResult<Self> {
        if !prev.is_empty() {
            tokens.match_one(Separator::Semicolon)?;
        }

        let modifier = match tokens.match_one_maybe(Keyword::Var.or(Keyword::Out)) {
            Some(tt) if tt.is_keyword(Keyword::Var) => Some(FunctionParamMod::Var),
            Some(tt) if tt.is_keyword(Keyword::Out) => Some(FunctionParamMod::Out),
            Some(..) => unreachable!(),
            None => None,
        };

        let ty = TypeName::parse(tokens)?;

        Ok(FunctionTypeNameParam { ty, modifier })
    }

    fn has_more(prev: &[Self], tokens: &mut LookAheadTokenStream) -> bool {
        if !prev.is_empty() && tokens.match_one(Separator::Semicolon).is_none() {
            return false;
        }

        tokens
            .match_one(Keyword::Var.or(Keyword::Out).or(TypeName::start_matcher()))
            .is_some()
    }
}

impl Hash for FunctionTypeNameParam {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.ty.hash(state);
        self.modifier.hash(state);
    }
}

impl fmt::Display for FunctionTypeNameParam {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(modifier) = &self.modifier {
            write!(f, "{} ", modifier)?;
        }
        write!(f, "{}", self.ty)
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum TypeName {
    /// type is unknown or unnamed at parse time
    Unknown(Span),

    Ident(IdentTypeName),
    Array(ArrayTypeName),

    Function(FunctionTypeName),
}

impl Spanned for TypeName {
    fn span(&self) -> &Span {
        match self {
            TypeName::Ident(i) => i.span(),
            TypeName::Array(a) => a.span(),
            TypeName::Unknown(span) => span,
            TypeName::Function(f) => f.span(),
        }
    }
}

impl TypeAnnotation for TypeName {
    fn is_known(&self) -> bool {
        match self {
            TypeName::Unknown(_) => false,
            _ => true,
        }
    }
}

impl Parse for TypeName {
    fn parse(tokens: &mut TokenStream) -> ParseResult<Self> {
        let mut indirection = 0;
        let mut indirection_span = None;

        while let Some(deref_tt) = tokens.match_one_maybe(Operator::Caret) {
            if indirection_span.is_none() {
                indirection_span = Some(deref_tt.span().clone());
            }
            indirection += 1;
        }

        match tokens.match_one_maybe(Keyword::Array.or(Keyword::Function).or(Keyword::Procedure)) {
            None => Self::parse_named_type(tokens, indirection, indirection_span.as_ref()),

            Some(array_kw) if array_kw.is_keyword(Keyword::Array) => {
                Self::parse_array_type(tokens, array_kw.span(), indirection, indirection_span)
            },

            Some(fn_kw)
                if fn_kw.is_keyword(Keyword::Function) || fn_kw.is_keyword(Keyword::Procedure) =>
            {
                Self::parse_function_type(tokens, fn_kw.into_span(), indirection, indirection_span)
            },

            Some(..) => unreachable!(),
        }
    }
}

impl Match for TypeName {
    fn is_match(tokens: &mut LookAheadTokenStream) -> bool {
        tokens.match_one(Self::start_matcher()).is_some()
    }
}

impl TypeName {
    pub fn start_matcher() -> Matcher {
        Keyword::Array
            .or(Keyword::Function)
            .or(Keyword::Procedure)
            .or(Matcher::AnyIdent)
    }

    fn parse_array_type(
        tokens: &mut TokenStream,
        array_kw_span: &Span,
        indirection: usize,
        indirection_span: Option<Span>,
    ) -> ParseResult<Self> {
        // `array of` means the array is dynamic (no dimension)
        let dim = match tokens.look_ahead().match_one(Keyword::Of) {
            Some(_) => None,

            None => match tokens.match_one(Matcher::Delimited(DelimiterPair::SquareBracket))? {
                TokenTree::Delimited(group) => {
                    let mut dim_tokens = group.to_inner_tokens();
                    let dim_expr = Expr::parse(&mut dim_tokens)?;
                    dim_tokens.finish()?;

                    Some(Box::new(dim_expr))
                },

                _ => unreachable!("match failed"),
            },
        };

        tokens.match_one(Keyword::Of)?;

        let element = Self::parse(tokens)?;

        let array_span = array_kw_span.to(element.span());
        let span = match indirection_span {
            Some(indir_span) => indir_span.to(&array_span),
            None => array_span,
        };

        Ok(TypeName::Array(ArrayTypeName {
            dim,
            span,
            indirection,
            element: Box::new(element),
        }))
    }

    fn parse_function_type(
        tokens: &mut TokenStream,
        kw_span: Span,
        indirection: usize,
        indirection_span: Option<Span>,
    ) -> ParseResult<Self> {
        let span_begin = indirection_span.unwrap_or_else(|| kw_span.clone());
        let mut span_end = kw_span;

        let params = match tokens.match_one_maybe(DelimiterPair::Bracket) {
            Some(TokenTree::Delimited(group)) => {
                span_end = group.close.clone();

                let mut params_tokens = group.to_inner_tokens();
                let params = FunctionTypeNameParam::parse_seq(&mut params_tokens)?;
                params_tokens.finish()?;

                params
            },

            Some(..) => unreachable!(),

            None => Vec::new(),
        };

        let return_ty = match tokens.match_one_maybe(Separator::Colon) {
            Some(..) => {
                let return_ty = TypeName::parse(tokens)?;

                span_end = return_ty.span().clone();

                Some(Box::new(return_ty))
            },
            None => None,
        };

        let func_ty_name = FunctionTypeName {
            indirection,
            params,
            span: span_begin.to(&span_end),
            return_ty,
        };

        Ok(TypeName::Function(func_ty_name))
    }

    fn parse_named_type(
        tokens: &mut TokenStream,
        indirection: usize,
        indirection_span: Option<&Span>,
    ) -> ParseResult<Self> {
        let ident = IdentPath::parse(tokens)?;

        let (type_args, name_span) =
            match tokens.look_ahead().match_one(DelimiterPair::SquareBracket) {
                Some(..) => {
                    let type_args = TypeList::parse_type_args(tokens)?;
                    let name_span = ident.span().to(type_args.span());

                    (Some(type_args), name_span)
                },
                None => {
                    let name_span = ident.span().clone();
                    (None, name_span)
                },
            };

        let span = match indirection_span {
            Some(indir_span) => indir_span.to(&name_span),
            None => name_span,
        };

        Ok(TypeName::Ident(IdentTypeName {
            ident,
            indirection,
            type_args,
            span,
        }))
    }
}

impl fmt::Display for TypeName {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TypeName::Ident(ident_type_name) => write!(f, "{}", ident_type_name),
            TypeName::Array(array_type_name) => write!(f, "{}", array_type_name),
            TypeName::Function(func_type_name) => write!(f, "{}", func_type_name),
            TypeName::Unknown(_) => write!(f, "<unknown type>"),
        }
    }
}
