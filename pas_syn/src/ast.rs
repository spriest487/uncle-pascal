pub mod block;
pub mod call;
mod case;
mod match_block;
pub mod cond;
pub mod ctor;
pub mod expression;
pub mod function;
pub mod iter;
pub mod op;
pub mod raise;
pub mod statement;
pub mod type_constraint;
pub mod typedecl;
pub mod unit;
pub mod cast;

pub use self::{
    block::*,
    call::*,
    case::{CaseBranch, CaseExpr, CaseStatement, CaseBlock},
    match_block::*,
    cond::*,
    ctor::*,
    expression::*,
    function::*,
    iter::*,
    op::*,
    raise::*,
    statement::*,
    type_constraint::*,
    typedecl::*,
    unit::*,
    cast::Cast,
};
use crate::parse::prelude::*;
use pas_common::TracedError;
use std::hash::Hasher;
use std::{fmt, hash::Hash};
use derivative::*;

pub trait Typed: fmt::Debug + fmt::Display + Clone + PartialEq + Eq + Hash {
    fn is_known(&self) -> bool;
}

pub trait DeclNamed: fmt::Debug + fmt::Display + Clone + PartialEq + Eq + Hash {
    fn as_local(&self) -> &TypeDeclName;
    fn decl_ty_params(&self) -> &[Ident];
}

pub trait Annotation: Spanned + Clone + PartialEq + Eq + Hash {
    type Type: Typed;
    type Name: DeclNamed;
    type Pattern: fmt::Debug + fmt::Display + Clone + PartialEq + Eq + Hash;

    type ConstStringExpr: fmt::Debug + fmt::Display + Clone + PartialEq + Eq + Hash;
    type ConstIntegerExpr: fmt::Debug + fmt::Display + Clone + PartialEq + Eq + Hash;

    type ConstExpr: fmt::Debug + fmt::Display + Clone + PartialEq + Eq + Hash;
}

impl Annotation for Span {
    type Type = TypeName;
    type Name = TypeDeclName;
    type Pattern = TypeNamePattern;

    type ConstStringExpr = Box<Expression<Span>>;
    type ConstIntegerExpr = Box<Expression<Span>>;
    type ConstExpr = Box<Expression<Span>>;
}

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
    pub dim: Option<Box<Expression<Span>>>,
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
            write!(f, ": {}",return_ty)?;
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

        Ok(FunctionTypeNameParam {
            ty,
            modifier,
        })
    }

    fn has_more(prev: &[Self], tokens: &mut LookAheadTokenStream) -> bool {
        if !prev.is_empty() && tokens.match_one(Separator::Semicolon).is_none() {
            return false;
        }

        tokens.match_one(
            Keyword::Var
                .or(Keyword::Out)
                .or(TypeName::start_matcher())
        ).is_some()
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

impl Typed for TypeName {
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

            Some(array_kw) if array_kw.is_keyword(Keyword::Array) => Self::parse_array_type(
                tokens,
                array_kw.span(),
                indirection,
                indirection_span,
            ),

            Some(fn_kw) if fn_kw.is_keyword(Keyword::Function) || fn_kw.is_keyword(Keyword::Procedure) => Self::parse_function_type(
                tokens,
                fn_kw.into_span(),
                indirection,
                indirection_span,
            ),

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
                    let dim_expr = Expression::parse(&mut dim_tokens)?;
                    dim_tokens.finish()?;

                    Some(Box::new(dim_expr))
                }

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
                }
                None => {
                    let name_span = ident.span().clone();
                    (None, name_span)
                }
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

/// Delimited list of types used for declaring and using generics
/// e.g. the part in square brackets of the following:
///
/// * `let y := MakeANewBox[Integer](123);`
/// * `let x: Box[Integer] := y;`
///
/// Generic because items may be type names (when they refer to real types in expressions)
/// or idents only (when they are declaring type parameter names in type/function declarations)
#[derive(Clone, Eq, Derivative)]
#[derivative(PartialEq, Hash, Debug)]
pub struct TypeList<Item> {
    pub items: Vec<Item>,

    #[derivative(Debug = "ignore")]
    #[derivative(PartialEq = "ignore")]
    #[derivative(Hash = "ignore")]
    span: Span,
}

impl<Item> TypeList<Item> {
    pub fn len(&self) -> usize {
        self.items.len()
    }

    pub fn new(items: impl IntoIterator<Item = Item>, span: Span) -> Self {
        let items: Vec<_> = items.into_iter().collect();
        if items.len() == 0 {
            panic!("can't construct an empty type list (@ {})", span);
        }

        Self { items, span }
    }

    pub fn iter(&self) -> impl Iterator<Item = &Item> {
        self.items.iter()
    }
}

impl<Item> Spanned for TypeList<Item> {
    fn span(&self) -> &Span {
        &self.span
    }
}

impl<Item> fmt::Display for TypeList<Item>
where
    Item: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[")?;

        for (i, item) in self.items.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", item)?;
        }

        write!(f, "]")?;
        Ok(())
    }
}

impl TypeList<TypeName> {
    fn parse_type_args(tokens: &mut TokenStream) -> ParseResult<Self> {
        let type_args = Self::parse(tokens)?;

        if type_args.items.len() == 0 {
            let err = ParseError::EmptyTypeArgList(type_args);
            return Err(TracedError::trace(err));
        }

        Ok(type_args)
    }
}

impl TypeList<Ident> {
    fn parse_type_params(tokens: &mut TokenStream) -> ParseResult<Self> {
        let type_args = Self::parse(tokens)?;

        if type_args.items.len() == 0 {
            let err = ParseError::EmptyTypeParamList(type_args);
            return Err(TracedError::trace(err));
        }

        Ok(type_args)
    }
}

impl<Item> TypeList<Item> {
    fn parse(tokens: &mut TokenStream) -> ParseResult<Self>
    where
        Item: Spanned + Parse + Match,
    {
        let (span, mut items_tokens) = match tokens.match_one(DelimiterPair::SquareBracket)? {
            TokenTree::Delimited(group) => {
                (group.span.clone(), group.to_inner_tokens())
            },
            _ => unreachable!(),
        };

        // empty type lists aren't valid, but we parse them first then check that later
        // so accept them for now
        let mut items = Vec::new();
        loop {
            let mut items_tokens_ahead = items_tokens.look_ahead();
            if !items.is_empty() && items_tokens_ahead.match_one(Separator::Comma).is_none() {
                break;
            }

            if !Item::is_match(&mut items_tokens_ahead) {
                break;
            }

            if !items.is_empty() {
                items_tokens.match_one(Separator::Comma)?;
            }

            let item = Item::parse(&mut items_tokens)?;
            items.push(item);
        }

        // allow redundant comma after final item
        items_tokens.match_one_maybe(Separator::Comma);
        items_tokens.finish()?;

        Ok(TypeList { items, span })
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum TypeNamePatternKind {
    Is,
    IsWithBinding(Ident),
    IsNot,
}

impl TypeNamePatternKind {
    pub fn binding(&self) -> Option<&Ident> {
        match self {
            TypeNamePatternKind::IsWithBinding(binding) => Some(binding),
            _ => None,
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum TypeNamePattern {
    TypeName {
        name: IdentPath,
        kind: TypeNamePatternKind,
        span: Span,
    },
    ExactType {
        name: TypeName,
        kind: TypeNamePatternKind,
        span: Span,
    },
}

impl TypeNamePattern {
    pub fn kind(&self) -> &TypeNamePatternKind {
        match self {
            TypeNamePattern::ExactType { kind, .. } => kind,
            TypeNamePattern::TypeName { kind, .. } => kind,
        }
    }

    pub fn parse(tokens: &mut TokenStream) -> ParseResult<Self> {
        let not_kw = tokens.match_one_maybe(Operator::Not);
        let name = TypeName::parse(tokens)?;

        let pattern_path = match &name {
            TypeName::Ident(IdentTypeName {
                ident,
                type_args,
                indirection,
                ..
            }) => {
                if ident.as_slice().len() >= 2 && type_args.is_none() && *indirection == 0 {
                    Some(ident)
                } else {
                    None
                }
            }
            _ => None,
        };

        let binding = if not_kw.is_none() {
            tokens
                .match_one_maybe(Matcher::AnyIdent)
                .and_then(TokenTree::into_ident)
        } else {
            None
        };

        let span = match (&not_kw, &binding) {
            (Some(not_kw), None) => not_kw.span().to(name.span()),
            (None, Some(binding)) => name.span().to(binding.span()),
            _ => name.span().clone(),
        };

        let kind = match binding {
            Some(binding) => {
                assert!(not_kw.is_none());
                TypeNamePatternKind::IsWithBinding(binding)
            }
            None if not_kw.is_some() => TypeNamePatternKind::IsNot,
            None => TypeNamePatternKind::Is,
        };

        match pattern_path {
            Some(pattern_path) => Ok(TypeNamePattern::TypeName {
                name: pattern_path.clone(),
                span,
                kind,
            }),

            None => Ok(TypeNamePattern::ExactType { name, span, kind }),
        }
    }
}

impl fmt::Display for TypeNamePattern {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TypeNamePattern::TypeName { name, kind, .. } => {
                if let TypeNamePatternKind::IsNot = kind {
                    write!(f, "not ")?;
                }
                write!(f, "{}", name)?;
                if let TypeNamePatternKind::IsWithBinding(binding) = kind {
                    write!(f, " {}", binding)?;
                }
                Ok(())
            }

            TypeNamePattern::ExactType { name, kind, .. } => {
                if let TypeNamePatternKind::IsNot = kind {
                    write!(f, "not ")?;
                }
                write!(f, "{}", name)?;
                if let TypeNamePatternKind::IsWithBinding(binding) = kind {
                    write!(f, " {}", binding)?;
                }
                Ok(())
            }
        }
    }
}

impl Spanned for TypeNamePattern {
    fn span(&self) -> &Span {
        match self {
            TypeNamePattern::TypeName { span, .. } => span,
            TypeNamePattern::ExactType { span, .. } => span,
        }
    }
}
