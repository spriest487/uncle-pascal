#[cfg(test)]
mod test;

use crate::ast::type_name::TypeName;
use crate::ast::Annotation;
use crate::ast::BindingDeclKind;
use crate::ast::Block;
use crate::ast::DeclMod;
use crate::ast::Expr;
use crate::ast::Ident;
use crate::ast::TypeList;
use crate::ast::TypeParam;
use crate::ast::WhereClause;
use crate::parse::MatchOneOf;
use crate::parse::Matcher;
use crate::parse::Parse;
use crate::parse::ParseError;
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
use common::TracedError;
use derivative::*;
use linked_hash_map::LinkedHashMap;
use std::fmt;

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub enum FunctionParamMod {
    Var,
    Out,
}

impl fmt::Display for FunctionParamMod {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                FunctionParamMod::Var => "var",
                FunctionParamMod::Out => "out",
            }
        )
    }
}

#[derive(Clone, Eq, Derivative)]
#[derivative(Debug, PartialEq, Hash)]
pub struct FunctionParam<A: Annotation = Span> {
    pub ident: Ident,
    pub ty: A::Type,
    pub modifier: Option<FunctionParamMod>,

    #[derivative(Debug = "ignore")]
    #[derivative(PartialEq = "ignore")]
    #[derivative(Hash = "ignore")]
    pub span: Span,
}

impl<A: Annotation> fmt::Display for FunctionParam<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(modifier) = &self.modifier {
            write!(f, "{} ", modifier)?;
        }
        write!(f, "{}: {}", self.ident, self.ty)
    }
}

impl<A: Annotation> Spanned for FunctionParam<A> {
    fn span(&self) -> &Span {
        &self.span
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct ExplicitImpl<A: Annotation = Span> {
    pub for_ty: A::Type,
    pub iface: A::Type,
}

#[derive(Clone, Eq, Derivative)]
#[derivative(PartialEq, Hash, Debug)]
pub struct FunctionDecl<A: Annotation = Span> {
    pub ident: Ident,

    #[derivative(Debug = "ignore")]
    #[derivative(PartialEq = "ignore")]
    #[derivative(Hash = "ignore")]
    pub span: Span,

    pub params: Vec<FunctionParam<A>>,
    pub type_params: Option<TypeList<TypeParam<A::Type>>>,

    pub return_ty: Option<A::Type>,

    pub explicit_impl: Option<ExplicitImpl<A>>,

    pub mods: Vec<DeclMod<A>>,
}

impl FunctionDecl<Span> {
    pub fn parse(tokens: &mut TokenStream) -> ParseResult<Self> {
        let func_kw = tokens.match_one(Keyword::Function.or(Keyword::Procedure))?;

        let func_ident = Ident::parse(tokens)?;

        let mut span = func_kw.span().to(&func_ident);

        let type_params_list = match tokens.look_ahead().match_one(DelimiterPair::SquareBracket) {
            Some(..) => {
                let type_list = TypeList::parse_type_params(tokens)?;
                span = span.to(type_list.span());
                Some(type_list)
            },
            None => None,
        };

        let explicit_iface_ty = match tokens.match_one_maybe(Keyword::Of) {
            Some(..) => {
                let ty = TypeName::parse(tokens)?;
                span = span.to(ty.span());
                Some(ty)
            },
            None => None,
        };

        let params = if let Some(params_tt) = tokens.match_one_maybe(DelimiterPair::Bracket) {
            let params_group = match params_tt {
                TokenTree::Delimited(group) => group,
                _ => unreachable!(),
            };

            span = span.to(&params_group.span);

            let mut params_tokens = params_group.to_inner_tokens();
            Self::parse_params(&mut params_tokens)?
        } else {
            Vec::new()
        };

        let return_ty = match tokens.match_one_maybe(Separator::Colon) {
            Some(_) => {
                // look for a return type
                let ty = TypeName::parse(tokens)?;
                span = span.to(ty.span());
                Some(ty)
            },
            None => None,
        };

        let mods = DeclMod::parse_seq(tokens)?;
        if let Some(last_mod) = mods.last() {
            span = span.to(last_mod.span());
        }

        let where_clause_tt = tokens.look_ahead().match_one(Keyword::Where);
        let type_params = match (type_params_list, where_clause_tt) {
            (Some(type_params_list), Some(..)) => {
                let mut where_clause = WhereClause::parse(tokens)?;
                span = span.to(where_clause.span());

                // match parsed constraints in the where clause to type params in the param list by
                // ident. if a param has no constraint with the same ident, it gets a None constraint instead
                let mut type_params = Vec::new();
                for type_param_ident in &type_params_list.items {
                    let constraint_index = where_clause
                        .constraints
                        .iter()
                        .position(|c| c.param_ident == *type_param_ident);

                    let constraint = match constraint_index {
                        Some(i) => Some(where_clause.constraints.remove(i)),
                        None => None,
                    };

                    type_params.push(TypeParam {
                        name: type_param_ident.clone(),
                        constraint,
                    })
                }

                // any spare type constraints left over? they must not match a type param
                if !where_clause.constraints.is_empty() {
                    let first_bad = where_clause.constraints.remove(0);
                    // just error on the first one
                    let is_duplicate = type_params.iter().any(|p| p.name == first_bad.param_ident);
                    return Err(TracedError::trace(if is_duplicate {
                        ParseError::TypeConstraintAlreadySpecified(first_bad)
                    } else {
                        ParseError::NoMatchingParamForTypeConstraint(first_bad)
                    }));
                }

                assert_eq!(type_params.len(), type_params_list.len());

                Some(TypeList::new(type_params, type_params_list.span().clone()))
            },

            // the function has no type param list so it's an error to write a where clause here
            (None, Some(where_clause_tt)) => {
                let expected = None;
                let err = ParseError::UnexpectedToken(Box::new(where_clause_tt), expected);

                return Err(TracedError::trace(err));
            },

            // the function has a type param list but no where clause, so the type
            // constraints are None for every parameter
            (Some(type_params), None) => {
                let items: Vec<_> = type_params
                    .items
                    .iter()
                    .map(|ident| TypeParam {
                        name: ident.clone(),
                        constraint: None,
                    })
                    .collect();

                Some(TypeList::new(items, type_params.span().clone()))
            },

            // the function has neither a type param list or a where clause
            (None, None) => None,
        };

        let explicit_impl = explicit_iface_ty.map(|ty| {
            // we'll fill this in during typechecking, the parser doesn't care
            let for_ty = TypeName::Unknown(ty.span().clone());
            
            ExplicitImpl { for_ty, iface: ty }
        });

        Ok(FunctionDecl {
            ident: func_ident,
            explicit_impl,
            span,
            return_ty,
            params,
            type_params,
            mods,
        })
    }

    pub fn parse_params(tokens: &mut TokenStream) -> ParseResult<Vec<FunctionParam<Span>>> {
        let mut params = Vec::new();

        let match_mod = Keyword::Var.or(Keyword::Out);
        let match_more = match_mod.clone().or(Matcher::AnyIdent);

        loop {
            if !params.is_empty() && tokens.match_one_maybe(Separator::Semicolon).is_none() {
                break;
            }

            // check if there's another param ahead
            if tokens.look_ahead().match_one(match_more.clone()).is_none() {
                break;
            }

            // might start with a modifier keyword which applies to all params declared in this group
            let modifier = match tokens.match_one_maybe(match_mod.clone()) {
                Some(tt) if tt.is_keyword(Keyword::Var) => Some(FunctionParamMod::Var),
                Some(tt) if tt.is_keyword(Keyword::Out) => Some(FunctionParamMod::Out),
                _ => None,
            };

            // match comma-separated idents for this param type
            let mut idents = Vec::new();
            loop {
                let ident = Ident::parse(tokens)?;
                idents.push(ident);

                if tokens.match_one_maybe(Separator::Comma).is_none() {
                    break;
                }
            }

            tokens.match_one(Separator::Colon)?;
            let ty = TypeName::parse(tokens)?;

            for ident in idents {
                params.push(FunctionParam {
                    span: ident.span.clone(),
                    ty: ty.clone(),
                    ident,
                    modifier,
                })
            }
        }

        Ok(params)
    }
}

impl<A: Annotation> FunctionDecl<A> {
    pub fn external_src(&self) -> Option<&A::ConstStringExpr> {
        self.mods
            .iter()
            .filter_map(|decl_mod| match decl_mod {
                DeclMod::External { src, .. } => Some(src),
                _ => None,
            })
            .next()
    }
}

impl<A: Annotation> Spanned for FunctionDecl<A> {
    fn span(&self) -> &Span {
        &self.span
    }
}

impl<A: Annotation> fmt::Display for FunctionDecl<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "function ")?;

        if let Some(explicit_impl) = &self.explicit_impl {
            write!(f, "{}.", explicit_impl.iface)?;
        }

        write!(f, "{}(", self.ident)?;
        for (i, param) in self.params.iter().enumerate() {
            if i > 0 {
                write!(f, "; ")?;
            }
            write!(f, "{}", param)?;
        }
        write!(f, ")")?;

        if let Some(ty) = &self.return_ty {
            write!(f, ": {}", ty)?;
        }
        Ok(())
    }
}

#[derive(Clone, Eq, Derivative)]
#[derivative(Hash, PartialEq, Debug)]
pub struct FunctionLocalBinding<A>
    where A: Annotation
{
    pub kind: BindingDeclKind,

    pub ident: Ident,
    pub ty: A::Type,

    pub initial_val: Option<A::ConstExpr>,

    #[derivative(Debug = "ignore")]
    #[derivative(PartialEq = "ignore")]
    #[derivative(Hash = "ignore")]
    pub span: Span,
}

impl<A: Annotation> Spanned for FunctionLocalBinding<A> {
    fn span(&self) -> &Span {
        &self.span
    }
}

impl FunctionLocalBinding<Span> {
    // matches the next token of any optional element that wasn't parsed as part of this 
    // otherwise valid decl, e.g. the explicit init of a variable. used for better error messages
    pub fn match_trailing(&self) -> Option<Matcher> {
        match self.initial_val.as_ref() {
            None => Some(Matcher::from(LOCAL_INIT_DECL_OPERATOR)),
            Some(..) => None,
        }
    }
}

// note that function local init is NOT an assignment (:=), we're declaring a value
// that it's already *equal to* at the start of the body
const LOCAL_INIT_DECL_OPERATOR: Operator = Operator::Equals;

#[derive(Clone, Eq, Derivative)]
#[derivative(Hash, PartialEq, Debug)]
pub struct FunctionDef<A: Annotation> {
    pub decl: FunctionDecl<A>,

    pub locals: Vec<FunctionLocalBinding<A>>,

    pub body: Block<A>,

    #[derivative(Debug = "ignore")]
    #[derivative(PartialEq = "ignore")]
    #[derivative(Hash = "ignore")]
    pub span: Span,
}

impl FunctionDef<Span> {
    pub fn parse_body_of_decl(decl: FunctionDecl<Span>, tokens: &mut TokenStream) -> ParseResult<Self> {
        let body_start_matcher = Self::match_body_start();
        
        let mut trailing_semicolon = false;

        let mut locals = Vec::new();
        loop {
            match tokens.look_ahead().match_one(body_start_matcher.clone()) {
                Some(tt) if tt.is_keyword(Keyword::Var) => {
                    tokens.advance(1);
                    locals.extend(
                        Self::parse_locals_block(tokens, BindingDeclKind::Var)?
                    );

                    trailing_semicolon = tokens.match_one_maybe(Separator::Semicolon).is_some();
                },
                Some(tt) if tt.is_keyword(Keyword::Const) => {
                    tokens.advance(1);
                    locals.extend(
                        Self::parse_locals_block(tokens, BindingDeclKind::Const)?
                    );

                    trailing_semicolon = tokens.match_one_maybe(Separator::Semicolon).is_some();
                }

                _ => break,
            }
        }

        let body = Block::parse(tokens)
            .map_err(|err| {
                Self::map_unexpected_err_after_locals(err, trailing_semicolon, &locals)
            })?;

        let span = decl.span.to(body.span());

        Ok(FunctionDef {
            decl,
            locals,
            body,
            span,
        })
    }
    
    fn map_unexpected_err_after_locals(
        err: TracedError<ParseError>,
        trailing_semicolon: bool,
        locals: &[FunctionLocalBinding<Span>]) -> TracedError<ParseError> {       
        err.map(|err| match err {
            ParseError::UnexpectedToken(tt, expected) if !trailing_semicolon => {
                let trailing_match = Self::match_trailing_for_locals(&locals);

                ParseError::UnexpectedToken(tt, match (expected, trailing_match) {
                    (Some(expected), Some(trailing)) => Some(expected.or(trailing)),
                    (Some(expected), None) => Some(expected),
                    (None, Some(trailing)) => Some(trailing),
                    (None, trailing) => trailing,
                })
            }

            _ => err,
        })
    }
    
    fn match_trailing_for_locals(locals: &[FunctionLocalBinding<Span>]) -> Option<Matcher> {
        let last_local = locals.last()?;
        last_local.match_trailing()
    }

    fn parse_locals_block(tokens: &mut TokenStream, kind: BindingDeclKind) -> ParseResult<Vec<FunctionLocalBinding<Span>>> {
        let mut decls = Vec::new();
        loop {
            // separator from previous item
            if !decls.is_empty() {
                tokens.match_one(Separator::Semicolon)?;
            }

            // ident list, at least one ident, separated by commas
            let first_ident = Ident::parse(tokens)?;
            let mut idents = vec![first_ident];
            while tokens.match_one_maybe(Separator::Comma).is_some() {
                let ident = Ident::parse(tokens)?;
                idents.push(ident);
            }

            // if there's a colon following the names, expect an explicit type name to follow
            let ty = match tokens.match_one_maybe(Separator::Colon) {
                None => TypeName::Unknown(Span::of_slice(&idents)),
                Some(..) => TypeName::parse(tokens)?,
            };

            // equals operator indicates there's a default value
            let initial_val = match tokens.match_one_maybe(LOCAL_INIT_DECL_OPERATOR) {
                Some(..) => {
                    let expr = Expr::parse(tokens)?;
                    Some(Box::new(expr))
                },
                None => None,
            };

            decls.extend(idents.into_iter()
                .map(|ident| FunctionLocalBinding {
                    span: ident.span.clone(),
                    kind,
                    ident,
                    ty: ty.clone(),
                    initial_val: initial_val.clone(),
                }));

            let mut look_ahead = tokens.look_ahead();
            if !decls.is_empty() && look_ahead.match_one(Separator::Semicolon).is_none() {
                break;
            }
            if look_ahead.match_one(Matcher::AnyIdent).is_none() {
                break;
            }
        }

        Ok(decls)
    }

    pub fn match_body_start() -> Matcher {
        Keyword::Unsafe
            .or(DelimiterPair::BeginEnd)
            .or(Keyword::Var)
            .or(Keyword::Const)
    }
}

impl<A: Annotation> Spanned for FunctionDef<A> {
    fn span(&self) -> &Span {
        &self.span
    }
}

impl<A: Annotation> fmt::Display for FunctionDef<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "{}", self.decl)?;

        let mut last_local_kind = None;
        for local in &self.locals {
            if Some(local.kind) != last_local_kind {
                write!(f, "{}", local.kind)?;
                last_local_kind = Some(local.kind);
            }

            write!(f, "  {}: {}", local.ident, local.ty)?;
            if let Some(initial_val) = &local.initial_val {
                write!(f, " = {}", initial_val)?;
            }
            writeln!(f, ";")?;
        }

        write!(f, "{}", self.body)
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct AnonymousFunctionDef<A: Annotation> {
    pub annotation: A,

    pub params: Vec<FunctionParam<A>>,
    pub return_ty: Option<A::Type>,

    pub body: Block<A>,
    pub captures: LinkedHashMap<Ident, A::Type>,
}

impl<A: Annotation> fmt::Display for AnonymousFunctionDef<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "function (")?;

        for (i, param) in self.params.iter().enumerate() {
            if i > 0 {
                write!(f, ";")?;
            }
            if let Some(modifier) = &param.modifier {
                write!(f, "{} ", modifier)?;
            }
            write!(f, "{}: {}", param.ident, param.ty)?;
        }

        write!(f, ")")?;
        if let Some(return_ty) = &self.return_ty {
            write!(f, ": {}", return_ty)?;
        }
        writeln!(f, "; ")?;

        write!(f, "{}", self.body)?;
        
        Ok(())
    }
}

impl<A: Annotation> Spanned for AnonymousFunctionDef<A> {
    fn span(&self) -> &Span {
        self.annotation.span()
    }
}

impl Parse for AnonymousFunctionDef<Span> {
    fn parse(tokens: &mut TokenStream) -> ParseResult<Self> {
        let func_kw = tokens.match_one(Keyword::Function.or(Keyword::Procedure).or(Keyword::Lambda))?;

        let function_def = if func_kw.is_keyword(Keyword::Lambda) {
            let mut params = Vec::new();
            while let Some(TokenTree::Ident(ident)) = tokens.match_one_maybe(Matcher::AnyIdent) {
                params.push(FunctionParam {
                    span: ident.span().clone(),
                    ty: TypeName::Unknown(ident.span.clone()),
                    modifier: None,
                    ident,
                });

                if tokens.match_one_maybe(Separator::Comma).is_none() {
                    break;
                }
            }

            tokens.match_one(Separator::Colon)?;

            let body_expr = Expr::parse(tokens)?;
            let body = Block {
                annotation: body_expr.span().clone(),
                begin: body_expr.span().clone(),
                end: body_expr.span().clone(),
                stmts: Vec::new(),
                output: Some(body_expr),
                unsafe_kw: None,
            };

            let span = func_kw.span().to(body.span());

            AnonymousFunctionDef {
                annotation: span,
                body,
                params,
                return_ty: Some(TypeName::Unknown(func_kw.span().clone())),
                captures: Default::default(),
            }
        } else {
            let mut params_tokens = match tokens.match_one(DelimiterPair::Bracket)? {
                TokenTree::Delimited(group) => group.to_inner_tokens(),
                _ => unreachable!(),
            };

            let params = FunctionDecl::parse_params(&mut params_tokens)?;
            params_tokens.finish()?;

            let return_ty = if tokens.match_one_maybe(Separator::Colon).is_some() {
                let ty = TypeName::parse(tokens)?;
                Some(ty)
            } else {
                None
            };

            tokens.match_one(Separator::Semicolon)?;

            let body = Block::parse(tokens)?;

            let span = func_kw.span().to(body.span());

            AnonymousFunctionDef {
                annotation: span,
                body,
                params,
                return_ty,
                captures: Default::default(),
            }
        };

        Ok(function_def)
    }
}
