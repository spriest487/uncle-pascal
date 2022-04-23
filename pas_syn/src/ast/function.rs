#[cfg(test)]
mod test;

use crate::{
    ast::{Block, DeclMod, TypeList, TypeParam, WhereClause, Expression},
    parse::prelude::*,
};
use derivative::*;
use linked_hash_map::LinkedHashMap;

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
pub struct FunctionParam<A: Annotation> {
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
pub struct InterfaceImpl<A: Annotation> {
    pub for_ty: A::Type,
    pub iface: A::Type,
}

#[derive(Clone, Eq, Derivative)]
#[derivative(PartialEq, Hash, Debug)]
pub struct FunctionDecl<A: Annotation> {
    pub ident: IdentPath,

    #[derivative(Debug = "ignore")]
    #[derivative(PartialEq = "ignore")]
    #[derivative(Hash = "ignore")]
    pub span: Span,

    pub params: Vec<FunctionParam<A>>,
    pub type_params: Option<TypeList<TypeParam<A::Type>>>,

    pub return_ty: Option<A::Type>,

    pub impl_iface: Option<InterfaceImpl<A>>,

    pub mods: Vec<DeclMod<A>>,
}

impl FunctionDecl<Span> {
    pub fn parse(tokens: &mut TokenStream) -> ParseResult<Self> {
        let func_kw = tokens.match_one(Keyword::Function)?;

        let func_ident = Ident::parse(tokens)?;

        let type_params_list = match tokens.look_ahead().match_one(DelimiterPair::SquareBracket) {
            Some(..) => Some(TypeList::parse_type_params(tokens)?),
            None => None,
        };

        let iface_ty = match tokens.match_one_maybe(Keyword::Of) {
            Some(..) => TypeName::parse(tokens).map(Some)?,
            None => None,
        };

        let params_group = tokens.match_one(DelimiterPair::Bracket)?;
        let params_span = params_group.span().clone();
        let mut params_tokens = match params_group {
            TokenTree::Delimited(group) => group.to_inner_tokens(),
            _ => unreachable!(),
        };

        let params = Self::parse_params(&mut params_tokens)?;

        let return_ty = match tokens.match_one_maybe(Separator::Colon) {
            Some(_) => {
                // look for a return type
                Some(TypeName::parse(tokens)?)
            },
            None => None,
        };

        let mods = DeclMod::parse_seq(tokens)?;

        let where_clause_tt = tokens.look_ahead().match_one(Keyword::Where);
        let type_params = match (type_params_list, where_clause_tt) {
            (Some(type_params_list), Some(..)) => {
                let mut where_clause = WhereClause::parse(tokens)?;

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
                        ident: type_param_ident.clone(),
                        constraint,
                    })
                }

                // any spare type constraints left over? they must not match a type param
                if !where_clause.constraints.is_empty() {
                    let first_bad = where_clause.constraints.remove(0);
                    // just error on the first one
                    let is_duplicate = type_params.iter().any(|p| p.ident == first_bad.param_ident);
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
                        ident: ident.clone(),
                        constraint: None,
                    })
                    .collect();

                Some(TypeList::new(items, type_params.span().clone()))
            },

            // the function has neither a type param list or a where clause
            (None, None) => None,
        };

        let sig_span = match &return_ty {
            Some(return_ty) => func_kw.span().to(return_ty.span()),
            None => func_kw.span().to(&params_span),
        };

        let span = if mods.is_empty() {
            sig_span
        } else {
            sig_span.to(mods[mods.len() - 1].span())
        };

        let impl_iface = iface_ty.map(|ty| {
            // we'll fill this in during typechecking, the parser doesn't care
            let for_ty = TypeName::Unknown(ty.span().clone());

            InterfaceImpl { for_ty, iface: ty }
        });

        Ok(FunctionDecl {
            ident: IdentPath::from(func_ident),
            impl_iface,
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
            .map(|decl_mod| match decl_mod {
                DeclMod::External { src, .. } => src,
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
        if let Some(iface_impl) = &self.impl_iface {
            write!(f, "{}.", iface_impl.iface)?;
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

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub enum FunctionLocalDeclKind {
    Var,
    Const,
}

#[derive(Clone, Eq, Derivative)]
#[derivative(Hash, PartialEq, Debug)]
pub struct FunctionLocalDecl<A>
    where A: Annotation
{
    pub kind: FunctionLocalDeclKind,

    pub ident: Ident,
    pub ty: A::Type,

    pub initial_val: Option<A::ConstExpr>,

    #[derivative(Debug = "ignore")]
    #[derivative(PartialEq = "ignore")]
    #[derivative(Hash = "ignore")]
    pub span: Span,
}

impl<A: Annotation> Spanned for FunctionLocalDecl<A> {
    fn span(&self) -> &Span {
        &self.span
    }
}

#[derive(Clone, Eq, Derivative)]
#[derivative(Hash, PartialEq, Debug)]
pub struct FunctionDef<A: Annotation> {
    pub decl: FunctionDecl<A>,

    pub locals: Vec<FunctionLocalDecl<A>>,

    pub body: Block<A>,

    #[derivative(Debug = "ignore")]
    #[derivative(PartialEq = "ignore")]
    #[derivative(Hash = "ignore")]
    pub span: Span,
}

impl FunctionDef<Span> {
    pub fn parse_body_of_decl(decl: FunctionDecl<Span>, tokens: &mut TokenStream) -> ParseResult<Self> {
        let body_start_matcher = Self::body_start_matcher();

        let mut locals = Vec::new();
        loop {
            match tokens.look_ahead().match_one(body_start_matcher.clone()) {
                Some(tt) if tt.is_keyword(Keyword::Var) => {
                    tokens.advance(1);
                    locals.extend(
                        Self::parse_locals_block(tokens, FunctionLocalDeclKind::Var)?
                    );
                    tokens.match_one_maybe(Separator::Semicolon);
                },
                Some(tt) if tt.is_keyword(Keyword::Const) => {
                    tokens.advance(1);
                    locals.extend(
                        Self::parse_locals_block(tokens, FunctionLocalDeclKind::Const)?
                    );
                    tokens.match_one_maybe(Separator::Semicolon);
                }

                _ => break,
            }
        }

        let body = Block::parse(tokens)?;

        let span = decl.span.to(body.span());

        Ok(FunctionDef {
            decl,
            locals,
            body,
            span,
        })
    }

    fn parse_locals_block(tokens: &mut TokenStream, kind: FunctionLocalDeclKind) -> ParseResult<Vec<FunctionLocalDecl<Span>>> {
        let mut decls = Vec::new();
        loop {
            if !decls.is_empty() {
                tokens.match_one(Separator::Semicolon)?;
            }

            let mut idents = Vec::new();
            loop {
                if !idents.is_empty() && tokens.match_one_maybe(Separator::Comma).is_none() {
                    break;
                }

                let ident = Ident::parse(tokens)?;
                idents.push(ident);
            }

            tokens.match_one(Separator::Colon)?;
            let ty = TypeName::parse(tokens)?;

            let initial_val = match tokens.match_one_maybe(Operator::Equals) {
                Some(..) => {
                    let expr = Expression::parse(tokens)?;
                    Some(Box::new(expr))
                },
                None => None,
            };

            decls.extend(idents.into_iter()
                .map(|ident| FunctionLocalDecl {
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

    pub fn body_start_matcher() -> Matcher {
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
                match local.kind {
                    FunctionLocalDeclKind::Var => writeln!(f, "var")?,
                    FunctionLocalDeclKind::Const => writeln!(f, "const")?,
                };
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
        write!(f, "end")
    }
}

impl<A: Annotation> Spanned for AnonymousFunctionDef<A> {
    fn span(&self) -> &Span {
        self.annotation.span()
    }
}

impl Parse for AnonymousFunctionDef<Span> {
    fn parse(tokens: &mut TokenStream) -> ParseResult<Self> {
        let func_kw = tokens.match_one(Keyword::Function.or(Keyword::Procedure))?;
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

        let body = Block::parse(tokens)?;

        let span = func_kw.span().to(body.span());

        Ok(AnonymousFunctionDef {
            annotation: span,
            body,
            params,
            return_ty,
            captures: Default::default(),
        })
    }
}
