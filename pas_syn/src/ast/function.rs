#[cfg(test)]
mod test;

use crate::{
    ast::{Block, DeclMod, TypeList, TypeParam, WhereClause},
    parse::prelude::*,
};

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
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

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct FunctionParam<A: Annotation> {
    pub ident: Ident,
    pub ty: A::Type,
    pub span: Span,
    pub modifier: Option<FunctionParamMod>,
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

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct FunctionDecl<A: Annotation> {
    pub ident: IdentPath,
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

        let args_tt = tokens.match_one(DelimiterPair::Bracket)?;

        let args_span = args_tt.span().clone();

        let mut params_tokens = match args_tt {
            TokenTree::Delimited { inner, open, .. } => TokenStream::new(inner, open),
            _ => unreachable!(),
        };

        let params = params_tokens.match_separated(Separator::Semicolon, |_, tokens| {
            let match_mod = Keyword::Var.or(Keyword::Out);
            let modifier = match tokens.look_ahead().match_one(match_mod.clone()) {
                Some(_) => match tokens.match_one(match_mod)? {
                    TokenTree::Keyword {
                        kw: Keyword::Var, ..
                    } => Some(FunctionParamMod::Var),
                    TokenTree::Keyword {
                        kw: Keyword::Out, ..
                    } => Some(FunctionParamMod::Out),
                    tt => unreachable!("bad token parsing function param modifier: {:?}", tt),
                },
                None => None,
            };

            let idents = tokens.match_separated(Separator::Comma, |_, tokens| {
                let ident = tokens.match_one(Matcher::AnyIdent)?;
                Ok(Generate::Yield(ident))
            })?;

            if idents.is_empty() {
                return Err(TracedError::trace(ParseError::UnexpectedEOF(
                    Matcher::AnyIdent,
                    tokens.context().clone(),
                )));
            }

            tokens.match_one(Separator::Colon)?;
            let ty = TypeName::parse(tokens)?;
            let span = idents[0].span().to(ty.span());

            let params: Vec<_> = idents
                .into_iter()
                .map(|ident| FunctionParam {
                    modifier: modifier.clone(),
                    ident: ident.into_ident().unwrap(),
                    ty: ty.clone(),
                    span: span.clone(),
                })
                .collect();

            Ok(Generate::Yield(params))
        })?;
        params_tokens.finish()?;

        let return_ty = match tokens.match_one_maybe(Separator::Colon) {
            Some(_) => {
                // look for a return type
                Some(TypeName::parse(tokens)?)
            }
            None => None,
        };

        let mods = DeclMod::parse(tokens)?;

        let where_clause_tt = tokens.look_ahead().match_one(Keyword::Where);
        let type_params = match (type_params_list, where_clause_tt) {
            (None, Some(where_clause_tt)) => {
                let expected = None;
                let err = ParseError::UnexpectedToken(Box::new(where_clause_tt), expected);

                return Err(TracedError::trace(err));
            }

            (Some(type_params_list), Some(..)) => {
                let mut where_clause = WhereClause::parse(tokens)?;

                let mut type_params = Vec::new();
                for type_param_ident in &type_params_list.items {
                    let constraint_index = where_clause
                        .constraints
                        .iter()
                        .position(|c| c.param_ident == *type_param_ident);

                    type_params.push(TypeParam {
                        ident: type_param_ident.clone(),
                        constraint: match constraint_index {
                            Some(i) => Some(where_clause.constraints.remove(i)),
                            None => None,
                        },
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

                Some(TypeList::new(
                    type_params,
                    type_params_list.span().clone())
                )
            }

            (Some(type_params), None) => {
                let items: Vec<_> = type_params.items
                    .iter()
                    .map(|ident| TypeParam {
                        ident: ident.clone(),
                        constraint: None,
                    })
                    .collect();

                Some(TypeList::new(items, type_params.span().clone()))
            }

            (None, None) => {
                None
            }
        };

        let sig_span = match &return_ty {
            Some(return_ty) => func_kw.span().to(return_ty.span()),
            None => func_kw.span().to(&args_span),
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
            params: params.into_iter().flat_map(|params| params).collect(),
            type_params,
            mods,
        })
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

#[derive(Clone, Debug, PartialEq, Hash)]
pub struct FunctionDef<A: Annotation> {
    pub decl: FunctionDecl<A>,
    pub body: Block<A>,
    pub span: Span,
}

impl<A: Annotation> Spanned for FunctionDef<A> {
    fn span(&self) -> &Span {
        &self.span
    }
}

impl<A: Annotation> fmt::Display for FunctionDef<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "{}", self.decl)?;
        write!(f, "{}", self.body)
    }
}
