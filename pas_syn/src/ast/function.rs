use {
    crate::{
        ident::*,
        parse::*,
        ast::{
            Annotation,
            TypeName,
            Block,
        },
        token_tree::*,
        keyword::*,
        Operator,
    },
    std::fmt,
    pas_common::{
        TracedError,
        span::*,
    },
};

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct FunctionParam<A: Annotation> {
    pub ident: Ident,
    pub ty: A::Type,
    pub span: Span,
}

impl<A: Annotation> fmt::Display for FunctionParam<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
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
    pub iface: IdentPath,
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct FunctionDecl<A: Annotation> {
    pub ident: Ident,
    pub span: Span,

    pub params: Vec<FunctionParam<A>>,

    pub return_ty: Option<A::Type>,

    pub impl_iface: Option<InterfaceImpl<A>>,
}

impl FunctionDecl<Span> {
    pub fn parse(tokens: &mut TokenStream) -> ParseResult<Self> {
        let func_kw = tokens.match_one(Keyword::Function)?;

        let mut impl_iface = None;
        let mut ident_token = tokens.match_one(Matcher::AnyIdent)?;

        if tokens.look_ahead().match_one(Operator::Member).is_some() {
            impl_iface = Some(ident_token);
            tokens.advance(1);
            ident_token = tokens.match_one(Matcher::AnyIdent)?;
        }

        let args_tt = tokens.match_one(DelimiterPair::Bracket)?;

        let args_span = args_tt.span().clone();

        let return_ty = match tokens.look_ahead().match_one(Separator::Colon) {
            Some(_) => {
                tokens.advance(1);
                // look for a return type
                Some(TypeName::parse(tokens)?)
            }
            None => None,
        };

        let mut params_tokens = match args_tt {
            TokenTree::Delimited { inner, open, .. } => TokenStream::new(inner, open),
            _ => unreachable!(),
        };

        let params = params_tokens.match_separated(Separator::Semicolon, |_, tokens| {
            let idents = tokens.match_separated(Separator::Comma, |_, tokens| {
                let ident = tokens.match_one(Matcher::AnyIdent)?;
                Ok(Generate::Yield(ident))
            })?;

            if idents.is_empty() {
                return Err(TracedError::trace(
                    ParseError::UnexpectedEOF(Matcher::AnyIdent, tokens.context().clone())
                ));
            }

            tokens.match_one(Separator::Colon)?;
            let ty = TypeName::parse(tokens)?;
            let span = idents[0].span().to(ty.span());

            let params: Vec<_> = idents.into_iter()
                .map(|ident| FunctionParam {
                    ident: ident.into_ident().unwrap(),
                    ty: ty.clone(),
                    span: span.clone(),
                })
                .collect();

            Ok(Generate::Yield(params))
        })?;
        params_tokens.finish()?;

        let span = match &return_ty {
            Some(return_ty) => func_kw.span().to(return_ty.span()),
            None => func_kw.span().to(&args_span),
        };

        let impl_iface = impl_iface.map(|tt| {
            InterfaceImpl {
                for_ty: TypeName::Unknown(tt.span().clone()),
                iface: Path::new(tt.into_ident().unwrap(), Vec::new()), //todo
            }
        });

        Ok(FunctionDecl {
            ident: ident_token.into_ident().unwrap(),
            impl_iface,
            span: span,
            return_ty,
            params: params.into_iter().flat_map(|params| params).collect(),
        })
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
            write!(f, "{}.", iface_impl.iface.join("."))?;
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