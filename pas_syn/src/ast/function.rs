use {
    std::fmt,
    pas_common::{
        TracedError,
        span::*,
    },
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
    }
};

#[derive(Clone, Debug, PartialEq, Hash)]
pub struct FunctionParam<A: Annotation> {
    pub ident: Ident,
    pub ty: A::Type,
    pub annotation: A,
}

impl<A: Annotation> fmt::Display for FunctionParam<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}: {}", self.ident, self.ty)
    }
}

impl<A: Annotation> Spanned for FunctionParam<A> {
    fn span(&self) -> &Span {
        &self.annotation.span()
    }
}

#[derive(Clone, Debug, PartialEq, Hash)]
pub struct FunctionDecl<A: Annotation> {
    pub ident: Ident,
    pub annotation: A,

    pub params: Vec<FunctionParam<A>>,

    pub return_ty: Option<A::Type>,

    pub body: Block<A>,
}

impl FunctionDecl<Span> {
    pub fn parse(tokens: &mut TokenStream) -> ParseResult<Self> {
        let func_kw = tokens.match_one(Keyword::Function)?;
        let ident_token = tokens.match_one(Matcher::AnyIdent)?;
        let args_tt = tokens.match_one(DelimiterPair::Bracket)?;

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
                    annotation: span.clone(),
                })
                .collect();

            Ok(Generate::Yield(params))
        })?;
        params_tokens.finish()?;

        let body = Block::parse(tokens)?;

        let span = func_kw.span().to(body.span());

        Ok(FunctionDecl {
            ident: ident_token.as_ident().cloned().unwrap(),
            annotation: span,
            return_ty,
            params: params.into_iter().flat_map(|params| params).collect(),
            body,
        })
    }
}

impl<A: Annotation> Spanned for FunctionDecl<A> {
    fn span(&self) -> &Span {
        self.annotation.span()
    }
}

impl<A: Annotation> fmt::Display for FunctionDecl<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "function {}(", self.ident)?;
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
        writeln!(f)?;
        write!(f, "{}", self.body)
    }
}