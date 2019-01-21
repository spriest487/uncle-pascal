use {
    crate::parse::prelude::*,
};

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct Member<A: Annotation> {
    pub ident: Ident,
    pub ty: A::Type,
    pub span: Span,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub enum ClassKind {
    /// heap-allocated, reference-counted type, passed by pointer. declared
    /// with the `class` keyword.
    Object,

    /// locally-allocated value type. declared with the `record` keyword.
    Record,
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct Class<A: Annotation> {
    pub kind: ClassKind,
    pub ident: Ident,
    pub members: Vec<Member<A>>,
    pub span: Span,
}

impl<A: Annotation> Class<A> {
    pub fn find_member(&self, by_ident: &Ident) -> Option<&Member<A>> {
        self.members.iter()
            .find(|m| m.ident == *by_ident)
    }
}

impl Class<Span> {
    fn match_kw() -> Matcher {
        Keyword::Class.or(Keyword::Record)
    }

    fn parse(tokens: &mut TokenStream, ident: Ident) -> ParseResult<Self> {
        let kw_token = tokens.match_one(Self::match_kw())?;

        let members = tokens.match_separated(Separator::Semicolon, |_, tokens| {
            if tokens.look_ahead().match_one(Keyword::End).is_some() {
                return Ok(Generate::Break);
            }

            let ident_token = tokens.match_one(Matcher::AnyIdent)?;
            tokens.match_one(Separator::Colon)?;
            let ty = TypeName::parse(tokens)?;

            let ident = ident_token.into_ident().unwrap();

            Ok(Generate::Yield(Member {
                span: ident.span.to(&ty.span()),
                ty,
                ident,
            }))
        })?;

        let end_token = tokens.match_one(Keyword::End)?;

        let kind = match kw_token {
            TokenTree::Keyword { kw: Keyword::Class, .. } => ClassKind::Object,
            TokenTree::Keyword { kw: Keyword::Record, .. } => ClassKind::Record,
            _ => unreachable!(),
        };

        Ok(Class {
            kind,
            ident,
            members,
            span: kw_token.span().to(end_token.span()),
        })
    }
}

impl<A: Annotation> Spanned for Class<A> {
    fn span(&self) -> &Span {
        &self.span
    }
}

impl<A: Annotation> fmt::Display for Class<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "class")?;
        for member in &self.members {
            writeln!(f, "{}: {};", member.ident, member.ty)?;
        }
        write!(f, "end")
    }
}

impl<A: Annotation> TypeDecl<A> {
    pub fn ident(&self) -> &Ident {
        match self {
            TypeDecl::Class(class) => &class.ident,
        }
    }
}

#[derive(Clone, Debug)]
pub enum TypeDecl<A: Annotation> {
    Class(Class<A>),
}

impl TypeDecl<Span> {
    /// a type decl block starts with the `type` keyword. each decl in the
    /// block starts with an identifier then "=" and then the type definition.
    /// the block can contain 1+ types, each of which is terminated by a
    /// semicolon
    pub fn parse(tokens: &mut TokenStream) -> ParseResult<Vec<Self>> {
        let decl_start_matcher = Matcher::from(Keyword::Class);

        tokens.match_one(Keyword::Type)?;

        let decls = tokens.match_separated(Separator::Semicolon, |_, tokens| {
            if tokens.look_ahead().match_one(Matcher::AnyIdent).is_none() {
                return Ok(Generate::Break);
            }

            let ident = tokens.match_one(Matcher::AnyIdent)?.into_ident().unwrap();
            tokens.match_one(Operator::Equals)?;

            match tokens.look_ahead().next() {
                Some(ref tt) if Class::match_kw().is_match(tt) => {
                    let class_decl = Class::parse(tokens, ident.clone())?;
                    Ok(Generate::Yield(TypeDecl::Class(class_decl)))
                },

                Some(unexpected) => Err(TracedError::trace(
                    ParseError::UnexpectedToken(unexpected, Some(decl_start_matcher.clone()))
                )),
                None => Err(TracedError::trace(
                    ParseError::UnexpectedEOF(decl_start_matcher.clone(), tokens.context().clone())
                )),
            }
        })?;

        Ok(decls)
    }
}

impl<A: Annotation> Spanned for TypeDecl<A> {
    fn span(&self) -> &Span {
        match self {
            TypeDecl::Class(class) => class.span(),
        }
    }
}

impl<A: Annotation> fmt::Display for TypeDecl<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "type {} = ", self.ident())?;

        match self {
            TypeDecl::Class(class) => write!(f, "{}", class),
        }
    }
}