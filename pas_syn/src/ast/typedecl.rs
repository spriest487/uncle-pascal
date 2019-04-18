use crate::{
    ast::FunctionDecl,
    parse::prelude::*,
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
    pub ident: IdentPath,
    pub members: Vec<Member<A>>,
    pub span: Span,
}

impl<A: Annotation> Class<A> {
    pub fn find_member(&self, by_ident: &Ident) -> Option<&Member<A>> {
        self.members.iter().find(|m| m.ident == *by_ident)
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
            TokenTree::Keyword {
                kw: Keyword::Class, ..
            } => ClassKind::Object,
            TokenTree::Keyword {
                kw: Keyword::Record,
                ..
            } => ClassKind::Record,
            _ => unreachable!(),
        };

        Ok(Class {
            kind,
            ident: ident.into(),
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
        writeln!(
            f,
            "{}",
            match self.kind {
                ClassKind::Record => "record",
                ClassKind::Object => "class",
            }
        )?;
        for member in &self.members {
            writeln!(f, "{}: {};", member.ident, member.ty)?;
        }
        write!(f, "end")
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct Interface<A: Annotation> {
    pub ident: IdentPath,
    pub methods: Vec<FunctionDecl<A>>,
    pub span: Span,
}

impl<A: Annotation> Interface<A> {
    pub fn get_method(&self, method: &Ident) -> Option<&FunctionDecl<A>> {
        self.methods.iter().find(|m| m.ident == *method)
    }
}

impl Interface<Span> {
    pub fn parse(tokens: &mut TokenStream, ident: Ident) -> ParseResult<Self> {
        let iface_kw = tokens.match_one(Keyword::Interface)?;
        let methods = tokens.match_separated(Separator::Semicolon, |_, tokens| {
            if tokens.look_ahead().match_one(Keyword::End).is_some() {
                return Ok(Generate::Break);
            }

            let decl = FunctionDecl::parse(tokens)?;
            Ok(Generate::Yield(decl))
        })?;

        let end = tokens.match_one(Keyword::End)?;

        Ok(Interface {
            ident: ident.into(),
            span: iface_kw.span().to(end.span()),
            methods,
        })
    }
}

impl<A: Annotation> Spanned for Interface<A> {
    fn span(&self) -> &Span {
        &self.span
    }
}

impl<A: Annotation> fmt::Display for Interface<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "interface")?;
        for method in &self.methods {
            writeln!(f, "{};", method)?;
        }
        write!(f, "end")
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct Variant<A: Annotation> {
    pub ident: IdentPath,
    pub cases: Vec<VariantCase<A>>,

    pub span: Span,
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct VariantCase<A: Annotation> {
    pub ident: Ident,
    pub data_ty: Option<A::Type>,

    pub span: Span,
}

impl<A: Annotation> Variant<A> {
    pub fn case_position(&self, case_ident: &Ident) -> Option<usize> {
        self.cases.iter()
            .position(|c| c.ident == *case_ident)
    }
}

impl Variant<Span> {
    pub fn parse(tokens: &mut TokenStream, ident: Ident) -> ParseResult<Self> {
        let kw = tokens.match_one(Keyword::Variant)?;
        let cases = tokens.match_separated(Separator::Semicolon, |_, tokens| {
            if tokens.look_ahead().match_one(Keyword::End).is_some() {
                return Ok(Generate::Break);
            }

            let ident_tt = tokens.match_one(Matcher::AnyIdent)?;

            let (data_ty, span) = match tokens.look_ahead().match_one(Separator::Colon) {
                Some(..) => {
                    tokens.advance(1);
                    let ty = TypeName::parse(tokens)?;
                    let span = ident.span().to(ty.span());
                    (Some(ty), span)
                }
                None => (None, ident.span().clone()),
            };

            let ident = ident_tt.into_ident().unwrap();

            Ok(Generate::Yield(VariantCase {
                ident,
                data_ty,
                span,
            }))
        })?;

        let end_kw = tokens.match_one(Keyword::End)?;

        let span = kw.span().to(end_kw.span());

        Ok(Variant {
            ident: IdentPath::from(ident),
            cases,

            span,
        })
    }
}

impl<A: Annotation> fmt::Display for Variant<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "variant {}", self.ident)?;
        for case in &self.cases {
            write!(f, "  {}", case.ident)?;
            if let Some(data_ty) = &case.data_ty {
                write!(f, ": {}", data_ty)?;
            }
            writeln!(f, ";")?;
        }
        write!(f, "end")
    }
}

impl<A: Annotation> Spanned for Variant<A> {
    fn span(&self) -> &Span {
        &self.span
    }
}

#[derive(Clone, Debug)]
pub enum TypeDecl<A: Annotation> {
    Class(Class<A>),
    Interface(Interface<A>),
    Variant(Variant<A>),
}

impl<A: Annotation> TypeDecl<A> {
    pub fn ident(&self) -> &IdentPath {
        match self {
            TypeDecl::Class(class) => &class.ident,
            TypeDecl::Interface(iface) => &iface.ident,
            TypeDecl::Variant(variant) => &variant.ident,
        }
    }
}

impl TypeDecl<Span> {
    /// a type decl block starts with the `type` keyword. each decl in the
    /// block starts with an identifier then "=" and then the type definition.
    /// the block can contain 1+ types, each of which is terminated by a
    /// semicolon
    #[allow(unused)]
    pub fn parse_block(tokens: &mut TokenStream) -> ParseResult<Vec<Self>> {
        tokens.match_one(Keyword::Type)?;

        let decls = tokens.match_separated(Separator::Semicolon, |_, tokens| {
            if tokens.look_ahead().match_one(Matcher::AnyIdent).is_none() {
                return Ok(Generate::Break);
            }

            Self::parse_decl(tokens).map(Generate::Yield)
        })?;

        Ok(decls)
    }

    pub fn parse(tokens: &mut TokenStream) -> ParseResult<Self> {
        tokens.match_one(Keyword::Type)?;

        Self::parse_decl(tokens)
    }

    fn parse_decl(tokens: &mut TokenStream) -> ParseResult<Self> {
        let ident = tokens.match_one(Matcher::AnyIdent)?.into_ident().unwrap();
        tokens.match_one(Operator::Equals)?;

        let class_matcher = Class::match_kw();
        let decl_start_matcher = class_matcher.clone().or(Keyword::Variant);

        match tokens.look_ahead().next() {
            Some(ref tt) if class_matcher.is_match(tt) => {
                let class_decl = Class::parse(tokens, ident.clone())?;
                Ok(TypeDecl::Class(class_decl))
            },

            Some(TokenTree::Keyword {
                kw: Keyword::Interface,
                ..
            }) => {
                let iface_decl = Interface::parse(tokens, ident.clone())?;
                Ok(TypeDecl::Interface(iface_decl))
            },

            Some(TokenTree::Keyword {
                kw: Keyword::Variant,
                ..
                 }) => {
                let variant_decl = Variant::parse(tokens, ident.clone())?;
                Ok(TypeDecl::Variant(variant_decl))
            }

            Some(unexpected) => Err(TracedError::trace(ParseError::UnexpectedToken(
                unexpected,
                Some(decl_start_matcher.clone()),
            ))),

            None => Err(TracedError::trace(ParseError::UnexpectedEOF(
                decl_start_matcher.clone(),
                tokens.context().clone(),
            ))),
        }
    }
}

impl<A: Annotation> Spanned for TypeDecl<A> {
    fn span(&self) -> &Span {
        match self {
            TypeDecl::Class(class) => class.span(),
            TypeDecl::Interface(iface) => iface.span(),
            TypeDecl::Variant(variant) => variant.span(),
        }
    }
}

impl<A: Annotation> fmt::Display for TypeDecl<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "type {} = ", self.ident().last())?;

        match self {
            TypeDecl::Class(class) => write!(f, "{}", class),
            TypeDecl::Interface(iface) => write!(f, "{}", iface),
            TypeDecl::Variant(variant) => write!(f, "{}", variant),
        }
    }
}
