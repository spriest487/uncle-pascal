use crate::{
    ast::{FunctionDecl, TypeList, unit::AliasDecl},
    parse::prelude::*,
};
use std::rc::Rc;
use std::hash::{Hash, Hasher};

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct CompositeMember<A: Annotation> {
    pub ident: Ident,
    pub ty: A::Type,
    pub span: Span,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub enum CompositeKind {
    /// heap-allocated, reference-counted type, passed by pointer. declared
    /// with the `class` keyword.
    Class,

    /// locally-allocated value type. declared with the `record` keyword.
    Record,
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct Composite<A: Annotation> {
    pub kind: CompositeKind,
    pub name: A::Name,
    pub members: Vec<CompositeMember<A>>,
    pub span: Span,
}

impl<A: Annotation> Composite<A> {
    pub fn find_member(&self, by_ident: &Ident) -> Option<&CompositeMember<A>> {
        self.members.iter().find(|m| m.ident == *by_ident)
    }
}

impl Composite<Span> {
    fn match_kw() -> Matcher {
        Keyword::Class.or(Keyword::Record)
    }

    fn parse(tokens: &mut TokenStream, name: TypeDeclName) -> ParseResult<Self> {
        let kw_token = tokens.match_one(Self::match_kw())?;

        let members = tokens.match_separated(Separator::Semicolon, |_, tokens| {
            if tokens.look_ahead().match_one(Keyword::End).is_some() {
                return Ok(Generate::Break);
            }

            let ident_token = tokens.match_one(Matcher::AnyIdent)?;
            tokens.match_one(Separator::Colon)?;
            let ty = TypeName::parse(tokens)?;

            let ident = ident_token.into_ident().unwrap();

            Ok(Generate::Yield(CompositeMember {
                span: ident.span.to(&ty),
                ty,
                ident,
            }))
        })?;

        let end_token = tokens.match_one(Keyword::End)?;

        let kind = match kw_token {
            TokenTree::Keyword {
                kw: Keyword::Class, ..
            } => CompositeKind::Class,
            TokenTree::Keyword {
                kw: Keyword::Record,
                ..
            } => CompositeKind::Record,
            _ => unreachable!(),
        };

        Ok(Composite {
            kind,
            name,
            members,
            span: kw_token.span().to(end_token.span()),
        })
    }
}

impl<A: Annotation> Spanned for Composite<A> {
    fn span(&self) -> &Span {
        &self.span
    }
}

impl<A: Annotation> fmt::Display for Composite<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(
            f,
            "{}",
            match self.kind {
                CompositeKind::Record => "record",
                CompositeKind::Class => "class",
            }
        )?;
        for member in &self.members {
            writeln!(f, "{}: {};", member.ident, member.ty)?;
        }
        write!(f, "end")
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct InterfaceMethodDecl<A: Annotation> {
    pub decl: FunctionDecl<A>,
}

impl<A: Annotation> InterfaceMethodDecl<A> {
    pub fn ident(&self) -> &Ident {
        assert_eq!(1, self.decl.ident.len(), "interface methods should always have a single-part ident path after parsing");
        self.decl.ident.single()
    }
}

impl<A: Annotation> Spanned for InterfaceMethodDecl<A> {
    fn span(&self) -> &Span {
        self.decl.span()
    }
}

impl<A: Annotation> fmt::Display for InterfaceMethodDecl<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.decl)
    }
}

impl ParseSeq for InterfaceMethodDecl<Span> {
    fn parse_group(prev: &[Self], tokens: &mut TokenStream) -> ParseResult<Self> {
        if !prev.is_empty() {
            tokens.match_one(Separator::Semicolon)?;
        }

        let decl = FunctionDecl::parse(tokens)?;
        Ok(InterfaceMethodDecl {
            decl
        })
    }

    fn has_more(prev: &[Self], tokens: &mut LookAheadTokenStream) -> bool {
        if !prev.is_empty() && tokens.match_one(Separator::Semicolon).is_none() {
            return false;
        }

        tokens.match_one(Keyword::Function.or(Keyword::Procedure)).is_some()
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct Interface<A: Annotation> {
    pub name: A::Name,
    pub methods: Vec<InterfaceMethodDecl<A>>,
    pub span: Span,
}

impl<A: Annotation> Interface<A> {
    pub fn get_method(&self, method: &Ident) -> Option<&InterfaceMethodDecl<A>> {
        self.methods.iter().find(|m| *m.ident() == *method)
    }
}

impl Interface<Span> {
    pub fn parse(tokens: &mut TokenStream, name: TypeDeclName) -> ParseResult<Self> {
        let iface_kw = tokens.match_one(Keyword::Interface)?;

        let methods = InterfaceMethodDecl::parse_seq(tokens)?;
        tokens.match_one_maybe(Separator::Semicolon);

        let end = tokens.match_one(Keyword::End)?;

        Ok(Interface {
            name,
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
    pub name: A::Name,
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
        self.cases.iter().position(|c| c.ident == *case_ident)
    }
}

impl Variant<Span> {
    pub fn parse(tokens: &mut TokenStream, name: TypeDeclName) -> ParseResult<Self> {
        let kw = tokens.match_one(Keyword::Variant)?;
        let cases = tokens.match_separated(Separator::Semicolon, |_, tokens| {
            if tokens.look_ahead().match_one(Keyword::End).is_some() {
                return Ok(Generate::Break);
            }

            let ident_tt = tokens.match_one(Matcher::AnyIdent)?;

            let (data_ty, span) = match tokens.match_one_maybe(Separator::Colon) {
                Some(..) => {
                    let ty = TypeName::parse(tokens)?;
                    let span = ident_tt.span().to(ty.span());
                    (Some(ty), span)
                }
                None => (None, ident_tt.span().clone()),
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

        Ok(Variant { name, cases, span })
    }
}

impl<A: Annotation> fmt::Display for Variant<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "variant {}", self.name)?;
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
    Composite(Rc<Composite<A>>),
    Interface(Rc<Interface<A>>),
    Variant(Rc<Variant<A>>),
    Alias(Rc<AliasDecl<A>>),
}

impl<A: Annotation> TypeDecl<A> {
    pub fn ident(&self) -> &A::Name {
        match self {
            TypeDecl::Composite(class) => &class.name,
            TypeDecl::Interface(iface) => &iface.name,
            TypeDecl::Variant(variant) => &variant.name,
            TypeDecl::Alias(alias) => &alias.name,
        }
    }
}

/// the common part of a typedecl before the `=`, eg in `type X<Y> = class...`, `X<Y>` is the decl
/// name. we parse it first and pass it into the parse functions for specific decl kinds.
/// this isn't quite the same thing as a TypeName, which can be a full qualified path - a decl
/// name is a single unqualified ident + maybe a type parameter list
#[derive(Debug, Clone, Eq)]
pub struct TypeDeclName {
    pub ident: Ident,
    pub type_params: Option<TypeList<Ident>>,
    pub span: Span,
}

impl PartialEq for TypeDeclName {
    fn eq(&self, other: &Self) -> bool {
        self.ident == other.ident
            && self.type_params == other.type_params
    }
}

impl Hash for TypeDeclName {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.ident.hash(state);
        self.type_params.hash(state);
    }
}

impl DeclNamed for TypeDeclName {
    fn as_local(&self) -> &Self {
        self
    }

    fn decl_ty_params(&self) -> &[Ident] {
        match &self.type_params {
            Some(type_params) => &type_params.items,
            None => &[],
        }
    }
}

impl fmt::Display for TypeDeclName {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.ident)?;

        if let Some(type_params) = &self.type_params {
            write!(f, "{}", type_params)?;
        }

        Ok(())
    }
}

impl Spanned for TypeDeclName {
    fn span(&self) -> &Span {
        &self.span
    }
}

impl From<Ident> for TypeDeclName {
    fn from(ident: Ident) -> Self {
        TypeDeclName {
            span: ident.span().clone(),
            ident,
            type_params: None,
        }
    }
}

impl TypeDeclName {
    pub fn parse(tokens: &mut TokenStream) -> ParseResult<Self> {
        let ident = tokens.match_one(Matcher::AnyIdent)?.into_ident().unwrap();

        let type_params = match tokens.look_ahead().match_one(DelimiterPair::SquareBracket) {
            Some(..) => Some(TypeList::parse_type_params(tokens)?),
            None => None,
        };

        let span = match &type_params {
            Some(type_param_list) => ident.span().to(type_param_list.span()),
            None => ident.span().clone(),
        };

        Ok(Self {
            ident,
            type_params,
            span,
        })
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
        let name = TypeDeclName::parse(tokens)?;
        tokens.match_one(Operator::Equals)?;

        let composite_kw_matcher = Composite::match_kw();
        let decl_start_matcher = composite_kw_matcher.clone().or(Keyword::Variant);

        match tokens.look_ahead().next() {
            Some(ref tt) if composite_kw_matcher.is_match(tt) => {
                let composite_decl = Composite::parse(tokens, name)?;
                Ok(TypeDecl::Composite(Rc::new(composite_decl)))
            }

            Some(TokenTree::Keyword {
                kw: Keyword::Interface,
                ..
            }) => {
                let iface_decl = Interface::parse(tokens, name)?;
                Ok(TypeDecl::Interface(Rc::new(iface_decl)))
            }

            Some(TokenTree::Keyword {
                kw: Keyword::Variant,
                ..
            }) => {
                let variant_decl = Variant::parse(tokens, name)?;
                Ok(TypeDecl::Variant(Rc::new(variant_decl)))
            }

            // if it isn't a type def keyword, then it must be the name of an existing type to
            // declare an alias
            Some(..) => {
                let alias_decl = AliasDecl::parse(tokens, name)?;
                Ok(TypeDecl::Alias(Rc::new(alias_decl)))
            },

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
            TypeDecl::Composite(class) => class.span(),
            TypeDecl::Interface(iface) => iface.span(),
            TypeDecl::Variant(variant) => variant.span(),
            TypeDecl::Alias(alias) => alias.span(),
        }
    }
}

impl<A: Annotation> fmt::Display for TypeDecl<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "type {} = ", self.ident().as_local())?;

        match self {
            TypeDecl::Composite(class) => write!(f, "{}", class),
            TypeDecl::Interface(iface) => write!(f, "{}", iface),
            TypeDecl::Variant(variant) => write!(f, "{}", variant),
            TypeDecl::Alias(alias) => write!(f, "{}", alias),
        }
    }
}
