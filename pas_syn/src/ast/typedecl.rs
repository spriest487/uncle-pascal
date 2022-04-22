use crate::{
    ast::{FunctionDecl, TypeList, unit::AliasDecl},
    parse::prelude::*,
};
use std::{
    rc::Rc,
    hash::{Hash, Hasher}
};
use derivative::*;

#[derive(Clone, Eq, Derivative)]
#[derivative(Debug, PartialEq, Hash)]
pub struct CompositeMember<A: Annotation> {
    pub ident: Ident,
    pub ty: A::Type,

    #[derivative(Debug = "ignore")]
    #[derivative(PartialEq = "ignore")]
    #[derivative(Hash = "ignore")]
    pub span: Span,
}

impl ParseSeq for CompositeMember<Span> {
    fn parse_group(prev: &[Self], tokens: &mut TokenStream) -> ParseResult<Self> {
        if !prev.is_empty() {
            tokens.match_one(Separator::Semicolon)?;
        }

        let ident = Ident::parse(tokens)?;
        tokens.match_one(Separator::Colon)?;
        let ty = TypeName::parse(tokens)?;

        Ok(CompositeMember {
            span: ident.span().to(&ty),
            ty,
            ident,
        })
    }

    fn has_more(prev: &[Self], tokens: &mut LookAheadTokenStream) -> bool {
        if !prev.is_empty() && tokens.match_one(Separator::Semicolon).is_none() {
            return false;
        }

        tokens.match_one(Matcher::AnyIdent).is_some()
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub enum CompositeTypeKind {
    /// heap-allocated, reference-counted type, passed by pointer. declared
    /// with the `class` keyword.
    Class,

    /// locally-allocated value type. declared with the `record` keyword.
    Record,
}

#[derive(Clone, Eq, Derivative)]
#[derivative(Debug, PartialEq, Hash)]
pub struct CompositeTypeDecl<A: Annotation> {
    pub kind: CompositeTypeKind,
    pub name: A::Name,
    pub members: Vec<CompositeMember<A>>,

    #[derivative(Debug = "ignore")]
    #[derivative(PartialEq = "ignore")]
    #[derivative(Hash = "ignore")]
    pub span: Span,
}

impl<A: Annotation> CompositeTypeDecl<A> {
    pub fn find_member(&self, by_ident: &Ident) -> Option<&CompositeMember<A>> {
        self.members.iter().find(|m| m.ident == *by_ident)
    }
}

impl CompositeTypeDecl<Span> {
    fn match_kw() -> Matcher {
        Keyword::Class.or(Keyword::Record)
    }

    fn parse(tokens: &mut TokenStream, name: TypeDeclName) -> ParseResult<Self> {
        let kw_token = tokens.match_one(Self::match_kw())?;
        let kind = match &kw_token {
            tt if tt.is_keyword(Keyword::Class) => CompositeTypeKind::Class,
            tt if tt.is_keyword(Keyword::Record) => CompositeTypeKind::Record,
            _ => unreachable!(),
        };

        let members = CompositeMember::parse_seq(tokens)?;
        tokens.match_one_maybe(Separator::Semicolon);

        let end_token = tokens.match_one(Keyword::End)?;

        Ok(CompositeTypeDecl {
            kind,
            name,
            members,
            span: kw_token.span().to(end_token.span()),
        })
    }
}

impl<A: Annotation> Spanned for CompositeTypeDecl<A> {
    fn span(&self) -> &Span {
        &self.span
    }
}

impl<A: Annotation> fmt::Display for CompositeTypeDecl<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(
            f,
            "{}",
            match self.kind {
                CompositeTypeKind::Record => "record",
                CompositeTypeKind::Class => "class",
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

#[derive(Clone, Eq, Derivative)]
#[derivative(Debug, PartialEq, Hash)]
pub struct InterfaceDecl<A: Annotation> {
    pub name: A::Name,
    pub methods: Vec<InterfaceMethodDecl<A>>,

    #[derivative(Debug = "ignore")]
    #[derivative(PartialEq = "ignore")]
    #[derivative(Hash = "ignore")]
    pub span: Span,
}

impl<A: Annotation> InterfaceDecl<A> {
    pub fn get_method(&self, method: &Ident) -> Option<&InterfaceMethodDecl<A>> {
        self.methods.iter().find(|m| *m.ident() == *method)
    }
}

impl InterfaceDecl<Span> {
    pub fn parse(tokens: &mut TokenStream, name: TypeDeclName) -> ParseResult<Self> {
        let iface_kw = tokens.match_one(Keyword::Interface)?;

        let methods = InterfaceMethodDecl::parse_seq(tokens)?;
        tokens.match_one_maybe(Separator::Semicolon);

        let end = tokens.match_one(Keyword::End)?;

        Ok(InterfaceDecl {
            name,
            span: iface_kw.span().to(end.span()),
            methods,
        })
    }
}

impl<A: Annotation> Spanned for InterfaceDecl<A> {
    fn span(&self) -> &Span {
        &self.span
    }
}

impl<A: Annotation> fmt::Display for InterfaceDecl<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "interface")?;
        for method in &self.methods {
            writeln!(f, "{};", method)?;
        }
        write!(f, "end")
    }
}

#[derive(Clone, Eq, Derivative)]
#[derivative(Debug, PartialEq, Hash)]
pub struct Variant<A: Annotation> {
    pub name: A::Name,
    pub cases: Vec<VariantCase<A>>,

    #[derivative(Debug = "ignore")]
    #[derivative(PartialEq = "ignore")]
    #[derivative(Hash = "ignore")]
    pub span: Span,
}

#[derive(Clone, Eq, Derivative)]
#[derivative(Debug, PartialEq, Hash)]
pub struct VariantCase<A: Annotation> {
    pub ident: Ident,
    pub data_ty: Option<A::Type>,

    #[derivative(Debug = "ignore")]
    #[derivative(PartialEq = "ignore")]
    #[derivative(Hash = "ignore")]
    pub span: Span,
}

impl ParseSeq for VariantCase<Span> {
    fn parse_group(prev: &[Self], tokens: &mut TokenStream) -> ParseResult<Self> {
        if !prev.is_empty() {
            tokens.match_one(Separator::Semicolon)?;
        }

        let ident = Ident::parse(tokens)?;

        let case = match tokens.match_one_maybe(Separator::Colon) {
            Some(..) => {
                let ty = TypeName::parse(tokens)?;
                let span = ident.span().to(ty.span());

                VariantCase {
                    span,
                    ident,
                    data_ty: Some(ty),
                }
            }

            None => VariantCase {
                span: ident.span.clone(),
                ident,
                data_ty: None,
            }
        };

        Ok(case)
    }

    fn has_more(prev: &[Self], tokens: &mut LookAheadTokenStream) -> bool {
        if !prev.is_empty() && tokens.match_one(Separator::Semicolon).is_none() {
            return false;
        }

        tokens.match_one(Matcher::AnyIdent).is_some()
    }
}

impl<A: Annotation> Variant<A> {
    pub fn case_position(&self, case_ident: &Ident) -> Option<usize> {
        self.cases.iter().position(|c| c.ident == *case_ident)
    }
}

impl Variant<Span> {
    pub fn parse(tokens: &mut TokenStream, name: TypeDeclName) -> ParseResult<Self> {
        let kw = tokens.match_one(Keyword::Variant)?;

        let cases = VariantCase::parse_seq(tokens)?;
        tokens.match_one_maybe(Separator::Semicolon);

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
    Composite(Rc<CompositeTypeDecl<A>>),
    Interface(Rc<InterfaceDecl<A>>),
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
    pub fn parse(tokens: &mut TokenStream) -> ParseResult<Self> {
        tokens.match_one(Keyword::Type)?;

        Self::parse_decl(tokens)
    }

    fn parse_decl(tokens: &mut TokenStream) -> ParseResult<Self> {
        let name = TypeDeclName::parse(tokens)?;
        tokens.match_one(Operator::Equals)?;

        let composite_kw_matcher = CompositeTypeDecl::match_kw();
        let decl_start_matcher = composite_kw_matcher.clone().or(Keyword::Variant);

        match tokens.look_ahead().next() {
            Some(ref tt) if composite_kw_matcher.is_match(tt) => {
                let composite_decl = CompositeTypeDecl::parse(tokens, name)?;
                Ok(TypeDecl::Composite(Rc::new(composite_decl)))
            }

            Some(TokenTree::Keyword {
                kw: Keyword::Interface,
                ..
            }) => {
                let iface_decl = InterfaceDecl::parse(tokens, name)?;
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
