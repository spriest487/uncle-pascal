mod enum_decl;
mod iface_decl;
mod struct_def;
mod variant_def;

pub use self::enum_decl::*;
pub use self::iface_decl::*;
pub use self::struct_def::*;
pub use self::variant_def::*;
use crate::ast::unit::AliasDecl;
use crate::ast::Ident;
use crate::ast::Keyword;
use crate::ast::Operator;
use crate::ast::TypeList;
use crate::ast::TypeParam;
use crate::ast::{Annotation, TypeName};
use crate::parse::Matcher;
use crate::parse::Parse;
use crate::parse::ParseError;
use crate::parse::ParseResult;
use crate::parse::ParseSeq;
use crate::parse::TokenStream;
use crate::parse::LookAheadTokenStream;
use crate::DelimiterPair;
use crate::Separator;
use common::span::Span;
use common::span::Spanned;
use common::TracedError;
use derivative::*;
use std::fmt;
use std::fmt::Debug;
use std::hash::Hash;
use std::rc::Rc;

#[derive(Clone, Eq, Derivative)]
#[derivative(PartialEq, Debug, Hash)]
pub struct TypeDecl<A: Annotation = Span> {
    pub items: Vec<TypeDeclItem<A>>,

    #[derivative(Debug = "ignore")]
    #[derivative(Hash = "ignore")]
    #[derivative(PartialEq = "ignore")]
    pub span: Span,
}

impl Parse for TypeDecl<Span> {
    fn parse(tokens: &mut TokenStream) -> ParseResult<Self> {
        let kw_tt = tokens.match_one(Keyword::Type)?;

        let items = TypeDeclItem::parse_seq(tokens)?;

        let last_item = items.last().ok_or_else(|| {
            TracedError::trace(ParseError::EmptyTypeDecl {
                span: kw_tt.clone().into_span(),
            })
        })?;

        let span = kw_tt.span().to(last_item.span());

        Ok(TypeDecl { span, items })
    }
}

impl<A: Annotation> Spanned for TypeDecl<A> {
    fn span(&self) -> &Span {
        &self.span
    }
}

impl<A: Annotation> fmt::Display for TypeDecl<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "type")?;
        for (i, item) in self.items.iter().enumerate() {
            if i > 0 {
                writeln!(f, ";")?;
            }
            write!(f, "\t{}", item)?;
        }

        Ok(())
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum TypeDeclItem<A: Annotation = Span> {
    Struct(Rc<StructDef<A>>),
    Interface(Rc<InterfaceDecl<A>>),
    Variant(Rc<VariantDef<A>>),
    Alias(Rc<AliasDecl<A>>),
    Enum(Rc<EnumDecl<A>>),
}

impl<A: Annotation> TypeDeclItem<A> {
    pub fn name(&self) -> &A::Name {
        match self {
            TypeDeclItem::Struct(class) => &class.name,
            TypeDeclItem::Interface(iface) => &iface.name,
            TypeDeclItem::Variant(variant) => &variant.name,
            TypeDeclItem::Alias(alias) => &alias.name,
            TypeDeclItem::Enum(enum_decl) => &enum_decl.name,
        }
    }
}

impl ParseSeq for TypeDeclItem<Span> {
    fn parse_group(prev: &[Self], tokens: &mut TokenStream) -> ParseResult<Self> {
        if !prev.is_empty() {
            tokens.match_one(Separator::Semicolon)?;
        }

        TypeDeclItem::parse(tokens)
    }

    fn has_more(prev: &[Self], tokens: &mut LookAheadTokenStream) -> bool {
        if !prev.is_empty() && tokens.match_one(Separator::Semicolon).is_none() {
            return false;
        }

        tokens.match_one(Matcher::AnyIdent).is_some()
    }
}

/// the common part of a typedecl before the `=`, eg in `type X[Y] = class...`, `X<Y>` is the decl
/// name. we parse it first and pass it into the parse functions for specific decl kinds.
/// this isn't quite the same thing as a TypeName, which can be a full qualified path - a decl
/// name is a single unqualified ident + maybe a type parameter list
#[derive(Clone, Eq, Derivative)]
#[derivative(PartialEq, Debug, Hash)]
pub struct TypeDeclName {
    pub ident: Ident,
    pub type_params: Option<TypeList<TypeParam<TypeName>>>,

    #[derivative(Debug = "ignore")]
    #[derivative(Hash = "ignore")]
    #[derivative(PartialEq = "ignore")]
    pub span: Span,
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
        
        let type_params = TypeList::try_parse_type_params(tokens)?
            .map(|list| {
                list.map(|name| TypeParam {
                    name,
                    // in the context of a name on its own, type params are parsed without 
                    // constraints, which we need to parse later and add to this decl if needed
                    constraint: None,
                })
            });
        

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

impl Parse for TypeDeclItem<Span> {
    fn parse(tokens: &mut TokenStream) -> ParseResult<Self> {
        let name = TypeDeclName::parse(tokens)?;
        tokens.match_one(Operator::Equals)?;

        let composite_kw_matcher = StructDef::match_kw();
        let decl_start_matcher = composite_kw_matcher.clone().or(Keyword::Variant);

        match tokens.look_ahead().next() {
            Some(ref tt) if composite_kw_matcher.is_match(tt) => {
                let composite_decl = StructDef::parse(tokens, name)?;
                Ok(TypeDeclItem::Struct(Rc::new(composite_decl)))
            },

            Some(tt) if tt.is_keyword(Keyword::Interface) => {
                let iface_decl = InterfaceDecl::parse(tokens, name)?;
                Ok(TypeDeclItem::Interface(Rc::new(iface_decl)))
            },

            Some(tt) if tt.is_keyword(Keyword::Variant) => {
                let variant_decl = VariantDef::parse(tokens, name)?;
                Ok(TypeDeclItem::Variant(Rc::new(variant_decl)))
            },

            Some(tt) if tt.is_delimited(DelimiterPair::Bracket) => {
                let enum_decl = EnumDecl::parse(name, tokens)?;
                Ok(TypeDeclItem::Enum(Rc::new(enum_decl)))
            },

            // if it isn't a type def keyword, then it must be the name of an existing type to
            // declare an alias
            Some(..) => {
                let alias_decl = AliasDecl::parse(tokens, name)?;
                Ok(TypeDeclItem::Alias(Rc::new(alias_decl)))
            },

            None => Err(TracedError::trace(ParseError::UnexpectedEOF(
                decl_start_matcher.clone(),
                tokens.context().clone(),
            ))),
        }
    }
}

impl<A: Annotation> Spanned for TypeDeclItem<A> {
    fn span(&self) -> &Span {
        match self {
            TypeDeclItem::Struct(class) => class.span(),
            TypeDeclItem::Interface(iface) => iface.span(),
            TypeDeclItem::Variant(variant) => variant.span(),
            TypeDeclItem::Alias(alias) => alias.span(),
            TypeDeclItem::Enum(enum_decl) => enum_decl.span(),
        }
    }
}

impl<A: Annotation> fmt::Display for TypeDeclItem<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TypeDeclItem::Struct(class) => write!(f, "{}", class),
            TypeDeclItem::Interface(iface) => write!(f, "{}", iface),
            TypeDeclItem::Variant(variant) => write!(f, "{}", variant),
            TypeDeclItem::Alias(alias) => write!(f, "{}", alias),
            TypeDeclItem::Enum(enum_decl) => write!(f, "{}", enum_decl),
        }
    }
}
