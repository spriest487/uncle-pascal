use crate::ast::{type_method_start, Access, Annotation, FunctionName};
use crate::ast::FunctionDecl;
use crate::ast::TypeName;
use crate::parse::{Matcher, ParseError, TryParse};
use crate::parse::Parse;
use crate::parse::ParseResult;
use crate::Ident;
use crate::Keyword;
use crate::Separator;
use crate::TokenStream;
use crate::TokenTree;
use common::span::Span;
use common::span::Spanned;
use derivative::Derivative;
use std::fmt;
use std::rc::Rc;
use common::TracedError;

#[derive(Clone, PartialEq, Eq, Debug, Hash)]
pub enum StructMember<A: Annotation = Span> {
    Field(Field<A>),
    MethodDecl(Method<A>),
}

impl<A: Annotation> StructMember<A> {
    pub fn as_field(&self) -> Option<&Field<A>> {
        match self {
            StructMember::Field(field) => Some(field),
            _ => None,
        }
    }
    
    pub fn as_method(&self) -> Option<&Method<A>> {
        match self {
            StructMember::MethodDecl(method) => Some(method),
            _ => None,
        }
    }
}

impl<A: Annotation> StructMember<A> {
    pub fn ident(&self) -> &Ident {
        match self {
            StructMember::Field(field) => &field.ident,
            StructMember::MethodDecl(method) => method.decl.name.ident(),
        }
    } 
}

impl<A: Annotation> From<Field<A>> for StructMember<A> {
    fn from(value: Field<A>) -> Self {
        StructMember::Field(value)
    }
}

impl<A: Annotation> From<Method<A>> for StructMember<A> {
    fn from(value: Method<A>) -> Self {
        StructMember::MethodDecl(value)
    }
}

impl<A: Annotation> Spanned for StructMember<A> {
    fn span(&self) -> &Span {
        match self {
            StructMember::Field(field) => field.span(),
            StructMember::MethodDecl(method) => method.decl.span(),
        }
    }
}

#[derive(Clone, Eq, Derivative)]
#[derivative(Debug, PartialEq, Hash)]
pub struct Field<A: Annotation = Span> {
    pub ident: Ident,
    pub ty: A::Type,
    
    pub access: Access,

    #[derivative(Debug = "ignore")]
    #[derivative(PartialEq = "ignore")]
    #[derivative(Hash = "ignore")]
    pub span: Span,
}

impl<A:Annotation> Spanned for Field<A> {
    fn span(&self) -> &Span {
        &self.span
    }
}

impl<A: Annotation> fmt::Display for Field<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}: {}", self.ident, self.ty)
    }
}

pub fn struct_member_start() -> Matcher {
    type_method_start() | Matcher::AnyIdent
}

pub fn parse_struct_members(
    tokens: &mut TokenStream,
    default_access: Access
) -> ParseResult<Vec<StructMember>> {
    let mut access = default_access;

    let mut members = Vec::new();
    loop {
        if let Some(new_access) = Access::try_parse(tokens)? {
            access = new_access;
        }
        
        let next_start = tokens.look_ahead().next();
        
        match next_start {
            Some(TokenTree::Keyword { kw: Keyword::End, .. }) => {
                break;
            }

            Some(tt) if !struct_member_start().is_match(&tt) => {
                let err = ParseError::UnexpectedToken(
                    Box::new(tt),
                    Some(struct_member_start() | Keyword::End)
                );
                return Err(TracedError::trace(err));
            }
            
            Some(TokenTree::Ident(..)) => parse_field(tokens, access, &mut members)?,

            Some(method_start_tt) => {
                assert!(type_method_start().is_match(&method_start_tt));

                parse_method_decl(tokens, access, &mut members)?
            },

            None => break,
        };
        
        if tokens.match_one_maybe(Separator::Semicolon).is_none() {
            break;
        }
    }

    Ok(members)
}

fn parse_field(
    tokens: &mut TokenStream,
    access: Access,
    members: &mut Vec<StructMember>
) -> ParseResult<()> {
    let ident = Ident::parse(tokens)?;
    let mut idents = vec![ident];

    while tokens.match_one_maybe(Separator::Comma).is_some() {
        let ident = Ident::parse(tokens)?;
        idents.push(ident);
    }

    tokens.match_one(Separator::Colon)?;
    let ty = TypeName::parse(tokens)?;

    for ident in idents {
        let field = Field {
            access: access,
            span: ident.span().to(&ty),
            ty: ty.clone(),
            ident,
        };

        members.push(StructMember::Field(field));
    }
    
    Ok(())
}

fn parse_method_decl(
    tokens: &mut TokenStream,
    access: Access,
    members: &mut Vec<StructMember>
) -> ParseResult<()> {
    // these get parsed one at a time
    let decl = FunctionDecl::parse(tokens)?;
    
    members.push(StructMember::MethodDecl(Method {
        decl: Rc::new(decl),
        access,
    }));
    
    Ok(())
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Method<A: Annotation = Span> {
    pub access: Access,
    pub decl: Rc<FunctionDecl<A>>,
}

pub(crate) fn write_access_if_changed(
    f: &mut fmt::Formatter,
    current: &mut Access,
    next: Access
) -> fmt::Result {
    if *current != next {
        *current = next;
        
        write!(f, "{}", next)?;
    }

    Ok(())
}
