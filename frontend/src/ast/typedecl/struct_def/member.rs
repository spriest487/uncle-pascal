use crate::ast::{Annotation, FunctionName};
use crate::ast::FunctionDecl;
use crate::ast::TypeName;
use crate::parse::Matcher;
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
use std::fmt::Display;
use std::fmt::Formatter;
use std::rc::Rc;

#[derive(Clone, PartialEq, Eq, Debug, Hash)]
pub enum StructMember<A: Annotation = Span> {
    Field(Field<A>),
    MethodDecl(Rc<FunctionDecl<A>>),
}

impl<A: Annotation> StructMember<A> {
    pub fn as_field(&self) -> Option<&Field<A>> {
        match self {
            StructMember::Field(field) => Some(field),
            _ => None,
        }
    }
    
    pub fn as_method(&self) -> Option<&Rc<FunctionDecl<A>>> {
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
            StructMember::MethodDecl(decl) => decl.name.ident(),
        }
    } 
}

impl<A: Annotation> From<Field<A>> for StructMember<A> {
    fn from(value: Field<A>) -> Self {
        StructMember::Field(value)
    }
}

impl<A: Annotation> From<FunctionDecl<A>> for StructMember<A> {
    fn from(value: FunctionDecl<A>) -> Self {
        StructMember::MethodDecl(Rc::new(value))
    }
}

impl<A: Annotation> Spanned for StructMember<A> {
    fn span(&self) -> &Span {
        match self {
            StructMember::Field(field) => field.span(),
            StructMember::MethodDecl(method) => method.span(),
        }
    }
}

impl<A: Annotation> Display for StructMember<A> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            StructMember::Field(field) => write!(f, "{}", field),
            StructMember::MethodDecl(method) => write!(f, "{}", method)
        }
    }
}

#[derive(Clone, Eq, Derivative)]
#[derivative(Debug, PartialEq, Hash)]
pub struct Field<A: Annotation = Span> {
    pub ident: Ident,
    pub ty: A::Type,

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

impl<A: Annotation> Display for Field<A> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}: {}", self.ident, self.ty)
    }
}

pub fn parse_struct_members(tokens: &mut TokenStream) -> ParseResult<Vec<StructMember>> {
    let mut members = Vec::new();
    loop {        
        let member_start = Keyword::Function | Keyword::Procedure | Matcher::AnyIdent;
        let next_start = tokens.look_ahead().match_one(member_start);
        
        match next_start {
            Some(TokenTree::Ident(..)) => parse_field(tokens, &mut members)?,
            Some(..) => parse_method_decl(tokens, &mut members)?,
            None => break,
        };
        
        if tokens.match_one_maybe(Separator::Semicolon).is_none() {
            break;
        }
    }

    Ok(members)
}

fn parse_field(tokens: &mut TokenStream, members: &mut Vec<StructMember>) -> ParseResult<()> {
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
            span: ident.span().to(&ty),
            ty: ty.clone(),
            ident,
        };

        members.push(StructMember::Field(field));
    }
    
    Ok(())
}

fn parse_method_decl(tokens: &mut TokenStream, members: &mut Vec<StructMember>) -> ParseResult<()> {
    // these get parsed one at a time
    let decl = FunctionDecl::parse(tokens)?;
    
    members.push(StructMember::from(decl));
    
    Ok(())
}
    
