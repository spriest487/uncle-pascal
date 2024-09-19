mod member;

use crate::ast::Annotation;
use crate::ast::FunctionDecl;
use crate::ast::Ident;
use crate::ast::TypeDeclName;
use crate::parse::Matcher;
use crate::parse::ParseResult;
use crate::parse::TokenStream;
use crate::Keyword;
use common::span::Span;
use common::span::Spanned;
use derivative::*;
pub use member::*;
use std::fmt;

#[cfg(test)]
mod test;

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub enum StructKind {
    /// heap-allocated, reference-counted type, passed by pointer. declared
    /// with the `class` keyword.
    Class,

    /// locally-allocated value type. declared with the `record` keyword.
    Record,

    /// locally-allocated value type with memory layout based on declared field order and no extra padding.
    /// declared with the `packed record` keywords
    PackedRecord,
}

#[derive(Clone, Eq, Derivative)]
#[derivative(Debug, PartialEq, Hash)]
pub struct StructDef<A: Annotation = Span> {
    pub kind: StructKind,
    pub name: A::Name,
    pub members: Vec<StructMember<A>>,

    #[derivative(Debug = "ignore")]
    #[derivative(PartialEq = "ignore")]
    #[derivative(Hash = "ignore")]
    pub span: Span,
}

impl<A: Annotation> StructDef<A> {
    pub fn find_member(&self, by_ident: &Ident) -> Option<&StructMember<A>> {
        self.members.iter().find(|m| m.ident() == by_ident)
    }
    
    pub fn find_field(&self, by_ident: &Ident) -> Option<&Field<A>> {
        self.members.iter().find_map(|m| match m {
            StructMember::Field(field) if field.ident == *by_ident => Some(field),
            _ => None,
        })
    }

    pub fn find_method(&self, by_ident: &Ident) -> Option<&FunctionDecl<A>> {
        self.members.iter().find_map(|m| match m {
            StructMember::MethodDecl(decl) if decl.ident == *by_ident => Some(decl),
            _ => None,
        })
    }
    
    pub fn fields(&self) -> impl Iterator<Item=&Field<A>> {
        self.members.iter()
            .filter_map(|m| match m {
                StructMember::Field(field) => Some(field),
                _ => None,
            })
    }

    pub fn methods(&self) -> impl Iterator<Item=&FunctionDecl<A>> {
        self.members.iter()
            .filter_map(|m| match m {
                StructMember::MethodDecl(method) => Some(method),
                _ => None,
            })
    }
}

impl StructDef<Span> {
    pub fn match_kw() -> Matcher {
        Keyword::Class | Keyword::Record | Keyword::Packed
    }

    pub fn parse(tokens: &mut TokenStream, name: TypeDeclName) -> ParseResult<Self> {
        let kw_token = tokens.match_one(Self::match_kw())?;
        let kind = match &kw_token {
            tt if tt.is_keyword(Keyword::Class) => StructKind::Class,
            tt if tt.is_keyword(Keyword::Record) => StructKind::Record,
            tt if tt.is_keyword(Keyword::Packed) => {
                tokens.match_one(Keyword::Record)?;
                StructKind::PackedRecord
            },
            _ => unreachable!(),
        };

        let members = parse_struct_members(tokens)?;

        let end_token = tokens.match_one(Keyword::End)?;

        Ok(StructDef {
            kind,
            name,
            members,
            span: kw_token.span().to(end_token.span()),
        })
    }
}

impl<A: Annotation> Spanned for StructDef<A> {
    fn span(&self) -> &Span {
        &self.span
    }
}

impl<A: Annotation> fmt::Display for StructDef<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let kind = match self.kind {
            StructKind::Record => "record",
            StructKind::PackedRecord => "packed record",
            StructKind::Class => "class",
        };
        writeln!(f, "{}", kind)?;
        
        for member in &self.members {
            match member {
                StructMember::Field(field) => writeln!(f, "  {};", field)?,
                StructMember::MethodDecl(decl) => writeln!(f, "  {};", decl)?,
            }
        }
        write!(f, "end")
    }
}
