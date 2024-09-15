use crate::{
    ast::{Annotation, TypeDeclName},
    parse::MatchSequenceOf,
    parse::{MatchOneOf, Matcher, Parse, ParseResult, TokenStream},
    Ident, Keyword, Separator,
};
use derivative::*;
use pas_common::span::{Span, Spanned};
use std::fmt;
use crate::ast::type_name::TypeName;
use crate::parse::TryParse;

#[cfg(test)]
mod test;

#[derive(Clone, Eq, Derivative)]
#[derivative(Debug, PartialEq, Hash)]
pub struct StructMember<A: Annotation> {
    pub ident: Ident,
    pub ty: A::Type,

    #[derivative(Debug = "ignore")]
    #[derivative(PartialEq = "ignore")]
    #[derivative(Hash = "ignore")]
    pub span: Span,
}

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
pub struct StructDef<A: Annotation> {
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
        self.members.iter().find(|m| m.ident == *by_ident)
    }
}

fn parse_struct_members(tokens: &mut TokenStream) -> ParseResult<Vec<StructMember<Span>>> {
    let mut members = Vec::new();
    loop {
        if !members.is_empty() {
            tokens.match_one(Separator::Semicolon)?;
        }

        let ident = match Ident::try_parse(tokens)? {
            Some(ident) => ident,
            None => break,
        };

        let mut idents = vec![ident];
        while tokens.match_one_maybe(Separator::Comma).is_some() {
            let ident = Ident::parse(tokens)?;
            idents.push(ident);
        }

        tokens.match_one(Separator::Colon)?;
        let ty = TypeName::parse(tokens)?;

        for ident in idents {
            members.push(StructMember {
                span: ident.span().to(&ty),
                ty: ty.clone(),
                ident,
            });
        }

        if tokens
            .look_ahead()
            .match_sequence(Separator::Semicolon.and_then(Matcher::AnyIdent))
            .is_none()
        {
            break;
        }
    }

    Ok(members)
}

impl StructDef<Span> {
    pub fn match_kw() -> Matcher {
        Keyword::Class.or(Keyword::Record).or(Keyword::Packed)
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
        tokens.match_one_maybe(Separator::Semicolon);

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
        writeln!(
            f,
            "{}",
            match self.kind {
                StructKind::Record => "record",
                StructKind::PackedRecord => "packed record",
                StructKind::Class => "class",
            }
        )?;
        for member in &self.members {
            writeln!(f, "{}: {};", member.ident, member.ty)?;
        }
        write!(f, "end")
    }
}
