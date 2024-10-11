mod member;

use crate::ast::Annotation;
use crate::ast::FunctionName;
use crate::ast::Ident;
use crate::ast::TypeDeclName;
use crate::ast::parse_implements_clause;
use crate::ast::Access;
use crate::parse::{Matcher, ParseResult};
use crate::parse::TokenStream;
use crate::Keyword;
use crate::Separator;
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
    
    pub forward: bool,

    pub fields: Vec<Field<A>>,
    pub methods: Vec<Method<A>>,
    
    pub implements: Vec<A::Type>,

    #[derivative(Debug = "ignore")]
    #[derivative(PartialEq = "ignore")]
    #[derivative(Hash = "ignore")]
    pub span: Span,
}

impl<A: Annotation> StructDef<A> {    
    pub fn find_field(&self, by_ident: &Ident) -> Option<&Field<A>> {
        self.fields.iter().find(|field| field.ident == *by_ident)
    }

    pub fn find_method(&self, by_ident: &Ident) -> Option<&Method<A>> {
        self.methods.iter().find(|method| method.decl.name.ident() == by_ident)
    }
    
    pub fn fields(&self) -> impl Iterator<Item=&Field<A>> {
        self.fields.iter()
    }

    pub fn methods(&self) -> impl Iterator<Item=&Method<A>> {
        self.methods.iter()
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

        let span = kw_token.into_span();
        
        // the last type in a section can never be forward, so every legal forward declaration
        // will end with a semicolon
        if tokens.look_ahead().match_one(Separator::Semicolon).is_some() {
            Ok(StructDef {
                kind,
                name,
                forward: true,
                implements: Vec::new(),
                
                methods: Vec::new(),
                fields: Vec::new(),
                span,
            })
        } else {
            let implements = parse_implements_clause(tokens)?;
            
            let default_access = match kind {
                StructKind::Class => Access::Private,
                StructKind::Record | StructKind::PackedRecord => Access::Public,
            };

            let members = parse_struct_members(tokens, default_access)?;
            
            let mut methods = Vec::new();
            let mut fields = Vec::new();
            for member in members {
                match member {
                    StructMember::Field(field) => fields.push(field),
                    StructMember::MethodDecl(method) => methods.push(method),
                }
            }

            let end_token = tokens.match_one(Keyword::End)?;

            Ok(StructDef {
                kind,
                name,
                forward: false,
                implements,
                fields,
                methods,
                span: span.to(end_token.span()),
            })
        }
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
        
        let mut access = Access::Public;
        
        for field in &self.fields {
            write_access_if_changed(f, &mut access, field.access)?;
            writeln!(f, "  {};", field)?
        }

        for method in &self.methods {
            write_access_if_changed(f, &mut access, method.access)?;
            writeln!(f, "  {};", method.decl)?
        }

        write!(f, "end")
    }
}
