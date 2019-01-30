use {
    std::{
        fmt,
    },
    pas_common::{
        span::*
    },
    pas_syn::{
        ast::Annotation,
    },
    crate::{
        Type,
        ValueKind,
    },
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeAnnotation {
    pub ty: Type,
    pub value_kind: Option<ValueKind>,
    pub span: Span,
}

impl TypeAnnotation {
    pub fn typed_value(ty: impl Into<Type>, kind: ValueKind, span: Span) -> TypeAnnotation {
        Self {
            ty: ty.into(),
            value_kind: Some(kind),
            span,
        }
    }

    pub fn untyped(span: Span) -> TypeAnnotation {
        Self {
            ty: Type::Nothing,
            value_kind: None,
            span,
        }
    }
}

impl fmt::Display for TypeAnnotation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.span)
    }
}

impl Spanned for TypeAnnotation {
    fn span(&self) -> &Span {
        &self.span
    }
}

impl Annotation for TypeAnnotation {
    type Type = Type;
}