use {
    crate::span::Span,
    std::{
        fmt,
    }
};

#[derive(Eq, PartialEq, Clone, Debug, Hash)]
pub struct Ident {
    pub name: String,
    pub span: Span,
}

impl Ident {
    pub fn new(text: &str, span: impl Into<Span>) -> Self {
        Self {
            name: text.to_string(),
            span: span.into(),
        }
    }
}

impl fmt::Display for Ident {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}
