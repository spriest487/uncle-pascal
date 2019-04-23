use pas_common::span::*;
use std::{
    fmt::{
        self,
        Write,
    },
    hash::{
        Hash,
        Hasher,
    },
};

#[derive(Eq, Clone, Debug)]
pub struct Ident {
    pub name: String,
    pub span: Span,
}

impl PartialEq for Ident {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

impl Hash for Ident {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.name.hash(state)
    }
}

impl Ident {
    pub fn new(text: &str, span: impl Into<Span>) -> Self {
        Self {
            name: text.to_string(),
            span: span.into(),
        }
    }
}

impl Spanned for Ident {
    fn span(&self) -> &Span {
        &self.span
    }
}

impl fmt::Display for Ident {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct Path<Part> {
    parts: Vec<Part>,
}

impl<Part> Path<Part> {
    pub fn new(name: Part, namespace: impl IntoIterator<Item = Part>) -> Self {
        let mut path: Vec<_> = namespace.into_iter().collect();
        path.push(name);

        Self { parts: path }
    }

    pub fn from_parts(parts: impl IntoIterator<Item = Part>) -> Self {
        let parts: Vec<_> = parts.into_iter().collect();
        Self { parts }
    }

    pub fn iter(&self) -> impl Iterator<Item = &Part> {
        self.parts.iter()
    }

    pub fn into_parts(self) -> Vec<Part> {
        self.parts
    }

    pub fn map<IntoPart>(self, f: impl FnMut(Part) -> IntoPart) -> Path<IntoPart> {
        Path {
            parts: self.parts.into_iter().map(f).collect(),
        }
    }

    pub fn push(&mut self, part: Part) {
        self.parts.push(part);
    }

    pub fn first(&self) -> &Part {
        self.parts.first().unwrap()
    }

    pub fn last(&self) -> &Part {
        self.parts.last().unwrap()
    }

    pub fn as_slice(&self) -> &[Part] {
        self.parts.as_slice()
    }

    pub fn child(mut self, part: Part) -> Self {
        self.parts.push(part);
        self
    }
}

impl<Part: fmt::Display> Path<Part> {
    pub fn join(&self, sep: impl fmt::Display) -> String {
        let mut joined = String::new();
        for (i, part) in self.iter().enumerate() {
            if i > 0 {
                write!(joined, "{}", sep).unwrap();
            }
            write!(joined, "{}", part).unwrap()
        }
        joined
    }
}

pub type IdentPath = Path<Ident>;

impl fmt::Display for IdentPath {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.join("."))
    }
}

impl Spanned for IdentPath {
    fn span(&self) -> &Span {
        self.last().span()
    }
}
