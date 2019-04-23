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

#[derive(Eq, Clone)]
pub struct Ident {
    pub name: String,
    pub span: Span,
}

impl fmt::Debug for Ident {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "\"{}\" @ {:?}", self.name, self.span)
    }
}

impl PartialEq for Ident {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

impl PartialEq<String> for Ident {
    fn eq(&self, name: &String) -> bool {
        self.name == *name
    }
}

impl PartialEq<str> for Ident {
    fn eq(&self, name: &str) -> bool {
        self.name == name
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

#[derive(Clone, Eq, Hash, Debug)]
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

impl<Part: Clone> Path<Part> {
    pub fn parent(&self) -> Option<Self> {
        if self.parts.len() == 1 {
            None
        } else {
            let parent_parts = self.parts[0..self.parts.len() - 1].to_vec();
            Some(Self { parts: parent_parts })
        }
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

impl<Part> From<Part> for Path<Part> {
    fn from(part: Part) -> Self {
        Self { parts: vec![part] }
    }
}

impl<OtherPart, Part: PartialEq<OtherPart>> PartialEq<Path<OtherPart>> for Path<Part> {
    fn eq(&self, other: &Path<OtherPart>) -> bool {
        self.parts.len() == other.parts.len()
            && self.parts.iter().zip(other.parts.iter())
                .all(|(a, b)| *a == *b)
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
