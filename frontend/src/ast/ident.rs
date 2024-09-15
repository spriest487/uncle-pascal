use crate::parse::Generate;
use crate::parse::LookAheadTokenStream;
use crate::parse::Match;
use crate::parse::Matcher;
use crate::parse::Parse;
use crate::parse::ParseResult;
use crate::parse::TokenStream;
use crate::Operator;
use derivative::*;
use common::span::Span;
use common::span::Spanned;
use std::fmt;
use std::fmt::Write;
use std::rc::Rc;

#[derive(Eq, Clone, Derivative)]
#[derivative(PartialEq, Debug, Hash)]
pub struct Ident {
    pub name: Rc<String>,

    #[derivative(Debug = "ignore")]
    #[derivative(PartialEq = "ignore")]
    #[derivative(Hash = "ignore")]
    pub span: Span,
}

impl PartialEq<String> for Ident {
    fn eq(&self, name: &String) -> bool {
        *self.name == *name
    }
}

impl PartialEq<str> for Ident {
    fn eq(&self, name: &str) -> bool {
        *self.name == name
    }
}

impl Ident {
    pub fn new(text: &str, span: impl Into<Span>) -> Self {
        Self {
            name: Rc::new(text.to_string()),
            span: span.into(),
        }
    }
}

impl Parse for Ident {
    fn parse(tokens: &mut TokenStream) -> ParseResult<Self> {
        tokens
            .match_one(Matcher::AnyIdent)
            .map(|tt| tt.into_ident().unwrap())
    }
}

impl Match for Ident {
    fn is_match(tokens: &mut LookAheadTokenStream) -> bool {
        tokens.match_one(Matcher::AnyIdent).is_some()
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
pub struct Path<Part: fmt::Debug> {
    parts: Vec<Part>,
}

impl<Part: fmt::Debug> Path<Part> {
    pub fn new(name: Part, namespace: impl IntoIterator<Item = Part>) -> Self {
        let mut path: Vec<_> = namespace.into_iter().collect();
        path.push(name);

        Self { parts: path }
    }

    // paths should never be empty
    pub unsafe fn empty() -> Self {
        Self { parts: Vec::new() }
    }

    pub fn from_parts(parts: impl IntoIterator<Item = Part>) -> Self {
        let parts: Vec<_> = parts.into_iter().collect();
        Self { parts }
    }

    pub fn from_vec(parts: Vec<Part>) -> Self {
        assert!(parts.len() >= 1);
        Self { parts }
    }

    pub fn into_vec(self) -> Vec<Part> {
        self.parts
    }

    pub fn iter(&self) -> impl Iterator<Item = &Part> {
        self.parts.iter()
    }

    pub fn into_parts(self) -> Vec<Part> {
        self.parts
    }

    pub fn map<IntoPart: fmt::Debug>(self, f: impl FnMut(Part) -> IntoPart) -> Path<IntoPart> {
        Path {
            parts: self.parts.into_iter().map(f).collect(),
        }
    }

    pub fn push(&mut self, part: Part) {
        self.parts.push(part);
    }

    pub fn pop(&mut self) -> Part {
        if self.len() == 1 {
            panic!("can't pop the last part from a path");
        }

        self.parts.pop().unwrap()
    }

    pub fn set(&mut self, parts: impl IntoIterator<Item = Part>) {
        self.parts.clear();
        self.parts.extend(parts);

        assert!(self.parts.len() >= 1)
    }

    pub fn extend(&mut self, parts: impl IntoIterator<Item = Part>) {
        self.parts.extend(parts)
    }

    pub fn first(&self) -> &Part {
        self.parts.first().unwrap()
    }

    pub fn len(&self) -> usize {
        self.parts.len()
    }

    pub fn last(&self) -> &Part {
        self.parts.last().unwrap()
    }

    pub fn last_mut(&mut self) -> &mut Part {
        self.parts.last_mut().unwrap()
    }

    pub fn single(&self) -> &Part {
        if self.parts.len() > 1 {
            panic!(
                "single() expects ident path to have only 1 part (path: {:?})",
                self
            );
        }
        self.last()
    }

    pub fn as_slice(&self) -> &[Part] {
        self.parts.as_slice()
    }

    pub fn child(mut self, part: Part) -> Self {
        self.parts.push(part);
        self
    }
}

impl<Part: fmt::Debug + PartialEq> Path<Part> {
    pub fn is_parent_of(&self, other: &Self) -> bool {
        self.parts.len() < other.parts.len()
            && &other.parts[0..self.parts.len()] == self.parts.as_slice()
    }
}

impl<Part: fmt::Debug + Clone> Path<Part> {
    pub fn parent(&self) -> Option<Self> {
        if self.parts.len() == 1 {
            None
        } else {
            let parent_parts = self.parts[0..self.parts.len() - 1].to_vec();
            Some(Self {
                parts: parent_parts,
            })
        }
    }
}

impl<Part: fmt::Debug + fmt::Display> Path<Part> {
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

impl<Part: fmt::Debug> From<Part> for Path<Part> {
    fn from(part: Part) -> Self {
        Self { parts: vec![part] }
    }
}

impl<OtherPart, Part> PartialEq<Path<OtherPart>> for Path<Part>
where
    Part: fmt::Debug + PartialEq<OtherPart>,
    OtherPart: fmt::Debug,
{
    fn eq(&self, other: &Path<OtherPart>) -> bool {
        self.parts.len() == other.parts.len()
            && self
                .parts
                .iter()
                .zip(other.parts.iter())
                .all(|(a, b)| *a == *b)
    }
}

pub type IdentPath = Path<Ident>;

impl IdentPath {
    pub fn parse(tokens: &mut TokenStream) -> ParseResult<Self> {
        let path = tokens.match_repeating(|i, tokens| {
            if i > 0 && tokens.match_one_maybe(Operator::Period).is_none() {
                return Ok(Generate::Break);
            }

            let ident_tt = tokens.match_one(Matcher::AnyIdent)?;
            let ident = ident_tt.into_ident().unwrap();
            Ok(Generate::Yield(ident))
        })?;

        assert!(
            !path.is_empty(),
            "parsed ident path must always have 1+ parts"
        );

        Ok(IdentPath::from_parts(path))
    }

    pub fn path_span(&self) -> Span {
        match self.parts.len() {
            1 => self.parts[0].span.clone(),
            _ => self.parts[0]
                .span
                .to(&self.parts[self.parts.len() - 1].span),
        }
    }

    pub fn to_string_path(&self) -> Path<String> {
        Path {
            parts: self.parts.iter().map(|p| (*p.name).clone()).collect(),
        }
    }
}

impl fmt::Display for Path<Ident> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.join("."))
    }
}

impl Spanned for Path<Ident> {
    fn span(&self) -> &Span {
        self.last().span()
    }
}
