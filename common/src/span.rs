use crate::path_relative_to_cwd;
use serde::Deserialize;
use serde::Serialize;
use std::cmp::Ordering;
use std::fmt;
use std::path::PathBuf;
use std::rc::Rc;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash, Serialize, Deserialize)]
pub struct Location {
    pub line: usize,
    pub col: usize,
}

impl Location {
    pub fn new(line: usize, col: usize) -> Self {
        Self { line, col }
    }

    pub fn zero() -> Self {
        Location {
            line: 0,
            col: 0,
        }
    }
}

impl fmt::Display for Location {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}", self.line + 1, self.col + 1)
    }
}

impl Ord for Location {
    fn cmp(&self, other: &Self) -> Ordering {
        match self.line.cmp(&other.line) {
            Ordering::Equal => self.col.cmp(&other.col),
            line_ord => line_ord,
        }
    }
}

impl PartialOrd for Location {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

#[derive(Clone, Eq, PartialEq, Hash, Serialize, Deserialize)]
pub struct Span {
    pub file: Rc<PathBuf>,
    pub start: Location,
    pub end: Location,
}

impl Span {
    pub fn new(file: impl Into<PathBuf>, start: Location, end: Location) -> Self {
        Self {
            file: Rc::new(file.into()),
            start,
            end,
        }
    }

    pub fn zero(file: impl Into<PathBuf>) -> Self {
        Self {
            file: Rc::new(file.into()),
            start: Location { line: 0, col: 0 },
            end: Location { line: 0, col: 0 },
        }
    }

    pub fn to(&self, other: &impl Spanned) -> Self {
        Self {
            file: self.file.clone(),
            start: self.start,
            end: other.span().end,
        }
    }
    
    pub fn range(spans: &[impl Spanned]) -> Option<Self> {
        match spans {
            [] => None,
            [single] => Some(single.span().clone()),
            [first, .., last] => Some(first.span().to(last.span())),
        }
    }

    pub fn of_slice<T: Spanned>(slice: &[T]) -> Span {
        match slice.len() {
            0 => panic!("Span::of_slice: argument must have at least one element"),
            1 => slice[0].span().clone(),
            _ => slice[0].span().to(slice[slice.len() - 1].span()),
        }
    }
}

impl fmt::Debug for Span {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "Span({}:{}:{}..{}:{})",
            path_relative_to_cwd(&self.file).display(),
            self.start.line,
            self.start.col,
            self.end.line,
            self.end.col
        )
    }
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let rel_file = path_relative_to_cwd(&self.file);
        write!(f, "{}:{}", rel_file.display(), self.start)
    }
}

pub trait Spanned {
    fn span(&self) -> &Span;
}

impl Spanned for Span {
    fn span(&self) -> &Span {
        self
    }
}
