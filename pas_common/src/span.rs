use {
    std::{
        rc::Rc,
        path::PathBuf,
        fmt::{self},
    },
};

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct Location {
    pub line: usize,
    pub col: usize,
}

#[derive(Clone, Eq, PartialEq, Hash)]
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

    pub fn to(&self, other: &Span) -> Self {
        Self {
            file: self.file.clone(),
            start: self.start,
            end: other.end,
        }
    }
}

impl fmt::Display for Location {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}", self.line + 1, self.col + 1)
    }
}

impl fmt::Debug for Span {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Span({}:{}:{}..{}:{})", self.file.display(),
               self.start.line, self.start.col,
               self.end.line, self.end.col)
    }
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}", self.file.to_string_lossy(), self.start)
    }
}

pub trait Spanned: fmt::Display {
    fn span(&self) -> &Span;
}

impl Spanned for Span {
    fn span(&self) -> &Span {
        self
    }
}
