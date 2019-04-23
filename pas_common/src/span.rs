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

    pub fn fmt_context(&self, mut f: impl fmt::Write, source: &str) -> fmt::Result {
        writeln!(f, "{}:", self)?;
        writeln!(f)?;

        let line_count = if self.end.line > self.start.line {
            (self.end.line - self.start.line) + 1
        } else {
            1
        };

        let mut any_lines = false;
        for (y, line) in source.lines().enumerate().skip(self.start.line).take(line_count) {
            any_lines = true;

            if y == self.start.line || y == self.end.line {
                writeln!(f, "    {}", line)?;
                write!(f, "    ")?;

                for x in 0..line.len() {
                    let highlight = x >= self.start.col && x <= self.end.col;

                    if highlight {
                        write!(f, "^")?;
                    } else {
                        write!(f, " ")?;
                    }
                }
                writeln!(f)?;
            }
        }

        if !any_lines {
            write!(f, "    <missing line>")?;
        }

        Ok(())
    }

    pub fn print_context(&self, source: &str) {
        let mut out = String::new();
        let _ = self.fmt_context(&mut out, source);
        println!("{}", out);
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

    fn fmt_context(&self, mut f: impl fmt::Write, source: &str) -> fmt::Result {
        writeln!(f, "{}", self)?;
        self.span().fmt_context(f, source)
    }

    fn print_context(&self, source: &str) {
        let mut msg = String::new();
        self.fmt_context(&mut msg, source).unwrap();
        println!("{}", msg);
    }
}

impl Spanned for Span {
    fn span(&self) -> &Span {
        self
    }
}
