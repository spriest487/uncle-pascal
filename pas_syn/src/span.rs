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

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Span {
    pub file: Rc<PathBuf>,
    pub start: Location,
    pub end: Location,
}

impl Span {
    pub fn fmt_context(&self, mut f: impl fmt::Write, source: &str) -> fmt::Result {
        writeln!(f, "{}:", self)?;

        let line_count = if self.end.line > self.start.line {
            self.end.line - self.start.line
        } else {
            1
        };

        let mut any_lines = false;
        for (y, line) in source.lines().skip(self.start.line).take(line_count).enumerate() {
            any_lines = true;

            if y == 0 || y == line_count - 1 {
                writeln!(f, "    {}", line)?;
                write!(f, "    ")?;
            }

            for x in 0..line.len() {
                if x >= self.start.col && x <= self.end.col
                        && y >= self.start.line && x <= self.end.line {
                    write!(f, "^")?;
                } else {
                    write!(f, "")?;
                }
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

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}:{}", self.file.to_string_lossy(), self.start.line, self.start.col)?;

        if self.start != self.end {
            write!(f, " - {}:{}", self.end.line, self.end.col)
        } else {
            Ok(())
        }
    }
}

pub trait SpanDisplay : fmt::Display {
    fn span(&self) -> &Span;

    fn fmt_context(&self, mut f: impl fmt::Write, source: &str) -> fmt::Result {
        writeln!(f, "{}", self)?;
        self.span().fmt_context(f, source)
    }

    fn print_context(&self, source: &str) {
        println!("{}", self);
        self.span().print_context(source)
    }
}
