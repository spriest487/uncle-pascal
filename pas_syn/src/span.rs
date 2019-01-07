use {
    std::{
        rc::Rc,
        path::PathBuf,
        fmt::{self},
    },
};

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Span {
    pub file: Rc<PathBuf>,
    pub line: usize,
    pub col_start: usize,
    pub col_end: usize,
}

impl Span {
    pub fn fmt_context(&self, mut f: impl fmt::Write, source: &str) -> fmt::Result {
        let line = match source.lines().skip(self.line).next() {
            Some(line) => line,
            None => "<missing line>",
        };

        writeln!(f, "{}:", self)?;
        writeln!(f, "    {}", line)?;

        if self.col_end >= self.col_start {
            write!(f, "    ")?;
            for _ in 0..self.col_start {
                write!(f, " ")?;
            }

            for _ in 0..(self.col_end) - self.col_start {
                write!(f, "^")?;
            }
        }
        Ok(())
    }

    pub fn print_context(&self, source: &str) {
        let mut out = String::new();
        let _ = self.fmt_context(&mut out, source);
        println!("{}", out);
    }
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}:{}", self.file.to_string_lossy(), self.line, self.col_start)?;
        if self.col_end > self.col_start {
            write!(f, "..{}", self.col_end)
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
