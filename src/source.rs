use std::rc::*;
use std::fmt;

use tokens;

#[derive(Clone, Eq, PartialEq)]
pub struct Location {
    pub file: Rc<String>,

    pub line: usize,
    pub col: usize,
}

impl fmt::Debug for Location {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}

impl Location {
    pub fn new(file: &str, line: usize, col: usize) -> Location {
        Location {
            file: Rc::new(String::from(file)),
            line,
            col
        }
    }

    pub fn ge(&self, other: &Location) -> bool {
        self.line > other.line ||
            (self.line == other.line && self.col >= other.col)
    }

    pub fn gt(&self, other: &Location) -> bool {
        self.line > other.line ||
            (self.line == other.line && self.col > other.col)
    }

    pub fn le(&self, other: &Location) -> bool {
        self.line < other.line ||
            (self.line == other.line && self.col <= other.col)
    }

    pub fn lt(&self, other: &Location) -> bool {
        self.line < other.line ||
            (self.line == other.line && self.col < other.col)
    }
}

impl fmt::Display for Location {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} {}:{}", *self.file, self.line, self.col)
    }
}

#[derive(Clone, PartialEq)]
pub struct Token {
    pub token: Rc<tokens::Token>,
    pub location: Location,
}

impl Token {
    pub fn new(token: impl Into<tokens::Token>, location: impl Into<Location>) -> Self {
        Self {
            token: Rc::new(token.into()),
            location: location.into(),
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} @ {}", self.token, self.location)
    }
}

impl fmt::Debug for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Token {{{} @ {}}}", self.token, self.location)
    }
}

impl tokens::AsToken for Token {
    fn as_token(&self) -> &tokens::Token {
        &self.token
    }
}
