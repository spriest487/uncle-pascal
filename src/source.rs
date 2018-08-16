use std::rc::*;
use std::fmt;

use tokens;

#[derive(Clone)]
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
}

impl fmt::Display for Location {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} {}:{}", *self.file, self.line, self.col)
    }
}

#[derive(Clone)]
pub struct Token {
    pub token: Rc<tokens::Token>,
    pub location: Location,
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

#[allow(dead_code)]
pub fn tokens_to_string(tokens: &Vec<Token>) -> String {
    tokens.iter().map(|t| format!("{{{}}}", t.to_string()))
        .collect::<Vec<_>>()
        .join(", ")
}

#[allow(dead_code)]
pub mod test {
    use std::rc::*;
    use source;
    use tokens;
    use keywords;

    pub fn empty_context() -> source::Token {
        source::Token {
            token: Rc::from(tokens::Keyword(keywords::Program)),
            location: source::Location {
                file: Rc::from(String::from("test")),
                line: 0,
                col: 0,
            }
        }
    }
}