use std::rc::*;
use std::fmt;

use tokens;

#[derive(Clone, Debug)]
pub struct Location {
    pub file: Rc<String>,

    pub line: usize,
    pub col: usize,
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

#[derive(Clone, Debug)]
pub struct Token {
    pub token: Rc<tokens::Token>,
    pub location: Location,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} @ {}", self.token, self.location)
    }
}

impl tokens::AsToken for Token {
    fn as_token(&self) -> &tokens::Token {
        &self.token
    }
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