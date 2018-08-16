use std::{
    fmt,
    rc::*,
};

use tokens;
use operators;
use keywords;
use source;
use opts::CompileOptions;
use consts::IntConstant;

#[derive(Clone, Debug)]
pub struct IllegalToken {
    pub text: String,
    pub file: String,

    pub line: usize,
    pub col: usize,
}

impl fmt::Display for IllegalToken {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "illegal token in {}: {} @ {}:{}", self.file, self.text, self.line, self.col)
    }
}

struct Tokenizer<'a> {
    opts: &'a CompileOptions,

    out_buf: Vec<source::Token>,
}

impl<'a> Tokenizer<'a> {
    pub fn new(opts: &'a CompileOptions) -> Self {
        Tokenizer {
            opts,

            out_buf: Vec::with_capacity(64),
        }
    }
}

struct Lexer<'a> {
    line: Vec<char>,
    line_num: usize,
    col: usize,
    file: Rc<String>,

    opts: &'a CompileOptions,
}

fn find_ahead(s: &[char], predicate: impl Fn(char) -> bool) -> Option<usize> {
    s.iter().enumerate()
        .filter_map(|(col, c)| match predicate(c.clone()) {
            true => Some(col),
            false => None,
        })
        .next()
}

impl<'a> Lexer<'a> {
    fn simple_token(&mut self, t: impl Into<tokens::Token>, advance: usize) -> TokenizeResult<source::Token> {
        let result = self.make_token(t.into());
        self.col += advance;
        Ok(result)
    }

    fn illegal(&self) -> IllegalToken {
        IllegalToken {
            text: self.line[self.col].to_string(),
            file: self.file.to_string(),
            line: self.line_num,
            col: self.col,
        }
    }

    fn literal_hex(&mut self) -> TokenizeResult<source::Token> {
        let line_after_sigil = &self.line[self.col + 1..];
        let next_non_hex = find_ahead(line_after_sigil, |c| match c {
            'a'...'f' |
            'A'...'F' |
            '0'...'9' => false,
            _ => true,
        });

        /* this needs to include the $ because IntConstant::parse expects it */
        let int_str = match next_non_hex {
            Some(end) => &self.line[self.col..self.col + end + 1],
            None => &self.line[self.col..],
        };

        match IntConstant::parse_str(&int_str.iter().collect::<String>()) {
            Some(int) => {
                let token = self.make_token(tokens::LiteralInteger(int));
                self.col += int_str.len();
                Ok(token)
            },
            None => Err(self.illegal()),
        }
    }

    fn literal_int(&mut self, sigil: bool) -> TokenizeResult<source::Token> {
        let start = if sigil { self.col + 1 } else { self.col };

        let next_non_num = find_ahead(&self.line[start..], |c| match c {
            '0'...'9' => false,
            _ => true,
        });

        let int_str = match next_non_num {
            Some(end) => &self.line[self.col..self.col + end],
            None => &self.line[self.col..],
        };

        match IntConstant::parse_str(&int_str.iter().collect::<String>()) {
            Some(int) => {
                let token = self.make_token(tokens::LiteralInteger(int));
                self.col += int_str.len();
                Ok(token)
            },
            None => Err(self.illegal()),
        }
    }

    fn literal_string(&mut self) -> TokenizeResult<source::Token> {
        let mut contents = String::new();

        let mut next_col = self.col + 1;
        loop {
            match self.line.get(next_col) {
                /* todo: better error for unterminated string literal */
                None => return Err(self.illegal()),

                Some('\'') => {
                    /* depends on the token after this one */
                    match self.line.get(next_col + 1) {
                        /* it's quoted quote, add it to the contents and advance an extra col */
                        Some('\'') => {
                            contents.push('\'');
                            next_col += 1;
                        },

                        /* it's something else, this string ends here */
                        _ => {
                            break;
                        },
                    }
                }

                Some(c) => {
                    contents.push(*c);
                },
            }

            next_col += 1;
        };

        let token = self.make_token(tokens::LiteralString(contents));
        self.col = next_col + 1;
        Ok(token)
    }

    fn word(&mut self) -> TokenizeResult<source::Token> {
        let mut token_str = String::new();

        let mut next_col = self.col;
        loop {
            match self.line.get(next_col) {
                Some(c) => match c {
                    '0'...'9' => if token_str.len() > 0 {
                        token_str.push(*c);
                    } else {
                        //can't start with a number
                        return Err(self.illegal());
                    }

                    'a'...'z' |
                    'A'...'Z' |
                    '_' => {
                        token_str.push(*c);
                    }

                    _ => break,
                }

                None => break,
            }

            next_col += 1;
        }

        let token = if let Some(keyword) = keywords::Keyword::try_parse(&token_str, self.opts) {
            self.make_token(tokens::Keyword(keyword))
        } else if let Some(op) = operators::Operator::try_parse_text(&token_str, self.opts) {
            self.make_token(tokens::Operator(op))
        } else {
            self.make_token(tokens::Identifier(token_str))
        };

        self.col = next_col;

        Ok(token)
    }

    fn make_token(&self, token: tokens::Token) -> source::Token {
        source::Token::new(token, source::Location {
            file: self.file.clone(),
            line: self.line_num,
            col: self.col,
        })
    }

    fn next_token(&mut self) -> TokenizeResult<Option<source::Token>> {
        let next = loop {
            let next = match self.line.get(self.col) {
                Some(c) => *c,
                //end if stream
                None => return Ok(None),
            };

            // skip chars until we find something other than whitespace
            if !next.is_ascii_whitespace() {
                break next;
            } else {
                self.col += 1;
            }
        };

        let token = match next {
            '+' => self.simple_token(operators::Plus, 1),
            '-' => self.simple_token(operators::Minus, 1),
            '*' => self.simple_token(operators::Multiply, 1),
            '/' => self.simple_token(operators::Divide, 1),
            '(' => self.simple_token(tokens::BracketLeft, 1),
            ')' => self.simple_token(tokens::BracketRight, 1),
            '[' => self.simple_token(tokens::SquareBracketLeft, 1),
            ']' => self.simple_token(tokens::SquareBracketRight, 1),
            '@' => self.simple_token(operators::AddressOf, 1),
            ',' => self.simple_token(tokens::Comma, 1),
            '.' => self.simple_token(tokens::Period, 1),
            ';' => self.simple_token(tokens::Semicolon, 1),
            '^' => self.simple_token(operators::Deref, 1),
            '>' => match self.line.get(self.col + 1) {
                Some('=') => self.simple_token(operators::Gte, 2),
                _ => self.simple_token(operators::Gt, 1),
            }
            '<' => match self.line.get(self.col + 1) {
                Some('=') => self.simple_token(operators::Lte, 2),
                Some('>') => self.simple_token(operators::NotEquals, 2),
                _ => self.simple_token(operators::Lt, 1),
            }
            ':' => match self.line.get(self.col + 1) {
                Some('=') => self.simple_token(operators::Assignment, 2),
                _ => self.simple_token(tokens::Colon, 1),
            },
            '=' => self.simple_token(operators::Equals, 1),
            '$' => self.literal_hex(),
            '#' => self.literal_int(true),
            '0'...'9' => self.literal_int(false),
            '\'' => self.literal_string(),
            'a'...'z' | '_' | 'A'...'Z' => self.word(),
            _ => Err(self.illegal()),
        };

        Ok(Some(token?))
    }
}

impl<'a> Tokenizer<'a> {
    fn parse_line(&mut self,
                  file_name: &str,
                  line_num: usize,
                  line: &str) -> TokenizeResult<&mut Vec<source::Token>> {
        let mut lexer = Lexer {
            opts: self.opts,
            file: Rc::new(file_name.to_string()),
            line: line.chars().collect(),
            line_num,
            col: 0,
        };

        self.out_buf.clear();
        loop {
            match lexer.next_token()? {
                Some(token) => self.out_buf.push(token),
                None => break,
            }
        }

        Ok(&mut self.out_buf)
    }
}

pub type TokenizeResult<T> = Result<T, IllegalToken>;

pub fn tokenize(file_name: &str,
                source: &str,
                opts: &CompileOptions) -> TokenizeResult<Vec<source::Token>> {
    let lines: Vec<_> = source.replace("\r\n", "\n")
        .split("\n")
        .map(str::to_owned)
        .collect();

    let mut tokenizer = Tokenizer::new(opts);

    let mut tokens = Vec::new();
    for (line_num, line) in lines.into_iter().enumerate() {
        let line_tokens = tokenizer.parse_line(file_name, line_num + 1, &line)?;
        tokens.extend(line_tokens.drain(0..));
    }

    if tokens.len() == 0 {
        return Err(IllegalToken {
            file: file_name.to_string(),
            line: 0,
            col: 0,
            text: "<EOF>".to_string(),
        });
    }

    Ok(tokens)
}

#[cfg(test)]
mod test {
    use tokenizer;
    use tokens::{self, AsToken};
    use consts::IntConstant;
    use keywords;
    use opts::{
        CompileOptions,
        Mode,
    };

    #[test]
    fn tokenizes_literal_string() {
        let s = "  'hello world!'  ";

        let result = tokenizer::tokenize("test", s, &CompileOptions::default()).unwrap();
        assert_eq!(1, result.len());

        assert_eq!(&tokens::LiteralString("hello world!".to_owned()),
                   result[0].token.as_ref());
    }

    #[test]
    fn tokenizes_literal_char() {
        let result = tokenizer::tokenize("test,", "#32", &CompileOptions::default()).unwrap();
        assert_eq!(1, result.len());
        assert_eq!(&tokens::LiteralInteger(IntConstant::Char(32)),
                   result[0].as_token());
    }

    #[test]
    fn tokenizes_keywords_case_insensitive_for_ci_opts() {
        let opts = CompileOptions::new(Mode::Fpc);
        let result = tokenizer::tokenize("test", "True TRUE true begIN END", &opts)
            .unwrap();

        assert_eq!(&tokens::Keyword(keywords::True), result[0].as_token());
        assert_eq!(&tokens::Keyword(keywords::True), result[1].as_token());
        assert_eq!(&tokens::Keyword(keywords::True), result[2].as_token());
        assert_eq!(&tokens::Keyword(keywords::Begin), result[3].as_token());
        assert_eq!(&tokens::Keyword(keywords::End), result[4].as_token());
    }
}