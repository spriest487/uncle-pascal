use std::{
    fmt,
    rc::*,
};
use regex::{
    Regex,
    Captures,
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

enum TokenMatchParser {
    Simple(tokens::Token),
    ParseFn(Box<Fn(&Captures, &CompileOptions) -> Option<tokens::Token>>),
}

impl TokenMatchParser {
    fn try_parse(&self, token_match: &Captures, opts: &CompileOptions) -> Option<tokens::Token> {
        match self {
            &TokenMatchParser::Simple(ref token) => Some(token.clone()),
            &TokenMatchParser::ParseFn(ref parse_fn) => parse_fn(token_match, opts),
        }
    }
}

struct Tokenizer<'a> {
    token_matchers: Vec<(Regex, TokenMatchParser)>,
    opts: &'a CompileOptions,

    out_buf: Vec<source::Token>,
}

impl<'a> Tokenizer<'a> {
    pub fn new(opts: &'a CompileOptions) -> Self {
        let token_patterns = vec![
            func_pattern(r"[a-zA-Z_](([a-zA-Z0-9_])?)+", parse_name),
            //anything between two quote marks, with double quote as literal quote mark
            func_pattern(r"'(([^']|'{2})*)'", parse_literal_string),
            func_pattern(r"-?[0-9]+", parse_literal_integer), // decimal int
            func_pattern(r"#[0-9]{1,3}", parse_literal_integer), // char
            func_pattern(r"\$([0-9A-Fa-f]+)", parse_literal_integer), //hex int
            simple_pattern(r"\*", operators::Multiply),
            simple_pattern(r"[/]", operators::Divide),
            simple_pattern(r"\(", tokens::BracketLeft),
            simple_pattern(r"\)", tokens::BracketRight),
            simple_pattern(r"\[", tokens::SquareBracketLeft),
            simple_pattern(r"\]", tokens::SquareBracketRight),
            simple_pattern(r":=", operators::Assignment),
            simple_pattern(r">=", operators::Gte),
            simple_pattern(r">", operators::Gt),
            simple_pattern(r"<>", operators::NotEquals),
            simple_pattern(r"<=", operators::Lte),
            simple_pattern(r"<", operators::Lt),
            simple_pattern(r"@", operators::AddressOf),
            simple_pattern(r"\+", operators::Plus),
            simple_pattern(r"\-", operators::Minus),
            simple_pattern("=", operators::Equals),
            simple_pattern(r"\^", operators::Deref),
            simple_pattern(r":", tokens::Colon),
            simple_pattern(r";", tokens::Semicolon),
            simple_pattern(r",", tokens::Comma),
            simple_pattern(r"\.", tokens::Period),
        ];

        /* compile token pattern regexes */
        let token_matchers = token_patterns.into_iter()
            .map(|(pattern, token_parser)| {
                let whole_token_pattern = if opts.case_sensitive() {
                    pattern
                } else {
                    format!("(?i){}", pattern)
                };

                let token_regex = Regex::new(&whole_token_pattern)
                    .expect("token regex must be valid");

                (token_regex, token_parser)
            })
            .collect::<Vec<_>>();

        Tokenizer {
            token_matchers,
            opts,

            out_buf: Vec::with_capacity(64),
        }
    }
}

impl<'a> Tokenizer<'a> {
    fn parse_line(&mut self,
                  file_name: &str,
                  line_num: usize,
                  line: &str) -> TokenizeResult<&mut Vec<source::Token>> {
        let file_name = Rc::new(file_name.to_string());
        self.out_buf.clear();

        let mut col_it: Box<Iterator<Item=(usize, char)>> = Box::new(line.chars()
            .enumerate()
            .skip(0)
            .filter(|(_, col_char)| !col_char.is_whitespace()));

        loop {
            let (col, _) = match col_it.next() {
                Some(next @ _) => next,
                None => break,
            };

            let token = self.token_matchers.iter()
                .filter_map(|(pattern, token_matcher)| {
                    let captures = pattern.captures(&line[col..])?;
                    let token_match = captures.get(0)?;

                    if token_match.start() == 0 {
                        let matched = token_matcher.try_parse(&captures, self.opts)?;
                        col_it = Box::new(line.chars()
                            .enumerate()
                            .skip(col + token_match.end())
                            .filter(|(_, col_char)| !col_char.is_whitespace()));

                        Some(matched)
                    } else {
                        None
                    }
                })
                .next()
                .map(|token| source::Token::new(token, source::Location {
                    file: file_name.clone(),
                    line: line_num,
                    col,
                }))
                .ok_or_else(|| IllegalToken {
                    text: line[col..].to_string(),
                    file: file_name.to_string(),
                    col,
                    line: line_num,
                })?;

            self.out_buf.push(token);
        }

        Ok(&mut self.out_buf)
    }
}

fn parse_literal_string(token_match: &Captures, _opts: &CompileOptions) -> Option<tokens::Token> {
    //group 1 must be the inner content of the string (eg, minus the outer quotes)
    let text = token_match.get(1).unwrap().as_str().to_owned()
        .replace("''", "'");

    Some(tokens::LiteralString(text))
}

fn parse_literal_integer(token_match: &Captures,
                         _opts: &CompileOptions)
                         -> Option<tokens::Token> {
    let int_text = token_match.get(0).unwrap().as_str();

    let val = IntConstant::parse_str(int_text)?;

    Some(tokens::LiteralInteger(val))
}

fn parse_name(token_match: &Captures, opts: &CompileOptions) -> Option<tokens::Token> {
    let text = token_match.get(0).unwrap().as_str().to_owned();

    Some(keywords::Keyword::try_parse(&text, opts)
        .map(|kw| tokens::Keyword(kw))
        .or_else(|| operators::Operator::try_parse_text(&text, opts)
            .map(|op| tokens::Operator(op)))
        .unwrap_or_else(|| tokens::Identifier(text)))
}

fn simple_pattern<T>(pattern: &str, token: T) -> (String, TokenMatchParser)
    where T: Into<tokens::Token>
{
    (pattern.to_owned(), TokenMatchParser::Simple(token.into()))
}

fn func_pattern<TFn>(pattern: &str, f: TFn) -> (String, TokenMatchParser)
    where TFn: Fn(&Captures, &CompileOptions) -> Option<tokens::Token> + 'static
{
    (pattern.to_owned(), TokenMatchParser::ParseFn(Box::new(f)))
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