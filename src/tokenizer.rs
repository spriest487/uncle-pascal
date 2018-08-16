use tokens;
use regex;
use std::fmt;

#[derive(Clone, Debug)]
pub struct SourceToken {
    pub token: tokens::Token,

    pub line: usize,
    pub col: usize,
}

#[derive(Clone, Debug)]
pub struct IllegalToken {
    pub text: String,

    pub line: usize,
    pub col: usize,
}

impl fmt::Display for IllegalToken {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "illegal token '{}' @ {}:{}", self.text, self.line, self.col)
    }
}

fn parse_text_token(token_src: &str) -> tokens::Token {
    tokens::Token::Identifier(token_src.to_owned())
}

fn token_patterns() -> Vec<(String, Box<Fn(&str) -> tokens::Token>)> {
    //let simple_token = || |token| token;

    vec![
        (r"[a-zA-Z]((\.?[a-zA-Z0-9_])?)+".to_owned(), Box::from(parse_text_token)),
    ]
}

pub type TokenizeResult<T> = Result<T, IllegalToken>;

fn parse_line(line_num: usize, line: &str) -> TokenizeResult<Vec<SourceToken>> {
    let all_tokens_pattern = token_patterns().into_iter()
        .map(|(pattern, _)| pattern)
        .collect::<Vec<_>>()
        .join("|");
    let all_tokens_and_unrecognized = all_tokens_pattern + "|.";
    let all_tokens_regex = regex::Regex::new(&all_tokens_and_unrecognized)
        .expect("tokens regex must be valid");

    let patterns = token_patterns().into_iter()
        .map(|(pattern, token_parser)| {
            let token_regex = regex::Regex::new(&pattern).expect("token regex must be valid");
            (token_regex, token_parser)
        })
        .collect::<Vec<_>>();

    let tokens : TokenizeResult<Vec<SourceToken>> = all_tokens_regex
        .captures_iter(line).map(|capture| {
            let token_match = capture.get(0).unwrap();
            let token_source = token_match.as_str();
            let col = token_match.start();

            patterns.iter()
                .find(|pattern| pattern.0.is_match(&token_source))
                .map(|pattern| {
                    let token = pattern.1(&token_source);

                    SourceToken {
                        token,
                        col,
                        line: line_num,
                    }
                })
                .ok_or_else(|| IllegalToken {
                    text: token_source.to_owned(),
                    col,
                    line: line_num,
                })
        })
        .collect();

    Ok(tokens?)
}

pub fn tokenize(source: &str) -> TokenizeResult<Vec<SourceToken>> {
    let lines : Vec<String> = source.replace("\r\n", "\n")
        .split("\n")
        .map(str::to_owned)
        .collect();

    let parsed_lines: TokenizeResult<Vec<Vec<SourceToken>>> = lines.into_iter()
        .enumerate()
        .map(|(line_num, line)| parse_line(line_num, &line))
        .collect();

    Ok(parsed_lines?.into_iter()
        .flat_map(|line_tokens| line_tokens)
        .collect())
}
