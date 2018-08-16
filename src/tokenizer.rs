use std::fmt;
use std::rc::*;
use regex;

use tokens;
use operators;
use keywords;
use source;

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
    ParseFn(Box<Fn(&regex::Captures) -> Option<tokens::Token>>),
}

impl TokenMatchParser {
    fn try_parse(&self, token_match: &regex::Captures) -> Option<tokens::Token> {
        match self {
            &TokenMatchParser::Simple(ref token) => Some(token.clone()),
            &TokenMatchParser::ParseFn(ref parse_fn) => parse_fn(token_match),
        }
    }
}

fn parse_literal_string(token_match: &regex::Captures) -> Option<tokens::Token> {
    //group 1 must be the inner content of the string (eg, minus the outer quotes)
    let text = token_match.get(1).unwrap().as_str().to_owned()
        .replace("''", "'");

    Some(tokens::LiteralString(text))
}

fn parse_literal_integer(token_match: &regex::Captures) -> Option<tokens::Token> {
    let int_text = token_match.get(0).unwrap().as_str();

    int_text.parse()
        .map(|int_val| tokens::LiteralInteger(int_val))
        .ok()
}

fn parse_name(token_match: &regex::Captures) -> Option<tokens::Token> {
    let text = token_match.get(0).unwrap().as_str().to_owned();

    Some(keywords::Keyword::try_parse(&text)
        .map(|kw| tokens::Keyword(kw))
        .unwrap_or_else(|| tokens::Identifier(text)))
}

fn token_patterns() -> Vec<(String, TokenMatchParser)> {
    vec![
        (
            r"[a-zA-Z]((\.?[a-zA-Z0-9_])?)+".to_owned(),
            TokenMatchParser::ParseFn(Box::from(parse_name))
        ),
        (
            r"'(([^']|'{2})*)'".to_owned(),
            {
                //anything between two quote marks, with double quote as literal quote mark
                let parse_fn = Box::from(parse_literal_string);
                TokenMatchParser::ParseFn(parse_fn)
            }
        ),
        (
            r"\(".to_owned(),
            TokenMatchParser::Simple(tokens::BracketLeft)),
        (
            r"\)".to_owned(),
            TokenMatchParser::Simple(tokens::BracketRight)),
        (
            r":=".to_owned(),
            {
                let op = tokens::Operator(operators::Assignment);
                TokenMatchParser::Simple(op)
            }
        ),
        (
            r"<>".to_owned(),
            {
                let op = tokens::Operator(operators::NotEquals);
                TokenMatchParser::Simple(op)
            }
        ),
        (
            r":".to_owned(),
            TokenMatchParser::Simple(tokens::Colon)),
        (
            r";".to_owned(),
            TokenMatchParser::Simple(tokens::Semicolon)),
        (
            r",".to_owned(),
            TokenMatchParser::Simple(tokens::Comma)),
        (
            r"\.".to_owned(),
            TokenMatchParser::Simple(tokens::Period)),
        (
            r"\+".to_owned(),
            TokenMatchParser::Simple(tokens::Operator(operators::Plus))),
        (
            r"\-".to_owned(),
            TokenMatchParser::Simple(tokens::Operator(operators::Minus))),
        (
            "=".to_owned(),
            TokenMatchParser::Simple(tokens::Operator(operators::Equals))),
        (
            "\\^".to_owned(),
            TokenMatchParser::Simple(tokens::Operator(operators::Deref))
            ),
        (
            r"[0-9]+".to_owned(),
            {
                let parse_fn = Box::from(parse_literal_integer);
                TokenMatchParser::ParseFn(parse_fn)
            }
        ),
    ]
}

pub type TokenizeResult<T> = Result<T, IllegalToken>;

fn parse_line(file_name: &str,
              line_num: usize,
              line: &str) -> TokenizeResult<Vec<source::Token>> {
    /* remove line comments before tokenization */
    let line_without_comment = match line.find("//") {
        Some(comment_pos) => line.chars().take(comment_pos).collect(),
        None => line.to_owned(),
    };

    /* make a combined regex of all the possible token match patterns */
    let all_tokens_pattern = token_patterns().into_iter()
        .map(|(pattern, _)| pattern)
        .collect::<Vec<_>>()
        .join("|");
    let all_tokens_and_unrecognized = all_tokens_pattern + "|\\s+|.";
    let all_tokens_regex = regex::Regex::new(&all_tokens_and_unrecognized)
        .expect("tokens regex must be valid");

    /* compile token pattern regexes */
    let patterns = token_patterns().into_iter()
        .map(|(pattern, token_parser)| {
            let whole_token_pattern = format!("^{}$", pattern);
            let token_regex = regex::Regex::new(&whole_token_pattern)
                .expect("token regex must be valid");

            (token_regex, token_parser)
        })
        .collect::<Vec<_>>();

    let file_name_shared = Rc::new(String::from(file_name));

    let parsed_tokens = all_tokens_regex.captures_iter(&line_without_comment)
        .filter_map(|capture| {
            let token_match = capture.get(0).unwrap();
            let token_source = token_match.as_str().trim();
            let col = token_match.start() + 1;

            if token_source.len() == 0 {
                /* token is purely whitespace, ignore it */
                None
            } else {
                let first_matching_token = patterns.iter()
                    .find(|pattern| {
                        pattern.0.is_match(&token_source)
                    })
                    .and_then(|pattern| {
                        //match against this regex only to get the expected groups
                        let pattern_captures = pattern.0.captures(&token_source)
                            .unwrap();

                        /* a pattern matched this token, use the parsing
                        function associated with this pattern */
                        pattern.1.try_parse(&pattern_captures)
                    })
                    .map(|token| source::Token {
                        token: Rc::from(token),
                        location: source::Location {
                            file: file_name_shared.clone(),
                            line: line_num,
                            col,
                        },
                    })
                    .ok_or_else(|| IllegalToken {
                        text: token_source.to_owned(),
                        file: String::from(file_name),
                        col,
                        line: line_num,
                    });

                Some(first_matching_token)
            }
        })
        .collect::<TokenizeResult<_>>()?;

    Ok(parsed_tokens)
}

pub fn tokenize(file_name: &str,
                source: &str) -> TokenizeResult<Vec<source::Token>> {
    let lines: Vec<_> = source.replace("\r\n", "\n")
        .split("\n")
        .map(str::to_owned)
        .collect();

    let parsed_lines: TokenizeResult<Vec<_>> = lines.into_iter()
        .enumerate()
        .map(|(line_num, line)| parse_line(file_name, line_num + 1, &line))
        .collect();

    Ok(parsed_lines?.into_iter()
        .flat_map(|line_tokens| line_tokens)
        .collect())
}

#[cfg(test)]
mod test {
    use tokenizer;
    use tokens;

    #[test]
    fn tokenizes_literal_string() {
        let s = "  'hello world!'  ";

        let result = tokenizer::tokenize("test", s).unwrap();
        assert_eq!(1, result.len());

        assert_eq!(&tokens::LiteralString("hello world!".to_owned()),
                   result[0].token.as_ref());
    }
}