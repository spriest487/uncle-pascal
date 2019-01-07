use {
    crate::{
        consts::{
            FloatConstant,
            IntConstant,
        },
        ident::Ident,
        keyword::Keyword,
        operators::Operator,
        span::{Span, SpanDisplay},
        TracedError,
    },
    std::{
        path::PathBuf,
        fmt,
    },
};

mod lex;

#[derive(Eq, PartialEq, Copy, Clone, Debug, Hash)]
pub enum DelimiterPair {
    BeginEnd,
    Bracket,
    SquareBracket,
}

#[derive(Eq, PartialEq, Copy, Clone, Debug, Hash)]
pub enum Separator {
    Semicolon,
    Comma,
    Colon,
}

#[derive(Eq, PartialEq, Clone, Debug, Hash)]
pub enum TokenTree {
    Ident(Ident),
    IntNumber {
        value: IntConstant,
        span: Span,
    },
    RealNumber {
        value: FloatConstant,
        span: Span,
    },
    String {
        value: String,
        span: Span,
    },
    Keyword {
        kw: Keyword,
        span: Span,
    },
    Operator {
        op: Operator,
        span: Span,
    },
    Separator {
        sep: Separator,
        span: Span,
    },
    Delimited {
        delim: DelimiterPair,
        inner: Vec<TokenTree>,
        span: Span,
    },
}

#[derive(Debug)]
pub enum TokenizeError {
    IllegalToken(Span),
    UnmatchedDelimiter {
        delim: DelimiterPair,
        to_match: Span,
        span: Span,
    },
    UnexpectedCloseDelimited {
        delim: DelimiterPair,
        span: Span,
    },
}

impl fmt::Display for TokenizeError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TokenizeError::IllegalToken(_) => write!(f, "Illegal token"),

            TokenizeError::UnmatchedDelimiter { delim, to_match, .. } => {
                write!(f, "unmatched {:?} delimiter from {}", delim, to_match)
            }

            TokenizeError::UnexpectedCloseDelimited { delim, .. } => {
                write!(f, "unexpected {:?} close delimiter", delim)
            }
        }
    }
}

impl SpanDisplay for TokenizeError {
    fn span(&self) -> &Span {
        match self {
            TokenizeError::IllegalToken(span) => span,
            TokenizeError::UnmatchedDelimiter { span, .. } => span,
            TokenizeError::UnexpectedCloseDelimited { span, .. } => span,
        }
    }
}

pub type TokenizeResult<T> = Result<T, TracedError<TokenizeError>>;

impl TokenTree {
    pub fn tokenize(filename: impl Into<PathBuf>,
        source: &str,
        case_sensitive: bool) -> TokenizeResult<Vec<Self>> {
        lex::lex(filename, source, case_sensitive)
    }
}

#[cfg(test)]
mod test {
    use {
        super::*,
    };

    fn tokenize(s: &str, case_sensitive: bool) -> Vec<TokenTree> {
        match TokenTree::tokenize("test", s, case_sensitive) {
            Ok(result) => result,
            Err(err) => {
                err.print_context(s);
                panic!("{:#?}", err.bt);
            }
        }
    }

    #[test]
    fn tokenizes_literal_string() {
        let result = tokenize("  'hello world!'  ", true);

        assert_eq!(1, result.len());

        match &result[0] {
            TokenTree::String { value, .. } => assert_eq!("hello world!", value),
            _ => panic!("got {:#?}, expected a string", result)
        }
    }

    #[test]
    fn tokenizes_literal_char() {
        let result = tokenize(" #32 ", true);
        assert_eq!(1, result.len());

        match result[0] {
            TokenTree::IntNumber { value, .. } => assert_eq!(IntConstant::Char(32), value),
            _ => panic!("got {:#?}, expected a char literal", result)
        }
    }

    #[test]
    fn tokenizes_keywords_case_insensitive_mode() {
        let result = tokenize("  True TRUE true FALSE false", false);

        let expected_kws = [
            Keyword::True,
            Keyword::True,
            Keyword::True,
            Keyword::False,
            Keyword::False,
        ];

        assert_eq!(expected_kws.len(), result.len(), "expected 5 keywords, found {:#?}", result);

        for i in 0..expected_kws.len() {
            let kw = match result[i] {
                TokenTree::Keyword { kw, .. } => kw,
                _ => panic!("expected result to be a {} keyword token, was {:#?}", expected_kws[i], result),
            };

            assert_eq!(expected_kws[i], kw);
        }
    }

    #[test]
    fn tokenizes_bracket_delim() {
        let result = tokenize("(a)", false);

        match &result[0] {
            TokenTree::Delimited { delim: DelimiterPair::Bracket, inner, .. } => {
                assert_eq!(1, inner.len());
                match &inner[0] {
                    TokenTree::Ident(ident) => assert_eq!("a", ident.name),
                    _ => panic!("expected ident `a`, found {:#?}", inner[0]),
                }
            }

            _ => panic!("expected bracket-delimited group, got {:#?}", result),
        }
    }

    #[test]
    fn tokenizes_mixed_delim() {
        let result = tokenize("(begin a end)", false);

        match &result[0] {
            TokenTree::Delimited { delim: DelimiterPair::Bracket, inner, .. } => {
                assert_eq!(1, inner.len());
                match &inner[0] {
                    TokenTree::Delimited { delim: DelimiterPair::BeginEnd, inner, .. }  => {
                        match &inner[0] {
                            TokenTree::Ident(ident) => assert_eq!("a", ident.name),
                            _ => panic!("expected ident `a`, found {:#?}", inner[0]),
                        }
                    },
                    _ => panic!("expected begin/end delimited tree, found {:#?}", inner[0]),
                }
            }

            _ => panic!("expected bracket-delimited group, got {:#?}", result),
        }
    }
}