use {
    crate::{
        consts::{
            RealConstant,
            IntConstant,
        },
        ident::Ident,
        keyword::Keyword,
        operators::Operator,
    },
    pas_common::{
        TracedError,
        BuildOptions,
        span::*,
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

impl DelimiterPair {
    pub fn tokens(&self) -> (&str, &str) {
        match self {
            DelimiterPair::BeginEnd => ("begin", "end"),
            DelimiterPair::Bracket => ("(", ")"),
            DelimiterPair::SquareBracket => ("[", "]"),
        }
    }
}

impl fmt::Display for DelimiterPair {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", match self {
            DelimiterPair::BeginEnd => "begin/end",
            DelimiterPair::SquareBracket => "[]",
            DelimiterPair::Bracket => "()",
        })
    }
}

#[derive(Eq, PartialEq, Copy, Clone, Debug, Hash)]
pub enum Separator {
    Semicolon,
    Comma,
    Colon,
}

impl fmt::Display for Separator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", match self {
            Separator::Colon => ':',
            Separator::Comma => ',',
            Separator::Semicolon => ';',
        })
    }
}

#[derive(Eq, PartialEq, Clone, Debug, Hash)]
pub enum TokenTree {
    Ident(Ident),
    IntNumber {
        value: IntConstant,
        span: Span,
    },
    RealNumber {
        value: RealConstant,
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
        open: Span,
        close: Span,
        span: Span,
    },
}

impl TokenTree {
    pub fn span(&self) -> &Span {
        match self {
            TokenTree::Ident(ident) => &ident.span,
            TokenTree::IntNumber { span, .. } => span,
            TokenTree::RealNumber { span, .. } => span,
            TokenTree::String { span, .. } => span,
            TokenTree::Keyword { span, .. } => span,
            TokenTree::Operator { span, .. } => span,
            TokenTree::Separator { span, .. } => span,
            TokenTree::Delimited { span, .. } => span,
        }
    }

    pub fn is_ident(&self, word: &str) -> bool {
        match self {
            TokenTree::Ident(ident) => ident.name == word,
            _ => false,
        }
    }

    pub fn as_ident(&self) -> Option<&Ident> {
        match self {
            TokenTree::Ident(ident) => Some(ident),
            _ => None,
        }
    }

    pub fn into_ident(self) -> Option<Ident> {
        match self {
            TokenTree::Ident(ident) => Some(ident),
            _ => None,
        }
    }

    pub fn is_keyword(&self, kw: Keyword) -> bool {
        match self {
            TokenTree::Keyword { kw: token_kw, .. } => *token_kw == kw,
            _ => false,
        }
    }

    pub fn as_keyword(&self) -> Option<Keyword> {
        match self {
            TokenTree::Keyword { kw, .. } => Some(*kw),
            _ => None,
        }
    }

    pub fn is_delimited(&self, delim: DelimiterPair) -> bool {
        match self {
            TokenTree::Delimited { delim: token_delim, .. } => *token_delim == delim,
            _ => false
        }
    }

    pub fn is_separator(&self, sep: Separator) -> bool {
        match self {
            TokenTree::Separator { sep: token_sep, .. } => *token_sep == sep,
            _ => false
        }
    }

    pub fn is_operator(&self, op: Operator) -> bool {
        match self {
            TokenTree::Operator { op: token_op, .. } => *token_op == op,
            _ => false
        }
    }

    pub fn as_operator(&self) -> Option<Operator> {
        match self {
            TokenTree::Operator { op, .. } => Some(*op),
            _ => None,
        }
    }

    pub fn as_literal_string(&self) -> Option<&str> {
        match self {
            TokenTree::String { value, .. } => Some(value),
            _ => None,
        }
    }

    pub fn as_literal_int(&self) -> Option<&IntConstant> {
        match self {
            TokenTree::IntNumber { value, .. } => Some(value),
            _ => None,
        }
    }

    pub fn as_literal_real(&self) -> Option<&RealConstant> {
        match self {
            TokenTree::RealNumber { value, .. } => Some(value),
            _ => None,
        }
    }
}

impl fmt::Display for TokenTree {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TokenTree::Keyword { kw, .. } => write!(f, "keyword `{}`", kw),
            TokenTree::Ident(ident) => write!(f, "identifier `{}`", ident),
            TokenTree::Operator { op, .. } => write!(f, "operator `{}`", op),
            TokenTree::Separator { sep, .. } => write!(f, "separator `{}`", sep),

            TokenTree::RealNumber { value, .. } => write!(f, "real number `{}`", value),
            TokenTree::IntNumber { value, .. } => write!(f, "integer number `{}`", value),
            TokenTree::String { value, .. } => write!(f, "string '{}'", value),

            TokenTree::Delimited { delim, inner, .. } => {
                let (open, close) = delim.tokens();
                write!(f, "{} ", open)?;
                for inner_token in inner {
                    write!(f, "{} ", inner_token)?;
                }
                write!(f, "{}", close)
            },
        }
    }
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

impl Spanned for TokenizeError {
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
                    opts: &BuildOptions) -> TokenizeResult<Vec<Self>> {
        lex::lex(filename, source, opts)
    }
}

#[cfg(test)]
mod test {
    use {
        std::rc::Rc,
        pas_common::span::*,
        super::*,
    };

    fn tokenize(s: &str, case_sensitive: bool) -> Vec<TokenTree> {
        let opts = BuildOptions { case_sensitive };

        match TokenTree::tokenize("test", s, &opts) {
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
                assert_eq!(1, inner.len(), "expected one inner token, got {:#?}", inner);
                match &inner[0] {
                    TokenTree::Delimited { delim: DelimiterPair::BeginEnd, inner, .. } => {
                        match &inner[0] {
                            TokenTree::Ident(ident) => assert_eq!("a", ident.name),
                            _ => panic!("expected ident `a`, found {:#?}", inner[0]),
                        }
                    }
                    _ => panic!("expected begin/end delimited tree, found {:#?}", inner[0]),
                }
            }

            _ => panic!("expected bracket-delimited group, got {:#?}", result),
        }
    }

    fn test_span(from: (usize, usize), to: (usize, usize)) -> Span {
        Span {
            file: Rc::new(PathBuf::from("test")),
            start: Location { line: from.0, col: from.1, },
            end: Location { line: to.0, col: to.1, },
        }
    }

    #[test]
    fn begin_end_delim_has_correct_spans() {
        let result = tokenize(r"begin
1 2 3
end", false);
        match &result[0] {
            TokenTree::Delimited { delim: DelimiterPair::BeginEnd, inner, open, close, span } => {
                assert_eq!(&test_span((0, 0), (0, 4)), open, "span of open token");
                assert_eq!(&test_span((2, 0), (2, 2)), close, "span of close token");
                assert_eq!(&test_span((0, 0), (2, 2)), span, "total span");
                assert_eq!(3, inner.len());
            }

            _ => panic!("expectefd begin/end delim group, got {:?}", result[0]),
        }
    }
}