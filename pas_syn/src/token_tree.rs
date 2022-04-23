mod lex;

use crate::parse::TokenStream;
use crate::{
    consts::{IntConstant, RealConstant},
    ident::Ident,
    keyword::Keyword,
    operators::Operator,
};
use pas_common::{span::*, BuildOptions, DiagnosticLabel, DiagnosticOutput, TracedError};
use std::rc::Rc;
use std::{
    fmt::{self, Write as _},
    path::PathBuf,
};

#[derive(Eq, PartialEq, Copy, Clone, Debug, Hash)]
pub enum DelimiterPair {
    BeginEnd,
    CaseEnd,
    MatchEnd,
    Bracket,
    SquareBracket,
}

impl DelimiterPair {
    pub fn tokens(self) -> (&'static str, &'static str) {
        match self {
            DelimiterPair::BeginEnd => ("begin", "end"),
            DelimiterPair::CaseEnd => ("case", "end"),
            DelimiterPair::MatchEnd => ("match", "end"),
            DelimiterPair::Bracket => ("(", ")"),
            DelimiterPair::SquareBracket => ("[", "]"),
        }
    }
}

impl fmt::Display for DelimiterPair {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                DelimiterPair::BeginEnd => "begin/end",
                DelimiterPair::CaseEnd => "case/end",
                DelimiterPair::MatchEnd => "match/end",
                DelimiterPair::SquareBracket => "[]",
                DelimiterPair::Bracket => "()",
            }
        )
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
        write!(
            f,
            "{}",
            match self {
                Separator::Colon => ':',
                Separator::Comma => ',',
                Separator::Semicolon => ';',
            }
        )
    }
}

#[derive(Eq, PartialEq, Clone, Debug, Hash)]
pub struct DelimitedGroup {
    pub delim: DelimiterPair,

    pub open: Span,
    pub close: Span,

    pub inner: Vec<TokenTree>,

    pub span: Span,
}

impl DelimitedGroup {
    pub fn to_inner_tokens(self) -> TokenStream {
        TokenStream::new(self.inner, self.open)
    }
}

#[derive(Eq, PartialEq, Clone, Debug, Hash)]
pub enum TokenTree {
    Ident(Ident),
    IntNumber { value: IntConstant, span: Span },
    RealNumber { value: RealConstant, span: Span },
    String { value: Rc<String>, span: Span },
    Keyword { kw: Keyword, span: Span },
    Operator { op: Operator, span: Span },
    Separator { sep: Separator, span: Span },
    Delimited(DelimitedGroup),
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
            TokenTree::Delimited(DelimitedGroup { span, .. }) => span,
        }
    }

    pub fn into_span(self) -> Span {
        match self {
            TokenTree::Ident(ident) => ident.span,
            TokenTree::IntNumber { span, .. } => span,
            TokenTree::RealNumber { span, .. } => span,
            TokenTree::String { span, .. } => span,
            TokenTree::Keyword { span, .. } => span,
            TokenTree::Operator { span, .. } => span,
            TokenTree::Separator { span, .. } => span,
            TokenTree::Delimited(DelimitedGroup { span, .. }) => span,
        }
    }

    pub fn is_ident(&self, word: &str) -> bool {
        match self {
            TokenTree::Ident(ident) => *ident.name == word,
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
            TokenTree::Delimited(DelimitedGroup {
                delim: token_delim, ..
            }) => *token_delim == delim,
            _ => false,
        }
    }

    pub fn is_separator(&self, sep: Separator) -> bool {
        match self {
            TokenTree::Separator { sep: token_sep, .. } => *token_sep == sep,
            _ => false,
        }
    }

    pub fn is_operator(&self, op: Operator) -> bool {
        match self {
            TokenTree::Operator { op: token_op, .. } => *token_op == op,
            _ => false,
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

    fn fmt_indented(&self, f: &mut fmt::Formatter, indent: usize) -> fmt::Result {
        fn write_indent(f: &mut fmt::Formatter, indent: usize) -> fmt::Result {
            const INDENT_SIZE: usize = 2;
            for _ in 0..indent * INDENT_SIZE {
                f.write_char(' ')?;
            }
            Ok(())
        }

        write_indent(f, indent)?;

        match self {
            TokenTree::Keyword { kw, .. } => write!(f, "keyword `{}`", kw)?,
            TokenTree::Ident(ident) => write!(f, "identifier `{}`", ident)?,
            TokenTree::Operator { op, .. } => write!(f, "operator `{}`", op)?,
            TokenTree::Separator { sep, .. } => write!(f, "separator `{}`", sep)?,

            TokenTree::RealNumber { value, .. } => write!(f, "real number `{}`", value)?,
            TokenTree::IntNumber { value, .. } => write!(f, "integer number `{}`", value)?,
            TokenTree::String { value, .. } => write!(f, "string '{}'", value)?,

            TokenTree::Delimited(DelimitedGroup { delim, .. }) => {
                write!(f, "{}-delimited group", delim)?;
            },
        }

        Ok(())
    }
}

impl fmt::Display for TokenTree {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.fmt_indented(f, 0)
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
            TokenizeError::IllegalToken(..) => write!(f, "Illegal token"),

            TokenizeError::UnmatchedDelimiter { .. } => write!(f, "unmatched delimiter"),

            TokenizeError::UnexpectedCloseDelimited { .. } => {
                write!(f, "unexpected close delimiter")
            },
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

impl DiagnosticOutput for TokenizeError {
    fn label(&self) -> Option<DiagnosticLabel> {
        match self {
            TokenizeError::IllegalToken(span) => Some(DiagnosticLabel {
                span: span.clone(),
                text: None,
            }),

            TokenizeError::UnmatchedDelimiter {
                delim, to_match, ..
            } => Some(DiagnosticLabel {
                span: to_match.clone(),
                text: {
                    let (open, close) = delim.tokens();
                    Some(format!(
                        "opening `{}` is not followed by a closing `{}`",
                        open, close
                    ))
                },
            }),

            TokenizeError::UnexpectedCloseDelimited { delim, span } => Some(DiagnosticLabel {
                span: span.clone(),
                text: {
                    let (open, close) = delim.tokens();
                    Some(format!(
                        "closing `{}` was not expected here (no opening `{}`)",
                        close, open
                    ))
                },
            }),
        }
    }
}

pub type TokenizeResult<T> = Result<T, TracedError<TokenizeError>>;

impl TokenTree {
    pub fn tokenize(
        filename: impl Into<PathBuf>,
        source: &str,
        opts: &BuildOptions,
    ) -> TokenizeResult<Vec<Self>> {
        lex::lex(filename, source, opts)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use std::rc::Rc;

    fn tokenize(s: &str, case_sensitive: bool) -> Vec<TokenTree> {
        let mut opts = BuildOptions::default();
        opts.case_sensitive = case_sensitive;

        match TokenTree::tokenize("test", s, &opts) {
            Ok(result) => result,
            Err(err) => {
                panic!("{} @ {:#?}", err.err, err.bt);
            },
        }
    }

    #[test]
    fn tokenizes_literal_string() {
        let result = tokenize("  'hello world!'  ", true);

        assert_eq!(1, result.len());

        match &result[0] {
            TokenTree::String { value, .. } => assert_eq!("hello world!", value.as_str()),
            _ => panic!("got {:#?}, expected a string", result),
        }
    }

    #[test]
    fn tokenizes_literal_char() {
        let result = tokenize(" #32 ", true);
        assert_eq!(1, result.len());

        match result[0] {
            TokenTree::IntNumber { value, .. } => assert_eq!(IntConstant::from(32), value),
            _ => panic!("got {:#?}, expected a char literal", result),
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

        assert_eq!(
            expected_kws.len(),
            result.len(),
            "expected 5 keywords, found {:#?}",
            result
        );

        for i in 0..expected_kws.len() {
            let kw = match result[i] {
                TokenTree::Keyword { kw, .. } => kw,
                _ => panic!(
                    "expected result to be a {} keyword token, was {:#?}",
                    expected_kws[i], result
                ),
            };

            assert_eq!(expected_kws[i], kw);
        }
    }

    #[test]
    fn tokenizes_bracket_delim() {
        let result = tokenize("(a)", false);

        match &result[0] {
            TokenTree::Delimited {
                delim: DelimiterPair::Bracket,
                inner,
                ..
            } => {
                assert_eq!(1, inner.len());
                match &inner[0] {
                    TokenTree::Ident(ident) => assert_eq!("a", ident.name.as_str()),
                    _ => panic!("expected ident `a`, found {:#?}", inner[0]),
                }
            },

            _ => panic!("expected bracket-delimited group, got {:#?}", result),
        }
    }

    #[test]
    fn tokenizes_mixed_delim() {
        let result = tokenize("(begin a end)", false);

        match &result[0] {
            TokenTree::Delimited {
                delim: DelimiterPair::Bracket,
                inner,
                ..
            } => {
                assert_eq!(1, inner.len(), "expected one inner token, got {:#?}", inner);
                match &inner[0] {
                    TokenTree::Delimited {
                        delim: DelimiterPair::BeginEnd,
                        inner,
                        ..
                    } => match &inner[0] {
                        TokenTree::Ident(ident) => assert_eq!("a", ident.name.as_str()),
                        _ => panic!("expected ident `a`, found {:#?}", inner[0]),
                    },
                    _ => panic!("expected begin/end delimited tree, found {:#?}", inner[0]),
                }
            },

            _ => panic!("expected bracket-delimited group, got {:#?}", result),
        }
    }

    fn test_span(from: (usize, usize), to: (usize, usize)) -> Span {
        Span {
            file: Rc::new(PathBuf::from("test")),
            start: Location {
                line: from.0,
                col: from.1,
            },
            end: Location {
                line: to.0,
                col: to.1,
            },
        }
    }

    #[test]
    fn begin_end_delim_has_correct_spans() {
        let result = tokenize(
            r"begin
1 2 3
end",
            false,
        );
        match &result[0] {
            TokenTree::Delimited {
                delim: DelimiterPair::BeginEnd,
                inner,
                open,
                close,
                span,
            } => {
                assert_eq!(&test_span((0, 0), (0, 4)), open, "span of open token");
                assert_eq!(&test_span((2, 0), (2, 2)), close, "span of close token");
                assert_eq!(&test_span((0, 0), (2, 2)), span, "total span");
                assert_eq!(3, inner.len());
            },

            _ => panic!("expectefd begin/end delim group, got {:?}", result[0]),
        }
    }
}
