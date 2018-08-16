use std::fmt;

use keywords;
use tokens;
use tokenizer;

pub enum ParseError {
    UnexpectedToken(tokenizer::SourceToken, Option<TokenMatcher>),
    UnbalancedPair(TokenMatcher, tokenizer::SourceToken),
    UnterminatedBlock,
    UnexpectedEOF,
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &ParseError::UnexpectedToken(ref source_token, ref expected) => {
                write!(f, "unexpected token: {}", source_token)?;

                expected.as_ref()
                    .map(|matcher| write!(f, " (expected: {})", matcher))
                    .unwrap_or(Ok(()))
            },
            &ParseError::UnbalancedPair(ref matcher, ref source_token) =>
                write!(f, "unbalanced pair: expected {} after {}", matcher, source_token),
            &ParseError::UnterminatedBlock =>
                write!(f, "unterminated block"),
            &ParseError::UnexpectedEOF =>
                write!(f, "unexpected end of input"),
        }
    }
}

pub struct ParseOutput<TValue> {
    value: TValue,
    next: Box<Iterator<Item=tokenizer::SourceToken>>,
}

impl<TValue> ParseOutput<TValue> {
    pub fn new<TNext>(value: TValue, next: TNext) -> Self
        where TNext: Iterator<Item=tokenizer::SourceToken> + 'static
    {
        Self {
            value,
            next: Box::from(next),
        }
    }

    pub fn value(&self) -> &TValue {
        &self.value
    }

    pub fn into_next(self) -> Box<Iterator<Item=tokenizer::SourceToken>> {
        self.next
    }

    pub fn finish(mut self) -> Result<TValue, ParseError> {
        let unexpected = self.next.next();
        match unexpected {
            Some(token) => Err(ParseError::UnexpectedToken(token, None)),
            None => Ok(self.value)
        }
    }
}

type ParseResult<T> = Result<ParseOutput<T>, ParseError>;

#[derive(Clone)]
pub enum TokenMatcher {
    Keyword(keywords::Keyword),
    AnyKeyword,
    AnyIdentifier,
    AnyBinaryOperator,
    AnyLiteralInteger,
    AnyLiteralString,
    Exact(tokens::Token),
    OneOf(Vec<Box<TokenMatcher>>),
}

impl fmt::Display for TokenMatcher {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &TokenMatcher::Keyword(kw) => write!(f, "keyword {}", kw),
            &TokenMatcher::AnyKeyword => write!(f, "keyword"),
            &TokenMatcher::AnyIdentifier => write!(f, "identifier"),
            &TokenMatcher::AnyBinaryOperator => write!(f, "binary operator"),
            &TokenMatcher::AnyLiteralInteger => write!(f, "integer literal"),
            &TokenMatcher::AnyLiteralString => write!(f, "string literal"),
            &TokenMatcher::Exact(ref exact_token) => write!(f, "{}", exact_token),
            &TokenMatcher::OneOf(ref matchers) => write!(f, "one of: {}", matchers.iter()
                .map(|matcher| format!("{}", matcher))
                .collect::<Vec<_>>()
                .join(", ")),
        }
    }
}

impl TokenMatcher {
    pub fn match_token(&self, token: &tokens::Token) -> bool {
        match self {
            &TokenMatcher::Keyword(kw) => token.is_keyword(kw),
            &TokenMatcher::AnyKeyword => token.is_any_keyword(),
            &TokenMatcher::AnyIdentifier => token.is_any_identifier(),
            &TokenMatcher::AnyBinaryOperator => token.is_any_binary_operator(),
            &TokenMatcher::AnyLiteralInteger => token.is_any_literal_int(),
            &TokenMatcher::AnyLiteralString => token.is_any_literal_string(),
            &TokenMatcher::Exact(ref exact_token) => token == exact_token,
            &TokenMatcher::OneOf(ref matchers) => matchers.iter()
                .any(|matcher| matcher.match_token(token)),
        }
    }
}

pub fn match_sequence<I>(matchers: &[TokenMatcher],
                         in_tokens: I) -> ParseResult<Vec<tokenizer::SourceToken>>
    where
        I: Iterator<Item=tokenizer::SourceToken>
{
    let tokens: Vec<_> = in_tokens.collect();

    let expected_len = matchers.len();

    let matches: Vec<_> = tokens.iter()
        .zip(matchers)
        .map(|(token, matcher)| (token.clone(), matcher.clone(), matcher.match_token(&token.token)))
        .collect();

    if matches.len() < expected_len {
        Err(ParseError::UnexpectedEOF)
    } else {
        let first_fail = matches.iter()
            .find(|&&(_, _, matched)| !matched)
            .cloned();

        match first_fail {
            Some((token, matcher, _)) =>
                Err(ParseError::UnexpectedToken(token, Some(matcher))),
            None => {
                let matched_tokens = matches.into_iter()
                    .map(|(token, _, _)| token.clone())
                    .collect::<Vec<_>>();

                let remaining_tokens = tokens.into_iter()
                    .skip(matched_tokens.len());

                Ok(ParseOutput::new(matched_tokens, remaining_tokens))
            },
        }
    }
}

pub mod program {
    use syntax;
    use types;
    use tokenizer;
    use keywords;

    mod function {
        #[derive(Clone, Debug)]
        pub struct Function {

        }
    }

    mod type_decl {
        use types;

        #[derive(Clone, Debug)]
        pub struct VarDecl {
            name: types::Identifier,
            decl_type: types::Identifier,
        }

        #[derive(Clone, Debug)]
        pub struct RecordDecl {
            name: types::Identifier,
            members: Vec<VarDecl>,
        }
    }

    #[derive(Clone, Debug)]
    pub struct Program {
        name: types::Identifier,

        uses: Vec<types::Identifier>,
        functions: Vec<function::Function>,
        type_decls: Vec<type_decl::RecordDecl>,
    }

    impl Program {
        pub fn parse<I>(tokens: I) -> syntax::ParseResult<Self>
            where I: Iterator<Item=tokenizer::SourceToken>
        {
            let _program = syntax::match_sequence(&[
                syntax::TokenMatcher::Keyword(keywords::Keyword::Program),
            ], tokens)?;

            Err(syntax::ParseError::UnexpectedEOF)
        }
    }
}
