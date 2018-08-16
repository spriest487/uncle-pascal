use std::fmt;

use syntax::*;
use keywords;
use operators;
use tokens;
use source;

pub struct SplitResult {
    pub split_at: source::Token,
    pub before_split: Vec<source::Token>,
}

#[allow(dead_code)]
#[derive(Clone, Debug)]
pub enum Matcher {
    Keyword(keywords::Keyword),
    BinaryOperator(operators::BinaryOperator),
    AnyKeyword,
    AnyIdentifier,
    AnyBinaryOperator,
    AnyLiteralInteger,
    AnyLiteralString,
    Exact(tokens::Token),
    OneOf(Vec<Matcher>),
}

impl From<tokens::Token> for Matcher {
    fn from(token: tokens::Token) -> Self {
        Matcher::Exact(token)
    }
}

impl From<operators::BinaryOperator> for Matcher {
    fn from(op: operators::BinaryOperator) -> Self {
        Matcher::BinaryOperator(op)
    }
}

impl From<keywords::Keyword> for Matcher {
    fn from(keyword: keywords::Keyword) -> Self {
        Matcher::Keyword(keyword)
    }
}

pub trait MatchOneOf {
    fn or<T>(self, next: T) -> Matcher where T: Into<Matcher>;
}

pub trait MatchSequenceOf {
    fn and_then<T>(self, next: T) -> SequenceMatcher where T: Into<Matcher>;
}

impl<TMatchable> MatchOneOf for TMatchable where TMatchable: Into<Matcher> {
    fn or<T>(self, next: T) -> Matcher where T: Into<Matcher> {
        self.into().or(next.into())
    }
}

impl<TMatchable> MatchSequenceOf for TMatchable where TMatchable: Into<Matcher> {
    fn and_then<T>(self, next: T) -> SequenceMatcher where T: Into<Matcher> {
        self.into().and_then(next.into())
    }
}

pub trait MatchBlockOf {
    fn terminated_by<T>(self, end: T) -> BlockMatcher where T: Into<Matcher>;
}

impl<TMatchable> MatchBlockOf for TMatchable where TMatchable: Into<Matcher> {
    fn terminated_by<T>(self, end: T) -> BlockMatcher where T: Into<Matcher> {
        self.into().terminated_by(end.into())
    }
}

impl fmt::Display for Matcher {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &Matcher::Keyword(kw) => write!(f, "{}", tokens::Keyword(kw)),
            &Matcher::BinaryOperator(ref op) => write!(f, "{}", op),
            &Matcher::AnyKeyword => write!(f, "keyword"),
            &Matcher::AnyIdentifier => write!(f, "identifier"),
            &Matcher::AnyBinaryOperator => write!(f, "binary operator"),
            &Matcher::AnyLiteralInteger => write!(f, "integer literal"),
            &Matcher::AnyLiteralString => write!(f, "string literal"),
            &Matcher::Exact(ref exact_token) => write!(f, "{}", exact_token),
            &Matcher::OneOf(ref matchers) => write!(f, "one of: {}", matchers.iter()
                .map(|matcher| format!("{}", matcher))
                .collect::<Vec<_>>()
                .join(", ")),
        }
    }
}

impl Matcher {
    pub fn is_match<T>(&self, token: &T) -> bool
        where T: tokens::AsToken
    {
        match self {
            &Matcher::Keyword(kw) => token.is_keyword(kw),
            &Matcher::AnyKeyword => token.is_any_keyword(),
            &Matcher::BinaryOperator(ref op) => token.is_binary_operator(op),
            &Matcher::AnyIdentifier => token.is_any_identifier(),
            &Matcher::AnyBinaryOperator => token.is_any_binary_operator(),
            &Matcher::AnyLiteralInteger => token.is_any_literal_int(),
            &Matcher::AnyLiteralString => token.is_any_literal_string(),
            &Matcher::Exact(ref exact_token) => token.as_token() == exact_token,
            &Matcher::OneOf(ref matchers) => matchers.iter()
                .any(|matcher| matcher.is_match(token)),
        }
    }

    pub fn or<TMatchable>(self, or: TMatchable) -> Matcher
        where TMatchable: Into<Matcher>
    {
        match self {
            Matcher::OneOf(mut options) => {
                options.push(or.into());
                Matcher::OneOf(options)
            }
            _ => {
                Matcher::OneOf(vec![
                    self,
                    or.into(),
                ])
            }
        }
    }

    pub fn and_then<TMatchable>(self, next_matcher: TMatchable) -> SequenceMatcher
        where TMatchable: Into<Matcher>
    {
        SequenceMatcher {
            sequence: vec![self, next_matcher.into()]
        }
    }

    pub fn terminated_by<TMatchable>(self, close: TMatchable) -> BlockMatcher
        where TMatchable: Into<Matcher>
    {
        BlockMatcher {
            open: self,
            close: close.into(),
        }
    }
}

pub trait Matchable {
    fn as_matcher(&self) -> Matcher;

    fn match_one<TIter>(&self,
                        in_tokens: TIter,
                        context: &source::Token)
                        -> ParseResult<source::Token>
        where TIter: IntoIterator<Item=source::Token> + 'static
    {
        let mut tokens = in_tokens.into_iter();
        match tokens.next() {
            Some(ref token) if self.as_matcher().is_match(token) => {
                Ok(ParseOutput::new(token.clone(), token.clone(), tokens))
            }

            Some(ref unexpected) => {
                Err(ParseError::UnexpectedToken(unexpected.clone(), Some(self.as_matcher())))
            }

            None => {
                Err(ParseError::UnexpectedEOF(self.as_matcher(), context.clone()))
            }
        }
    }

    fn match_peek<TIter>(&self, in_tokens: TIter,
                         context: &source::Token)
                         -> ParseResult<Option<source::Token>>
        where TIter: IntoIterator<Item=source::Token> + 'static {
        let mut peekable = in_tokens.into_iter().peekable();

        match peekable.peek().cloned() {
            Some(ref token) if self.as_matcher().is_match(token) => {
                Ok(ParseOutput::new(Some(token.clone()),
                                    context.clone(),
                                    peekable))
            }

            Some(_) => {
                Ok(ParseOutput::new(None,
                                    context.clone(),
                                    peekable))
            }

            None => Err(ParseError::UnexpectedEOF(self.as_matcher(), context.clone())),
        }
    }

    fn split_at_match<TIter>(&self,
                             in_tokens: TIter,
                             context: &source::Token)
                             -> ParseResult<SplitResult>
        where TIter: IntoIterator<Item=source::Token> + 'static
    {
        let mut before_split = Vec::new();
        let split_at;

        let mut tokens = in_tokens.into_iter();

        loop {
            match tokens.next() {
                Some(next_token) => {
                    if self.as_matcher().is_match(&next_token) {
                        split_at = next_token.clone();
                        break;
                    } else {
                        before_split.push(next_token.clone());
                    }
                }

                None => {
                    //ran out of tokens before finding match
                    return Err(ParseError::UnexpectedEOF(self.as_matcher(), context.clone()));
                }
            }
        }

        Ok(ParseOutput::new(SplitResult {
            split_at: split_at.clone(),
            before_split,
        }, split_at, tokens))
    }

    fn match_until<TIter>(&self, in_tokens: TIter,
                          context: &source::Token)
                          -> ParseResult<Vec<source::Token>>
        where TIter: IntoIterator<Item=source::Token> + 'static
    {
        let mut tokens_until = Vec::new();
        let mut peekable_tokens = in_tokens.into_iter().peekable();
        let mut last_context = context.clone();

        loop {
            let peeked_next = peekable_tokens.peek().cloned();

            match peeked_next {
                Some(ref matching) if self.as_matcher().is_match(matching) => {
                    break;
                }

                Some(_) => {
                    let next = peekable_tokens.next().unwrap();
                    last_context = next.clone();
                    tokens_until.push(next.clone());
                }

                None => {
                    return Err(ParseError::UnexpectedEOF(self.as_matcher(), last_context));
                }
            }
        }

        Ok(ParseOutput::new(tokens_until, last_context, peekable_tokens))
    }
}

impl Matchable for Matcher {
    fn as_matcher(&self) -> Matcher {
        self.clone()
    }
}

impl Matchable for tokens::Token {
    fn as_matcher(&self) -> Matcher {
        Matcher::Exact(self.clone())
    }
}

impl Matchable for keywords::Keyword {
    fn as_matcher(&self) -> Matcher {
        Matcher::Keyword(self.clone())
    }
}

impl Matchable for operators::BinaryOperator {
    fn as_matcher(&self) -> Matcher {
        Matcher::BinaryOperator(self.clone())
    }
}

#[derive(Clone, Debug)]
pub struct SequenceMatcher {
    sequence: Vec<Matcher>,
}

impl SequenceMatcher {
    pub fn and_then<TMatchable>(mut self, next_matcher: TMatchable) -> Self
        where TMatchable: Into<Matcher>
    {
        self.sequence.push(next_matcher.into());
        self
    }

    pub fn match_sequence<TIter>(&self, in_tokens: TIter,
                                 context: &source::Token)
                                 -> ParseResult<Vec<source::Token>>
        where TIter: IntoIterator<Item=source::Token> + 'static
    {
        let tokens: Vec<_> = in_tokens.into_iter().collect();

        let expected_len = self.sequence.len();

        let matches: Vec<_> = tokens.iter()
            .zip(self.sequence.iter())
            .map(|(token, matcher)| (token.clone(), matcher.clone(), matcher.is_match(token)))
            .collect();

        if matches.len() < expected_len {
            Err(ParseError::UnexpectedEOF(self.sequence[0].clone(), context.clone()))
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

                    let last_parsed = matched_tokens.last().unwrap().clone();

                    Ok(ParseOutput::new(matched_tokens,
                                        last_parsed,
                                        remaining_tokens))
                }
            }
        }
    }
}

#[derive(Clone, Debug)]
pub struct BlockMatch {
    pub open: source::Token,
    pub close: source::Token,
    pub inner: Vec<source::Token>,
}

#[derive(Clone, Debug)]
pub struct GroupMatch {
    pub tokens: Vec<source::Token>,
    pub context: source::Token,
}

#[derive(Clone, Debug)]
pub struct GroupsMatch {
    pub open: source::Token,
    pub close: source::Token,
    pub groups: Vec<GroupMatch>,
}

type BlockMatchResult = Result<ParseOutput<BlockMatch>, ParseError>;
type BlockMatchPeekResult = Result<ParseOutput<Option<BlockMatch>>, ParseError>;

#[derive(Clone, Debug)]
pub struct BlockMatcher {
    open: Matcher,
    close: Matcher,
}

impl BlockMatcher {
    pub fn match_block_peek<TIter>(&self, in_tokens: TIter, context: &source::Token)
        -> BlockMatchPeekResult
        where TIter: IntoIterator<Item=source::Token> + 'static
    {
        let open_token = self.open.match_peek(in_tokens, context)?;
        if open_token.value.is_none() {
            return Ok(ParseOutput::new(None, open_token.last_token, open_token.next_tokens));
        }

        let mut block_tokens = open_token.next_tokens.skip(1);

        let mut open_count = 1;
        let mut peeked = vec![open_token.value.as_ref().unwrap().clone()];

        let mut final_close_token = None;

        loop {
            let next = block_tokens.next();
            if let &Some(ref next) = &next {
                peeked.push(next.clone());
            }

            match next {
                Some(ref nested_open) if self.open.is_match(nested_open) => {
                    open_count += 1;
                }
                Some(ref close) if self.close.is_match(close) => {
                    open_count -= 1;

                    if open_count == 0 {
                        final_close_token = Some(close.clone());
                        break;
                    }
                }
                Some(_) => {}

                None => break,
            }
        }

        let inner_len = peeked.len() - 2; //2 because open + close
        let inner_tokens = peeked.iter()
            .skip(1)
            .take(inner_len)
            .cloned()
            .collect::<Vec<_>>();

        let tokens_unpeeked = peeked.into_iter().chain(block_tokens);

        match final_close_token {
            Some(close_token) => {
                let matched_block = BlockMatch {
                    open: open_token.value.unwrap(),
                    close: close_token.clone(),
                    inner: inner_tokens,
                };

                Ok(ParseOutput::new(Some(matched_block), close_token, tokens_unpeeked))
            }
            None => {
                Ok(ParseOutput::new(None, open_token.last_token, tokens_unpeeked))
            }
        }
    }

    pub fn match_block<TIter>(&self, in_tokens: TIter,
                              context: &source::Token) -> BlockMatchResult
        where TIter: IntoIterator<Item=source::Token> + 'static
    {
        let open_token = self.open.match_one(in_tokens, context)?;

        let mut block_tokens = open_token.next_tokens;
        let mut inner_tokens = Vec::new();
        let mut open_count = 1;

        loop {
            match (*block_tokens).next() {
                Some(ref open) if self.open.is_match(open) => {
                    inner_tokens.push(open.clone());
                    open_count += 1;
                }

                Some(ref close) if self.close.is_match(close) => {
                    open_count -= 1;

                    if open_count == 0 {
                        let block = BlockMatch {
                            open: open_token.value,
                            close: close.clone(),
                            inner: inner_tokens,
                        };

                        return Ok(ParseOutput::new(block, close.clone(), block_tokens));
                    } else {
                        inner_tokens.push(close.clone());
                    }
                }

                Some(ref inner) => {
                    inner_tokens.push(inner.clone());
                }

                None => {
                    return Err(ParseError::UnexpectedEOF(self.close.clone(), open_token.last_token));
                }
            }
        }
    }

    pub fn match_groups_inner<TIter, TSepMatcher>(&self,
                                                  separator_matcher: TSepMatcher,
                                                  in_tokens: TIter,
                                                  context: &source::Token)
                                                  -> ParseResult<Vec<GroupMatch>>
        where TIter: IntoIterator<Item=source::Token> + 'static,
              TSepMatcher: Into<Matcher>
    {
        let match_separator = separator_matcher.into();

        //the groups are found in the inner tokens of the outer block
        let mut group_tokens: Box<Iterator<Item=TIter::Item>> = Box::new(in_tokens.into_iter());
        let mut group_last_token = context.clone();

        let mut groups = Vec::new();
        let mut next_group = GroupMatch {
            context: group_last_token.clone(),
            tokens: Vec::new(),
        };

        loop {
            let mut peek_group_tokens = group_tokens.peekable();

            match peek_group_tokens.peek().cloned() {
                /* ran out of inner tokens, this is the last group */
                None => {
                    if next_group.tokens.len() > 0 {
                        groups.push(next_group);
                    }

                    group_tokens = Box::new(peek_group_tokens);
                    break
                }

                Some(next_token) => {
                    if self.open.is_match(&next_token) {
                        /* this token is the start of an inner group, include the */
                        let inner_block = self.match_block(peek_group_tokens,
                                                           &group_last_token)?;

                        next_group.tokens.push(inner_block.value.open.clone());
                        next_group.tokens.extend(inner_block.value.inner);
                        next_group.tokens.push(inner_block.value.close.clone());

                        group_tokens = inner_block.next_tokens;
                        group_last_token = inner_block.value.close;
                    } else {
                        if match_separator.is_match(&next_token) {
                            //finish the group
                            if next_group.tokens.len() > 0 {
                                groups.push(next_group);
                                next_group = GroupMatch {
                                    tokens: Vec::new(),
                                    context: group_last_token.clone(),
                                };
                            }
                        } else {
                            next_group.tokens.push(next_token.clone());
                        }

                        //skip 1 because we already peeked this value
                        group_tokens = Box::new(peek_group_tokens.skip(1));
                        group_last_token = next_token;
                    }
                }
            }
        }

        Ok(ParseOutput::new(groups, group_last_token, group_tokens))
    }

    pub fn match_groups<TIter, TSepMatcher>(&self,
                                            separator_matcher: TSepMatcher,
                                            in_tokens: TIter,
                                            context: &source::Token)
                                            -> ParseResult<GroupsMatch>
        where TIter: IntoIterator<Item=source::Token> + 'static,
              TSepMatcher: Into<Matcher>
    {
        //match the outer block
        let outer_block = self.match_block(in_tokens, context)?;

        let groups = self.match_groups_inner(separator_matcher,
                                             outer_block.value.inner,
                                             &outer_block.last_token)?.finish()?;

        let groups_match = GroupsMatch {
            open: outer_block.value.open,
            close: outer_block.value.close,
            groups,
        };

        Ok(ParseOutput::new(groups_match,
                            outer_block.last_token,
                            outer_block.next_tokens))
    }
}

#[cfg(test)]
mod test {
    use std::rc::*;
    use keywords;
    use tokens;
    use source;
    use syntax::*;
    use tokens::AsToken;

    fn tokens_to_source(tokens: Vec<tokens::Token>) -> Vec<source::Token> {
        tokens.into_iter()
            .map(|t| source::Token {
                token: Rc::from(t),
                location: source::Location::new("test", 0, 0)
            })
            .collect()
    }

    fn source_to_tokens<I>(iter: I) -> Vec<tokens::Token>
        where I: IntoIterator<Item=source::Token>
    {
        iter.into_iter().map(|t| t.as_token().clone())
            .collect()
    }

    #[test]
    fn block_matches_nested() {
        let tokens = tokens_to_source(vec![
            tokens::Keyword(keywords::Begin),
            tokens::Keyword(keywords::Begin),
            tokens::Identifier("hello world!".to_owned()),
            tokens::Keyword(keywords::End),
            tokens::Keyword(keywords::End),
        ]);

        let matcher = keywords::Begin.terminated_by(keywords::End);

        let context = tokens[0].clone();

        let result = matcher.match_block(tokens, &context);
        assert!(result.is_ok());
        let block = result.unwrap();

        assert_eq!(&tokens::Keyword(keywords::Begin), block.value.open.as_token());

        assert_eq!(vec![
            tokens::Keyword(keywords::Begin),
            tokens::Identifier("hello world!".to_owned()),
            tokens::Keyword(keywords::End),
        ], source_to_tokens(block.value.inner));
        assert_eq!(&tokens::Keyword(keywords::End), block.value.close.as_token());
    }

    #[test]
    fn block_matches_empty_groups() {
        let tokens = tokens_to_source(vec![tokens::BracketLeft, tokens::BracketRight]);

        let context = tokens[0].clone();
        let result = tokens::BracketLeft.terminated_by(tokens::BracketRight)
            .match_groups(tokens::Comma, tokens, &context);

        assert!(result.is_ok());
        let groups = result.unwrap().value.groups;

        assert_eq!(0, groups.len());
    }

    #[test]
    fn block_matches_groups() {
        let tokens = tokens_to_source(vec![
            tokens::Keyword(keywords::Begin),
            tokens::LiteralString("one".to_owned()),
            tokens::LiteralString("two".to_owned()),
            tokens::Semicolon,
            tokens::LiteralString("three".to_owned()),
            tokens::LiteralString("four".to_owned()),
            tokens::Keyword(keywords::End),
        ]);

        let matcher = keywords::Begin.terminated_by(keywords::End);

        let context = tokens[0].clone();
        let result = matcher.match_groups(tokens::Semicolon, tokens, &context);

        assert!(result.is_ok());
        let groups = result.unwrap().value.groups;

        assert_eq!(2, groups.len());
        assert_eq!(vec![
            tokens::LiteralString("one".to_owned()),
            tokens::LiteralString("two".to_owned())
        ], source_to_tokens(groups[0].tokens.clone()));
        assert_eq!(vec![
            tokens::LiteralString("three".to_owned()),
            tokens::LiteralString("four".to_owned())
        ], source_to_tokens(groups[1].tokens.clone()));
    }

    #[test]
    fn block_matches_groups_and_skips_nested() {
        let tokens = tokens_to_source(vec![
            tokens::Keyword(keywords::Begin),
            tokens::LiteralString("one".to_owned()),
            tokens::LiteralString("two".to_owned()),
            tokens::Keyword(keywords::Begin),
            tokens::LiteralString("three".to_owned()),
            tokens::Semicolon,
            tokens::Keyword(keywords::End),
            tokens::LiteralString("four".to_owned()),
            tokens::Keyword(keywords::End),
        ]);

        let matcher = keywords::Begin.terminated_by(keywords::End);

        let context = tokens[0].clone();
        let result = matcher.match_groups(tokens::Semicolon, tokens, &context);

        assert!(result.is_ok());
        let groups = result.unwrap().value.groups;

        assert_eq!(1, groups.len());
        assert_eq!(vec![
            tokens::LiteralString("one".to_owned()),
            tokens::LiteralString("two".to_owned()),
            tokens::Keyword(keywords::Begin),
            tokens::LiteralString("three".to_owned()),
            tokens::Semicolon,
            tokens::Keyword(keywords::End),
            tokens::LiteralString("four".to_owned()),
        ], source_to_tokens(groups[0].tokens.clone()));
    }
}
