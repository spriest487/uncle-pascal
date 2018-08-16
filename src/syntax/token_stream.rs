use std::{
    collections::VecDeque,
};
use source;
use tokens::AsToken;
use tokenizer::{
    tokenize,
    TokenizeResult,
};
use opts::CompileOptions;
use syntax::{
    matcher::*,
    ParseError,
    ParseResult,
    BlockGroupsMatch,
    SplitResult,
    GroupMatch,
};

pub struct TokenStream {
    tokens: Box<Iterator<Item=source::Token>>,
    context: source::Token,

    lookahead_buffer: VecDeque<source::Token>,
}

impl Iterator for TokenStream {
    type Item = source::Token;

    fn next(&mut self) -> Option<<Self as Iterator>::Item> {
        self.lookahead_buffer.pop_front()
            .or_else(|| self.tokens.next())
            .map(|next_token| {
                self.context = next_token.clone();
                next_token
            })
    }
}

impl From<GroupMatch> for TokenStream {
    fn from(group: GroupMatch) -> Self {
        TokenStream::new(group.tokens, &group.context)
    }
}

impl From<Vec<source::Token>> for TokenStream {
    fn from(tokens: Vec<source::Token>) -> Self {
        if tokens.len() == 0 {
            panic!("length of tokenstream must be at least 1");
        }

        let context = tokens[0].clone();

        let boxed: Box<Iterator<Item=source::Token>> = Box::new(tokens.into_iter());

        TokenStream {
            tokens: boxed,
            context,

            lookahead_buffer: VecDeque::new(),
        }
    }
}

impl TokenStream {
    pub fn tokenize(file_name: &str, source: &str, opts: &CompileOptions) -> TokenizeResult<Self> {
        let tokens = tokenize(file_name, source, opts)?;

        Ok(TokenStream::from(tokens))
    }

    pub fn new(tokens: impl IntoIterator<Item=source::Token> + 'static,
               context: &source::Token) -> Self {
        let boxed: Box<Iterator<Item=source::Token>> = Box::new(tokens.into_iter());

        TokenStream {
            tokens: boxed,
            context: context.clone(),

            lookahead_buffer: VecDeque::new(),
        }
    }

    pub fn context(&self) -> &source::Token {
        &self.context
    }

    pub fn advance(&mut self, count: usize) {
        /* skip new stream elements */
        for _ in 0..count {
            self.next();
        }
    }

    pub fn advance_to(&mut self, location: &source::Location) {
        loop {
            if self.context.location.ge(&location) {
                break;
            }

            if self.next().is_none() {
                panic!("must not run out of tokens while advancing (at {}, lookahead count: {})",
                       self.context,
                       self.lookahead_buffer.len());
            };
        }
    }

    pub fn finish(mut self) -> ParseResult<()> {
        match self.next() {
            Some(unexpected) => {
                Err(ParseError::UnexpectedToken(unexpected, None))
            }
            None => Ok(())
        }
    }

    pub fn match_one(&mut self, matcher: impl Into<Matcher>) -> ParseResult<source::Token> {
        let matcher = matcher.into();

        match self.next() {
            Some(token) => {
                if !matcher.is_match(token.as_token()) {
                    return Err(ParseError::UnexpectedToken(token, Some(matcher)));
                }

                Ok(token)
            }

            None => {
                Err(ParseError::UnexpectedEOF(matcher, self.context.clone()))
            }
        }
    }

    /// match an optional line terminator. a token on the next line will satisfy the
    /// match but will not be consumed, and the end of the input will be treated as a
    /// success
    pub fn match_or_endl(&mut self, matcher: impl Into<Matcher>) -> ParseResult<()> {
        let matcher = matcher.into();

        match self.look_ahead().next() {
            // EOF counts as an endl
            None => Ok(()),

            // endl - next token is on a new line
            Some(ref token) if token.location.line != self.context.location.line => Ok(()),

            // found separator token - next token is the one beyond that
            Some(ref token) if matcher.is_match(token) => {
                self.next();
                Ok(())
            }

            Some(unexpected) => {
                Err(ParseError::UnexpectedToken(unexpected, Some(matcher)))
            }
        }
    }

    pub fn match_sequence(&mut self,
                          sequence: impl IntoIterator<Item=Matcher>)
                          -> Result<Vec<source::Token>, ParseError> {
        sequence.into_iter()
            .map(|matcher| self.match_one(matcher))
            .collect::<Result<Vec<_>, _>>()
    }

    pub fn match_groups_inner(&mut self,
                              open: impl Into<Matcher>,
                              close: impl Into<Matcher>,
                              separator: impl Into<Matcher>)
                              -> ParseResult<GroupsMatch> {
        let match_open = open.into();
        let match_close = close.into();
        let match_separator = separator.into();

        let mut separators = Vec::new();
        let mut groups = Vec::new();

        let mut next_group = GroupMatch {
            context: self.context.clone(),
            tokens: Vec::new(),
        };

        loop {
            match self.look_ahead().next() {
                /* ran out of inner tokens, this is the last group */
                None => {
                    if next_group.tokens.len() > 0 {
                        groups.push(next_group);
                    }

                    break;
                }

                Some(next_token) => {
                    if match_open.is_match(&next_token) {
                        /* this token is the start of an inner group, match the block to
                         find where it ends (we don't advance because the open token is needed
                         to match the block )*/
                        let inner_block = self.match_block(match_open.clone(),
                                                           match_close.clone())?;

                        next_group.tokens.push(inner_block.open.clone());
                        next_group.tokens.extend(inner_block.inner);
                        next_group.tokens.push(inner_block.close.clone());
                    } else {
                        //advance because we already peeked this value
                        self.advance(1);

                        if match_separator.is_match(&next_token) {
                            //finish the group
                            if next_group.tokens.len() > 0 {
                                groups.push(next_group);
                                next_group = GroupMatch {
                                    tokens: Vec::new(),
                                    context: self.context().clone(),
                                };
                            }

                            separators.push(next_token.clone());
                        } else {
                            next_group.tokens.push(next_token.clone());
                        }
                    }
                }
            }
        }

        Ok(GroupsMatch {
            groups,
            separators,
        })
    }


    pub fn match_block(&mut self,
                       open: impl Into<Matcher>,
                       close: impl Into<Matcher>)
                       -> ParseResult<BlockMatch> {
        let open_matcher = open.into();
        let close_matcher = close.into();

        let open_token = self.match_one(open_matcher.clone())?;

        let mut inner_tokens = Vec::new();
        let mut open_count = 1;

        loop {
            match self.next() {
                Some(ref open) if open_matcher.is_match(open) => {
                    inner_tokens.push(open.clone());
                    open_count += 1;
                }

                Some(ref close) if close_matcher.is_match(close) => {
                    open_count -= 1;

                    if open_count == 0 {
                        break Ok(BlockMatch {
                            open: open_token,
                            close: close.clone(),
                            inner: inner_tokens,
                        });
                    } else {
                        inner_tokens.push(close.clone());
                    }
                }

                Some(ref inner) => {
                    inner_tokens.push(inner.clone());
                }

                None => {
                    return Err(ParseError::UnexpectedEOF(close_matcher,
                                                         self.context.clone()));
                }
            }
        }
    }

    pub fn match_groups(&mut self,
                        open: impl Into<Matcher>,
                        close: impl Into<Matcher>,
                        separator: impl Into<Matcher>)
                        -> ParseResult<BlockGroupsMatch> {
        let open = open.into();
        let close = close.into();

        //match the outer block
        let outer_block = self.match_block(open.clone(), close.clone())?;

        let mut inner_tokens = TokenStream::new(outer_block.inner, &outer_block.open);

        let groups = inner_tokens.match_groups_inner(open, close, separator)?;
        inner_tokens.finish()?;

        Ok(BlockGroupsMatch {
            open: outer_block.open,
            close: outer_block.close,
            groups: groups.groups,
            separators: groups.separators,
        })
    }

    pub fn look_ahead(&mut self) -> LookAheadTokenStream {
        LookAheadTokenStream {
            tokens: self,
            pos: 0,
            limit: None,
        }
    }

    pub fn split_at_match(&mut self, matcher: impl Into<Matcher>)
                          -> ParseResult<SplitResult> {
        let matcher = matcher.into();
        let mut before_split = Vec::new();

        let split_at = loop {
            match self.next() {
                Some(next_token) => {
                    if matcher.is_match(&next_token) {
                        break next_token;
                    } else {
                        before_split.push(next_token.clone());
                    }
                }

                None => {
                    //ran out of tokens before finding match
                    return Err(ParseError::UnexpectedEOF(matcher, self.context.clone()));
                }
            }
        };

        Ok(SplitResult {
            split_at,
            before_split,
        })
    }

    pub fn parse<TParsed>(&mut self) -> ParseResult<TParsed>
        where TParsed: Parse
    {
        TParsed::parse(self)
    }

    pub fn parse_to_end<TParsed>(mut self) -> ParseResult<TParsed>
        where TParsed: Parse
    {
        let result = TParsed::parse(&mut self)?;
        self.finish()?;
        Ok(result)
    }
}

pub trait Parse where Self: Sized {
    fn parse(tokens: &mut TokenStream) -> ParseResult<Self>;
}

pub struct LookAheadTokenStream<'tokens> {
    tokens: &'tokens mut TokenStream,
    pos: usize,
    limit: Option<usize>,
}

impl<'tokens> LookAheadTokenStream<'tokens> {
    pub fn limit(mut self, limit: usize) -> Self {
        self.limit = Some(limit);
        self
    }

    pub fn context(&self) -> &source::Token {
        if self.pos == 0 {
            self.tokens.context()
        } else {
            &self.tokens.lookahead_buffer[self.pos - 1]
        }
    }

    pub fn match_one(&mut self, matcher: impl Into<Matcher>) -> Option<source::Token> {
        let matcher = matcher.into();

        self.next().and_then(|t| {
            match matcher.is_match(t.as_token()) {
                true => Some(t),
                false => None
            }
        })
    }

    pub fn find(&mut self, matcher: impl Into<Matcher>) -> Option<usize> {
        let matcher = matcher.into();

        let mut offset = 0;
        let result = loop {
            match self.next() {
                Some(next) => {
                    offset += 1;
                    if matcher.is_match(&next) {
                        break Some(offset - 1);
                    }
                }

                None => {
                    break None
                },
            }
        };

        self.pos -= offset;
        result
    }

    pub fn match_sequence(&mut self,
                          sequence: impl Into<SequenceMatcher>)
                          -> Option<Vec<source::Token>> {
        let mut sequence = sequence.into().into_iter();
        let mut matches = Vec::new();

        loop {
            let seq_next_matcher = match sequence.next() {
                Some(matcher) => matcher,
                None => {
                    /* reached end of sequence without incident */
                    break Some(matches);
                }
            };

            match self.next() {
                Some(peeked_token) => {
                    if seq_next_matcher.is_match(&peeked_token) {
                        matches.push(peeked_token);
                    } else {
                        // no match
                        break None;
                    }
                }
                None => {
                    /* there are less tokens in the stream than in the sequence */
                    break None;
                }
            };
        }
    }

    /* maybe matches a block. regardless of if the block is matched, the
    next tokens and last token of the resulting output are the same as the
    input, if no error occurs */
    pub fn match_block(&mut self,
                       open: impl Into<Matcher>,
                       close: impl Into<Matcher>)
                       -> Option<BlockMatch> {
        let open_matcher = open.into();
        let close_matcher = close.into();

        let open_token = self.match_one(open_matcher.clone());
        if open_token.is_none() {
            return None;
        }

        let mut open_count = 1;
        let mut inner_tokens = Vec::new();

        let final_close_token = loop {
            let next = self.next();

            match next {
                Some(inner_token) => if close_matcher.is_match(&inner_token) {
                    open_count -= 1;

                    if open_count == 0 {
                        break Some(inner_token);
                    } else {
                        inner_tokens.push(inner_token);
                    }
                } else {
                    if open_matcher.is_match(&inner_token) {
                        open_count += 1;
                    }

                    inner_tokens.push(inner_token);
                }

                None => break None,
            }
        };

        match final_close_token {
            Some(close_token) => {
                let matched_block = BlockMatch {
                    open: open_token.unwrap(),
                    close: close_token,
                    inner: inner_tokens,
                };

                Some(matched_block)
            }
            None => {
                None
            }
        }
    }

    pub fn match_groups_inner(&mut self,
                              match_open: impl Into<Matcher>,
                              match_close: impl Into<Matcher>,
                              match_separator: impl Into<Matcher>)
                              -> GroupsMatch {
        let match_open = match_open.into();
        let match_close = match_close.into();
        let match_separator = match_separator.into();

        let mut separators = Vec::new();
        let mut groups = Vec::new();

        let mut next_group = GroupMatch {
            context: self.context().clone(),
            tokens: Vec::new(),
        };

        loop {
            match self.next() {
                /* ran out of inner tokens, this is the last group */
                None => {
                    if next_group.tokens.len() > 0 {
                        groups.push(next_group);
                    }

                    break;
                }

                Some(next_token) => {
                    if match_open.is_match(&next_token) {
                        /* go back, we need to block match starting from this open token */
                        self.pos -= 1;

                        match self.match_block(match_open.clone(), match_close.clone()) {
                            /* add the whole contents of the inner group to the last group */
                            Some(inner_block) => {
                                next_group.tokens.push(inner_block.open.clone());
                                next_group.tokens.extend(inner_block.inner);
                                next_group.tokens.push(inner_block.close.clone());
                            }

                            /* no close token? just add the rest of the tokens to the last group
                             and stop, this will probably turn into a real parser error later but
                              it's not this function's job to deal with that */
                            None => {
                                loop {
                                    match self.next() {
                                        Some(t) => next_group.tokens.push(t),
                                        None => break,
                                    }
                                }
                                break;
                            }
                        }
                    } else {
                        if match_separator.is_match(&next_token) {
                            //finish the group
                            if next_group.tokens.len() > 0 {
                                groups.push(next_group);
                                next_group = GroupMatch {
                                    tokens: Vec::new(),
                                    context: self.context().clone(),
                                };
                            }

                            separators.push(next_token.clone());
                        } else {
                            next_group.tokens.push(next_token.clone());
                        }
                    }
                }
            }
        }

        GroupsMatch {
            groups,
            separators,
        }
    }
}

impl<'tokens> Iterator for LookAheadTokenStream<'tokens> {
    type Item = source::Token;

    fn next(&mut self) -> Option<source::Token> {
        if let Some(limit) = self.limit {
            if self.pos >= limit {
                return None;
            }
        }

        let next = if self.pos < self.tokens.lookahead_buffer.len() {
            self.tokens.lookahead_buffer.get(self.pos).cloned()
        } else {
            let next_token = self.tokens.tokens.next()?;
            self.tokens.lookahead_buffer.push_back(next_token.clone());
            Some(next_token)
        };

        self.pos += 1;
        next
    }
}

#[cfg(test)]
mod test {
    use std::rc::*;
    use keywords;
    use tokens;
    use source;
    use operators;
    use syntax::*;
    use tokens::AsToken;

    fn tokens_to_source(tokens: Vec<tokens::Token>) -> Vec<source::Token> {
        tokens.into_iter()
            .map(|t| source::Token {
                token: Rc::from(t),
                location: source::Location::new("test", 0, 0),
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
        let mut tokens = TokenStream::from(tokens_to_source(vec![
            tokens::Keyword(keywords::Begin),
            tokens::Keyword(keywords::Begin),
            tokens::Identifier("hello world!".to_owned()),
            tokens::Keyword(keywords::End),
            tokens::Keyword(keywords::End),
        ]));

        let result = tokens.match_block(keywords::Begin, keywords::End);
        assert!(result.is_ok());
        let block = result.unwrap();

        assert_eq!(&tokens::Keyword(keywords::Begin), block.open.as_token());

        assert_eq!(vec![
            tokens::Keyword(keywords::Begin),
            tokens::Identifier("hello world!".to_owned()),
            tokens::Keyword(keywords::End),
        ], source_to_tokens(block.inner));
        assert_eq!(&tokens::Keyword(keywords::End), block.close.as_token());
    }

    #[test]
    fn block_matches_empty_groups() {
        let mut tokens = TokenStream::from(tokens_to_source(vec![
            tokens::BracketLeft,
            tokens::BracketRight
        ]));

        let result = tokens.match_groups(tokens::BracketLeft, tokens::BracketRight, tokens::Comma);

        assert!(result.is_ok());
        let groups = result.unwrap().groups;

        assert_eq!(0, groups.len());
    }

    #[test]
    fn block_matches_groups() {
        let mut tokens = TokenStream::from(tokens_to_source(vec![
            tokens::Keyword(keywords::Begin),
            tokens::LiteralString("one".to_owned()),
            tokens::LiteralString("two".to_owned()),
            tokens::Semicolon,
            tokens::LiteralString("three".to_owned()),
            tokens::LiteralString("four".to_owned()),
            tokens::Keyword(keywords::End),
        ]));

        let result = tokens.match_groups(keywords::Begin, keywords::End, tokens::Semicolon);

        assert!(result.is_ok());
        let groups = result.unwrap().groups;

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
        let mut tokens = TokenStream::from(tokens_to_source(vec![
            tokens::Keyword(keywords::Begin),
            tokens::LiteralString("one".to_owned()),
            tokens::LiteralString("two".to_owned()),
            tokens::Keyword(keywords::Begin),
            tokens::LiteralString("three".to_owned()),
            tokens::Semicolon,
            tokens::Keyword(keywords::End),
            tokens::LiteralString("four".to_owned()),
            tokens::Keyword(keywords::End),
        ]));

        let result = tokens.match_groups(keywords::Begin, keywords::End, tokens::Semicolon);

        assert!(result.is_ok());
        let groups = result.unwrap().groups;

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

    #[test]
    fn match_groups_inner_includes_all_separators() {
        let mut tokens = TokenStream::from(tokens_to_source(vec![
            tokens::Operator(operators::Plus),
            tokens::Operator(operators::Minus),
            tokens::Identifier("test1".to_owned()),
            tokens::Operator(operators::Equals),
            tokens::Operator(operators::NotEquals),
        ]));

        let result = tokens.match_groups_inner(tokens::BracketLeft,
                                               tokens::BracketRight,
                                               Matcher::AnyOperator);

        assert!(result.is_ok());
        let groups_match = result.unwrap();

        assert_eq!(1, groups_match.groups.len());
        assert_eq!(4, groups_match.separators.len());
    }
}
