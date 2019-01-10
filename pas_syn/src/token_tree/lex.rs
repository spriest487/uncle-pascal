use {
    crate::{
        consts::{
            RealConstant,
            IntConstant,
        },
        ident::Ident,
        keyword::Keyword,
        operators::Operator,
        span::{Span, Location},
        token_tree::{
            Separator,
            DelimiterPair,
            TokenizeError,
            TokenizeResult,
            TokenTree,
        },
    },
    pas_common::{
        BuildOptions,
        TracedError,
    },
    std::{
        rc::Rc,
        path::PathBuf,
    },
};

pub fn lex(file_name: impl Into<PathBuf>, source: &str, opts: &BuildOptions) -> TokenizeResult<Vec<TokenTree>> {
    let mut lexer = Lexer {
        file: Rc::new(file_name.into()),
        line: Vec::new(),
        location: Location { line: 0, col: 0 },
        case_sensitive: opts.case_sensitive,
        delim_stack: Vec::new(),
    };

    let mut tokens = Vec::new();
    for (line_num, line) in source.lines().enumerate() {
        lexer.line = line.chars().collect();
        lexer.location = Location { line: line_num, col: 0 };

        while let Some(token) = lexer.next_token()? {
            tokens.push(token);
        }
    }

    if let Some((unmatched_at, unmatched)) = lexer.delim_stack.pop() {
        return Err(TracedError::trace(TokenizeError::UnmatchedDelimiter {
            span: lexer.span_to_current(unmatched_at.start),
            to_match: unmatched_at,
            delim: unmatched,
        }));
    }

    Ok(tokens)
}

struct Lexer {
    line: Vec<char>,
    location: Location,
    file: Rc<PathBuf>,
    case_sensitive: bool,

    delim_stack: Vec<(Span, DelimiterPair)>,
}

fn find_ahead(s: &[char], predicate: impl Fn(char) -> bool) -> Option<usize> {
    s.iter().enumerate()
        .filter_map(|(col, c)| if predicate(*c) {
            Some(col)
        } else {
            None
        })
        .next()
}

impl Lexer {
    fn span_to_current(&self, start: Location) -> Span {
        Span {
            file: self.file.clone(),
            start,
            end: self.location.clone(),
        }
    }

    fn make_float_token(&mut self, len: Option<usize>) -> TokenizeResult<TokenTree> {
        let chars = match len {
            None => &self.line[self.location.col..],
            Some(len) => &self.line[self.location.col..self.location.col + len],
        };

        let start_loc = self.location;

        self.location.col += chars.len();

        let span = self.span_to_current(start_loc);

        RealConstant::parse_str(&chars.iter().collect::<String>())
            .map(|value| TokenTree::RealNumber { value, span: span.clone() })
            .ok_or_else(|| TracedError::trace(TokenizeError::IllegalToken(span)))
    }

    fn literal_float(&mut self) -> TokenizeResult<TokenTree> {
        let find_non_num = |c| match c {
            '0'...'9' => false,
            _ => true,
        };

        //find end of integer part, as offset from self.location.col
        let mut cur = match find_ahead(&self.line[self.location.col..], find_non_num) {
            Some(after_int_part) => after_int_part,
            // no decimal point or exponent, just numbers until the end
            None => return self.make_float_token(None),
        };

        if self.line[self.location.col + cur] == '.' {
            cur += 1;
            //find end of fractional part
            cur = match find_ahead(&self.line[self.location.col + cur..], find_non_num) {
                Some(after_frac_part) => cur + after_frac_part,
                // no exponent and entire rest of line after decimal point is fractional part
                None => return self.make_float_token(None),
            }
        }

        if self.line[self.location.col + cur] == 'e' || self.line[self.location.col + cur] == 'E' {
            cur += 1;
            //find end of exponent
            cur = match find_ahead(&self.line[self.location.col + cur..], find_non_num) {
                Some(after_exponent) => cur + after_exponent,
                //exponent is last thing on this line
                None => return self.make_float_token(None),
            }
        }

        self.make_float_token(Some(cur))
    }

    fn literal_hex(&mut self) -> TokenizeResult<TokenTree> {
        let start_loc = Location {
            line: self.location.line,
            col: self.location.col - 1,  // include $ sigil
        };

        let line_after_sigil = &self.line[self.location.col + 1..];
        let next_non_hex = find_ahead(line_after_sigil, |c| match c {
            'a'...'f' |
            'A'...'F' |
            '0'...'9' => false,
            _ => true,
        });

        /* this needs to include the $ because IntConstant::parse expects it */
        let int_str = match next_non_hex {
            Some(end) => &self.line[self.location.col..self.location.col + end + 1],
            None => &self.line[self.location.col..],
        };

        self.location.col += int_str.len();
        let span = self.span_to_current(start_loc);

        match IntConstant::parse_str(&int_str.iter().collect::<String>()) {
            Some(value) => Ok(TokenTree::IntNumber { value, span }),
            None => Err(TracedError::trace(TokenizeError::IllegalToken(span))),
        }
    }

    fn literal_int(&mut self, sigil: bool) -> TokenizeResult<TokenTree> {
        let sigil_len = if sigil { 1 } else { 0 };
        let num_start = self.location.col + sigil_len;

        let start_loc = self.location;

        let next_non_num = {
            let from_start = &self.line[num_start..];
            find_ahead(from_start, |c| match c {
                '0'...'9' => false,
                _ => true,
            })
        };

        if !sigil {
            if let Some(num_end) = next_non_num {
                let end_char = self.line[num_start + num_end];

                if end_char == 'E' || end_char == 'e' {
                    /* the only valid option for num + e is a float with an exponent*/
                    return self.literal_float();
                }

                if end_char == '.' && self.line.get(num_start + num_end + 1) != Some(&'.') {
                    /* a period might mean this is a decimal, but two periods means this
                    is an integer in a range */
                    return self.literal_float();
                }
            }
        }

        let int_str = match next_non_num {
            Some(end) => &self.line[self.location.col..self.location.col + end + sigil_len],
            None => &self.line[self.location.col..],
        };

        self.location.col += int_str.len();
        let span = self.span_to_current(start_loc);

        match IntConstant::parse_str(&int_str.iter().collect::<String>()) {
            Some(value) => Ok(TokenTree::IntNumber { value, span }),
            None => Err(TracedError::trace(TokenizeError::IllegalToken(span))),
        }
    }

    fn literal_string(&mut self) -> TokenizeResult<TokenTree> {
        let mut contents = String::new();

        let start_loc = self.location;

        let mut next_col = self.location.col + 1;
        loop {
            match self.line.get(next_col) {
                /* todo: better error for unterminated string literal */
                None => return Err(TracedError::trace(TokenizeError::IllegalToken(Span {
                    file: self.file.clone(),
                    start: start_loc,
                    end: self.location,
                }))),

                /* depends on the token after this one */
                Some('\'') => match self.line.get(next_col + 1) {
                    /* it's quoted quote, add it to the contents and advance an extra col */
                    Some('\'') => {
                        contents.push('\'');
                        next_col += 1;
                    }

                    /* it's something else, this string ends here */
                    _ => break,
                }

                Some(c) => contents.push(*c),
            }

            next_col += 1;
        };

        self.location.col = next_col + 1;
        let span = self.span_to_current(start_loc);

        Ok(TokenTree::String {
            value: contents,
            span,
        })
    }

    fn word(&mut self) -> TokenizeResult<TokenTree> {
        let mut token_str = String::new();

        let start_loc = self.location;

        let mut next_col = self.location.col;
        while let Some(c) = self.line.get(next_col) {
            match c {
                '0'...'9' => if !token_str.is_empty() {
                    token_str.push(*c);
                } else {
                    //can't start with a number
                    return Err(TracedError::trace(TokenizeError::IllegalToken(Span {
                        file: self.file.clone(),
                        start: start_loc,
                        end: self.location,
                    })));
                }

                'a'...'z' |
                'A'...'Z' |
                '_' => {
                    token_str.push(*c);
                }

                _ => break,
            }

            next_col += 1;
        }

        self.location.col = next_col;
        let span = self.span_to_current(start_loc);

        let token = if let Some(kw) = Keyword::try_parse(&token_str, self.case_sensitive) {
            TokenTree::Keyword { kw, span }
        } else if let Some(op) = Operator::try_parse_text(&token_str, self.case_sensitive) {
            TokenTree::Operator { op, span }
        } else {
            TokenTree::Ident(Ident::new(&token_str, span))
        };

        Ok(token)
    }

    fn operator_token(&mut self, op: Operator, len: usize) -> TokenTree {
        let start_loc = self.location;
        self.location.col += len;
        let span = self.span_to_current(start_loc);
        TokenTree::Operator {
            op,
            span,
        }
    }

    fn separator_token(&mut self, sep: Separator, len: usize) -> TokenTree {
        let start_loc = self.location;
        self.location.col += len;
        let span = self.span_to_current(start_loc);
        TokenTree::Separator {
            sep,
            span,
        }
    }

    fn begin_delim_group(&mut self, delim: DelimiterPair, consume: usize) -> TokenizeResult<TokenTree> {
        let start_loc = self.location;
        self.location.col += consume;

        self.delim_stack.push((self.span_to_current(start_loc), delim));

        let mut inner = Vec::new();
        while let Some(inner_token) = self.next_token()? {
            inner.push(inner_token);
        }

        Ok(TokenTree::Delimited {
            delim,
            span: self.span_to_current(start_loc),
            inner,
        })
    }

    fn end_delim_group(&mut self, delim: DelimiterPair, consume: usize) -> TokenizeResult<()> {
        let start_loc = self.location;
        self.location.col += consume;
        let span = self.span_to_current(start_loc);

        match self.delim_stack.pop() {
            // no group was started, or a delimited group was started, but it wasn't the
            // same type as the one we're closing
            None => {
                Err(TracedError::trace(TokenizeError::UnexpectedCloseDelimited { delim, span }))
            }
            Some((_, unexpected_delim)) if unexpected_delim != delim => {
                Err(TracedError::trace(TokenizeError::UnexpectedCloseDelimited { delim, span }))
            }

            Some(_) => Ok(())
        }
    }

    fn next_token(&mut self) -> TokenizeResult<Option<TokenTree>> {
        let next = loop {
            let next = match self.line.get(self.location.col) {
                Some(c) => *c,
                //end if stream
                None => return Ok(None),
            };

            // skip chars until we find something other than whitespace
            if !next.is_ascii_whitespace() {
                break next;
            } else {
                self.location.col += 1;
            }
        };

        let token = match next {
            '(' => self.begin_delim_group(DelimiterPair::Bracket, 1)?,
            ')' => {
                self.end_delim_group(DelimiterPair::Bracket, 1)?;
                return Ok(None);
            }
            '[' => self.begin_delim_group(DelimiterPair::SquareBracket, 1)?,
            ']' => {
                self.end_delim_group(DelimiterPair::SquareBracket, 1)?;
                return Ok(None);
            }

            '+' => self.operator_token(Operator::Plus, 1),
            '-' => self.operator_token(Operator::Minus, 1),
            '*' => self.operator_token(Operator::Multiply, 1),
            '/' => self.operator_token(Operator::Divide, 1),
            '@' => self.operator_token(Operator::AddressOf, 1),
            ',' => self.separator_token(Separator::Comma, 1),
            '.' => match self.line.get(self.location.col + 1) {
                Some('.') => self.operator_token(Operator::RangeInclusive, 2),
                _ => self.operator_token(Operator::Member, 1),
            },
            ';' => self.separator_token(Separator::Semicolon, 1),
            '^' => self.operator_token(Operator::Deref, 1),
            '>' => match self.line.get(self.location.col + 1) {
                Some('=') => self.operator_token(Operator::Gte, 2),
                _ => self.operator_token(Operator::Gt, 1),
            }
            '<' => match self.line.get(self.location.col + 1) {
                Some('=') => self.operator_token(Operator::Lte, 2),
                Some('>') => self.operator_token(Operator::NotEquals, 2),
                _ => self.operator_token(Operator::Lt, 1),
            }
            ':' => match self.line.get(self.location.col + 1) {
                Some('=') => self.operator_token(Operator::Assignment, 2),
                _ => self.separator_token(Separator::Colon, 1),
            },
            '=' => self.operator_token(Operator::Equals, 1),
            '$' => self.literal_hex()?,
            '#' => self.literal_int(true)?,
            '0'...'9' => self.literal_int(false)?,
            '\'' => self.literal_string()?,
            'a'...'z' | '_' | 'A'...'Z' => self.word()?,

            _ => {
                let err_start = self.location;
                self.location.col += 1;
                let err_span = self.span_to_current(err_start);
                return Err(TracedError::trace(TokenizeError::IllegalToken(err_span)));
            }
        };

        match &token {
            // keyword "begin" always signals the start of a begin..end delimited group
            TokenTree::Keyword { kw: Keyword::Begin, .. } => {
                return self.begin_delim_group(DelimiterPair::BeginEnd, 0).map(Some);
            }

            // keyword "end" can appear on its own in other contexts
            TokenTree::Keyword { kw: Keyword::End, .. } => {
                if let Some((_, DelimiterPair::BeginEnd)) = self.delim_stack.last() {
                    self.end_delim_group(DelimiterPair::BeginEnd, 0)?;
                    return Ok(None);
                }
            }

            _ => {}
        }

        Ok(Some(token))
    }
}

