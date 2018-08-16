use syntax::*;
use tokens;
use keywords;
use node;
use ToSource;

pub type Block = node::Block<node::Identifier>;

impl Block {
    fn parse_block_expr<TIter>(tokens: TIter, context: &TIter::Item)
                               -> ParseResult<Option<Expression>, TIter::Item>
        where TIter: IntoIterator + 'static,
              TIter::Item: tokens::AsToken + 'static
    {
        let mut next_expr_tokens: Box<Iterator<Item=TIter::Item>> = Box::new(tokens.into_iter());
        let mut last_expr_token = context.clone();

        /* take tokens until the next semicolon, but if a new begin/end block
                        begins inside this expression, include the whole contents of the block in
                        this expression, including any semicolons */

        let mut expr_tokens = Vec::new();
        loop {
            let mut peekable_expr_tokens = next_expr_tokens.peekable();

            match peekable_expr_tokens.peek().cloned() {
                None => {
                    next_expr_tokens = Box::new(peekable_expr_tokens);
                    break
                }

                Some(next_token) => {
                    if Matcher::Keyword(keywords::Begin).is_match(&next_token) {
                        let expr_inner_block = Matcher::Keyword(keywords::Begin)
                            .terminated_by(Matcher::Keyword(keywords::End))
                            .match_block(peekable_expr_tokens, &last_expr_token)?;

                        expr_tokens.push(expr_inner_block.value.open.clone());
                        expr_tokens.extend(expr_inner_block.value.inner.iter().cloned());
                        expr_tokens.push(expr_inner_block.value.close.clone());

                        next_expr_tokens = expr_inner_block.next_tokens;
                    } else {
                        //skip 1 because we already peeked this value
                        next_expr_tokens = Box::new(peekable_expr_tokens.skip(1));

                        if Matcher::Exact(tokens::Semicolon).is_match(&next_token) {
                            break;
                        } else {
                            expr_tokens.push(next_token);
                        }
                    }
                }
            }
        }

        if expr_tokens.len() == 0 {
            Ok(ParseOutput::new(None, last_expr_token, next_expr_tokens))
        }
        else {
            last_expr_token = expr_tokens[expr_tokens.len() - 1].clone();
            let expr = Expression::parse(expr_tokens, context)?;

            Ok(ParseOutput::new(Some(expr), last_expr_token, next_expr_tokens))
        }
    }

    pub fn parse<TIter>(tokens: TIter, context: &TIter::Item) -> ParseResult<Block, TIter::Item>
        where TIter: IntoIterator + 'static,
              TIter::Item: tokens::AsToken + 'static
    {
        let block_pair = Matcher::Keyword(keywords::Begin)
            .terminated_by(Matcher::Keyword(keywords::End))
            .match_block(tokens, context)?;

        let mut next_expr_tokens: Box<Iterator<Item=TIter::Item>> = Box::new(block_pair.value.inner.into_iter());
        let mut last_expr_token = block_pair.value.open;

        let mut statements = Vec::new();
        loop {
            let try_peek_nested_begin = Matcher::Keyword(keywords::Begin)
                .match_peek(next_expr_tokens, &last_expr_token);

            match try_peek_nested_begin {
                //reached end of block
                Err(ref eof_err) if eof_err.is_eof() => {
                    break;
                }

                //other failure
                Err(ref err) => return Err(err.clone()),

                Ok(nested_begin) => match nested_begin.value {
                    //got a nested block, parse it as a new block expression
                    Some(_) => {
                        let nested_block = Block::parse(nested_begin.next_tokens,
                                                        &nested_begin.last_token)?;

                        statements.push(Expression::block(nested_block.value));

                        next_expr_tokens = nested_block.next_tokens;
                        last_expr_token = nested_block.last_token;
                    }

                    //not a nested block, parse one expr until next semicolon
                    None => {
                        let parsed_expr = Block::parse_block_expr(nested_begin.next_tokens, &last_expr_token)?;
                        parsed_expr.value.map(|expr| statements.push(expr));

                        next_expr_tokens = parsed_expr.next_tokens;
                        last_expr_token = parsed_expr.last_token;
                    }
                }
            }
        }

        let block = Block {
            statements
        };

        Ok(ParseOutput::new(block, block_pair.last_token, WrapIter::new(block_pair.next_tokens)))
    }
}

impl ToSource for Block {
    fn to_source(&self) -> String {
        let mut lines = Vec::new();
        lines.push("begin".to_owned());

        for (i, statement) in self.statements.iter().enumerate() {
            let mut line = statement.to_source();
            if i < self.statements.len() - 1 {
                line = line + ";";
            }

            lines.push(format!("\t{}", line));
        }

        lines.push("end".to_string()).to_owned();
        lines.join("\n")
    }
}