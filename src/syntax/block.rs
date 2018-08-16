use syntax::*;
use tokens;
use ToSource;

#[derive(Clone, Debug)]
pub struct Block {
    pub statements: Vec<Expression>
}

impl Block {
    pub fn parse<TIter>(tokens: TIter, context: &TIter::Item) -> ParseResult<Block, TIter::Item>
        where TIter: IntoIterator + 'static,
              TIter::Item: tokens::AsToken + 'static
    {
        let block_pair = Matcher::Keyword(keywords::Begin)
            .terminated_by(Matcher::Keyword(keywords::End))
            .match_block(tokens, context)?;

        let mut next_expr_tokens : Box<Iterator<Item=TIter::Item>> = Box::new(block_pair.value.inner.into_iter());
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

                        statements.push(Expression::Block(nested_block.value));

                        next_expr_tokens = nested_block.next_tokens;
                        last_expr_token = nested_block.last_token;
                    }
                    //not a nested block, parse one expr until next semicolon
                    None => {
                        next_expr_tokens = nested_begin.next_tokens;

                        let expr_tokens = next_expr_tokens
                            .by_ref()
                            .take_while(|t| {
                                !Matcher::Exact(tokens::Semicolon).is_match(t)
                            })
                            .collect::<Vec<_>>();

                        if expr_tokens.len() == 0 {
                            break;
                        }

                        last_expr_token = expr_tokens[expr_tokens.len() - 1].clone();

                        let expr = Expression::parse(expr_tokens, &nested_begin.last_token)?;
                        statements.push(expr);
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