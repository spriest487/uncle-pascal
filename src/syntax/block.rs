use syntax::*;
use tokens;
use keywords;
use node;
use source;

pub type Block = node::Block<node::Identifier>;

impl Block {
//    fn split_statements_by_lines<TIter>(tokens: TIter) -> Vec<Vec<source::Token>>
//        where TIter: IntoIterator<Item=source::Token>
//    {
//        let mut last_line_no = None;
//
//        let mut lines = Vec::new();
//        let mut line = Vec::new();
//
//        for token in tokens.into_iter() {
//            match &last_line_no {
//                &Some(line_no) if line_no != token.location.line => {
//                    lines.push(line);
//                    line = vec![token.clone()];
//                }
//
//                _ => line.push(token.clone()),
//            }
//
//            last_line_no = Some(token.location.line);
//        }
//
//        if line.len() > 0 {
//            lines.push(line);
//        }
//
//        lines
//    }

    pub fn parse_exprs_multiline<TIter>(in_tokens: TIter, context: &source::Token)
        -> Result<Vec<Expression>, ParseError>
        where TIter: IntoIterator<Item=source::Token> + 'static
    {
        let mut exprs = Vec::new();
        let mut next_expr_tokens : Box<Iterator<Item=source::Token>> = Box::from(in_tokens.into_iter());
        let mut next_expr_context = context.clone();

        loop {
            let mut peek_start = next_expr_tokens.peekable();

            match peek_start.peek() {
                Some(_) => {
                    let expr = Expression::parse(peek_start, &next_expr_context)?;

                    /* if there's another expression in this group, it must
                    be on a different line */
                    let mut peek_after = expr.next_tokens.peekable();
//                    let mut peek_after = nexttokens.into_iter().peekable();

                    match peek_after.peek().cloned() {
                        Some(ref token_after)
                        if token_after.location.line <= expr.value.context.location.line => {
                            return Err(ParseError::UnexpectedToken(token_after.clone(),
                                                                   None));
                        }

                        //nothing after, or next token is on anther line, proceed
                        _ => {
                            next_expr_tokens = Box::from(peek_after);
                            next_expr_context = expr.last_token;
                        },
                    }

                    exprs.push(expr.value);
                }

                None => break,
            }
        }

        Ok(exprs)
    }

    pub fn parse<TIter>(tokens: TIter, context: &source::Token) -> ParseResult<Block>
        where TIter: IntoIterator<Item=source::Token> + 'static
    {
        let statement_groups = keywords::Begin.terminated_by(keywords::End)
            .match_groups(tokens::Semicolon, tokens, context)?;

        let statements: Vec<_> = statement_groups.value.groups
            .into_iter()
            .map(|statement_group| {
                Block::parse_exprs_multiline(statement_group.tokens,
                                             &statement_group.context)
            })
            .collect::<Result<_, _>>()?;

        let block = Block {
            statements: statements.into_iter()
                .flat_map(|exprs| exprs)
                .collect(),
            context: context.clone(),
        };

        Ok(ParseOutput::new(block,
                            statement_groups.last_token,
                            statement_groups.next_tokens))
    }
}

impl node::ToSource for Block {
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