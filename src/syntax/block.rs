use syntax::*;
use tokens;
use keywords;
use node;

pub type Block = node::Block<ParsedSymbol, ParsedContext>;

impl Block {
    pub fn parse_exprs_multiline(tokens: &mut TokenStream) -> ParseResult<Vec<Expression>> {
        let mut exprs = Vec::new();

        loop {
            match tokens.peek() {
                Some(_) => {
                    let expr: Expression = tokens.parse()?;

                    /* if there's another expression in this group, it must
                    be on a different line */
                    if let Some(token_after) = tokens.peek() {
                        if token_after.location.line <= tokens.context().location.line {
                            return Err(ParseError::UnexpectedToken(token_after.clone(), None));
                        }
                    }

                    exprs.push(expr);
                }

                None => break,
            }
        }

        Ok(exprs)
    }
}

impl Parse for Block {
    fn parse(tokens: &mut TokenStream) -> ParseResult<Self> {
        let statement_groups = tokens.match_groups(keywords::Begin,
                                                   keywords::End,
                                                   tokens::Semicolon)?;

        let statements: Vec<_> = statement_groups.groups
            .into_iter()
            .map(|statement_group| {
                Block::parse_exprs_multiline(&mut TokenStream::new(statement_group.tokens,
                                                                   &statement_group.context))
            })
            .collect::<Result<_, _>>()?;

        Ok(Block {
            statements: statements.into_iter()
                .flat_map(|exprs| exprs)
                .collect(),
            context: statement_groups.open.into(),
        })
    }
}
