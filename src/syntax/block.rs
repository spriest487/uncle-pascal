use syntax::*;
use tokens;
use keywords;
use node;

pub type Block = node::Block<ParsedContext>;

impl Block {
    pub fn parse_statements(tokens: &mut TokenStream,
                            terminator: impl Into<Matcher>)
                            -> ParseResult<Vec<Expression>> {
        let terminator = terminator.into();
        let mut statements = Vec::new();
        loop {
            // handle empty statements
            while tokens.look_ahead().match_one(tokens::Semicolon).is_some() {
                tokens.advance(1);
            }

            match tokens.look_ahead().next() {
                Some(ref t) if terminator.is_match(t) => {
                    //done
                    tokens.advance(1);
                    break;
                }

                _ => {
                    if !statements.is_empty() {
                        tokens.match_or_endl(tokens::Semicolon)?;
                    }

                    let next_expr: Expression = tokens.parse()?;

                    statements.push(next_expr);
                }
            }
        }

        Ok(statements)
    }
}

impl Parse for Block {
    fn parse(tokens: &mut TokenStream) -> ParseResult<Self> {
        let begin = tokens.match_one(keywords::Begin)?;

        let statements = Block::parse_statements(tokens, keywords::End)?;

        Ok(Block {
            statements,
            context: begin.into(),
        })
    }
}
