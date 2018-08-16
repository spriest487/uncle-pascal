use syntax::*;
use tokens::{self, AsToken};
use keywords;
use node;

pub type Block = node::Block<ParsedContext>;

impl Parse for Block {
    fn parse(tokens: &mut TokenStream) -> ParseResult<Self> {
        let begin = tokens.match_one(keywords::Begin)?;

        let mut statements = Vec::new();
        loop {
            match tokens.look_ahead().next() {
                Some(ref t) if t.is_keyword(keywords::End) => {
                    //done
                    tokens.advance(1);
                    break;
                }

                Some(ref t) if t.is_token(&tokens::Semicolon) => {
                    //empty statement
                    tokens.advance(1);
                }

                _ => {
                    let next_expr: Expression = tokens.parse()?;
                    tokens.match_or_endl(tokens::Semicolon)?;

                    statements.push(next_expr);
                }
            }
        }

        Ok(Block {
            statements,
            context: begin.into(),
        })
    }
}
