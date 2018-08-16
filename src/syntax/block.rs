use syntax::*;
use tokens;

pub struct Block {
    statements: Vec<Vec<tokens::Token>>
}

impl Block {
    pub fn parse<TIter, TToken>(_tokens: TIter) -> ParseResult<Block, TToken> {
        unimplemented!()
    }
}

