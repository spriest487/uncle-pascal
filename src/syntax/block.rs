use syntax::*;
use types;

pub struct VarDecl {
    name: String,
    decl_type: types::Identifier,
}

pub struct Block {
    vars: Vec<VarDecl>,
}

impl Block {
    pub fn parse<TIter, TToken>(_tokens: TIter) -> ParseResult<Block, TToken> {
        unimplemented!()
    }
}

