use {
    crate::{
        TokenStream,
        Generate,
        Separator,
        ast::*,
    }
};

#[derive(Clone, Debug)]
pub struct Unit {
    pub init: Vec<Statement>,
}

impl Unit {
    pub fn parse(tokens: &mut TokenStream) -> ParseResult<Unit> {
        let statements = tokens.match_separated(Separator::Semicolon, |_i, tokens: &mut TokenStream| {
            let stmt = Statement::parse(tokens)?;
            Ok(Generate::Yield(stmt))
        })?;

        Ok(Unit {
            init: statements
        })
    }
}
