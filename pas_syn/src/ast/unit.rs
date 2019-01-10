use {
    std::{
        fmt,
    },
    crate::{
        TokenStream,
        Generate,
        Separator,
        ast::*,
    }
};

#[derive(Clone, Debug)]
pub struct Unit<A: Annotation> {
    pub init: Vec<Statement<A>>,
}

impl Unit<Span> {
    pub fn parse(tokens: &mut TokenStream) -> ParseResult<Self> {
        let statements = tokens.match_separated(Separator::Semicolon, |_i, tokens: &mut TokenStream| {
            if tokens.look_ahead().match_one(statement_start_matcher()).is_none() {
                return Ok(Generate::Break);
            }

            let stmt = Statement::parse(tokens)?;
            Ok(Generate::Yield(stmt))
        })?;

        Ok(Unit {
            init: statements
        })
    }
}

impl<A: Annotation> fmt::Display for Unit<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for init_stmt in &self.init {
            writeln!(f, "{};", init_stmt)?;
        }
        Ok(())
    }
}