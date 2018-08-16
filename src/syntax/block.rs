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

        let match_separator = Matcher::Exact(tokens::Semicolon);
        let statements = block_pair.value.inner.split(|t| match_separator.is_match(t))
            .filter(|stmt_tokens| {
                stmt_tokens.len() > 0
            })
            .map(|stmt_tokens| -> Result<_, _> {
                let expr_tokens = stmt_tokens.iter().cloned().collect::<Vec<_>>();
                let expr_start = expr_tokens[0].clone();
                Expression::parse(expr_tokens, &expr_start)
            })
            .collect::<Result<Vec<_>, _>>();

        let block = Block {
            statements: statements?
        };

        Ok(ParseOutput::new(block, block_pair.last_token, block_pair.next_tokens))
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