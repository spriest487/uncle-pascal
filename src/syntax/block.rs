use syntax::*;
use tokens;
use ToSource;

#[derive(Clone, Debug)]
pub struct Block {
    pub statements: Vec<Expression>
}

pub struct ParsedBlock<TToken> {
    pub begin: TToken,
    pub end: TToken,
    pub block: Block,
}

impl Block {
    pub fn parse<TIter>(tokens: TIter, context: &TIter::Item) -> ParseResult<ParsedBlock<TIter::Item>, TIter::Item>
        where TIter: IntoIterator + 'static,
              TIter::Item: tokens::AsToken + 'static
    {
        let (block_pair, block_last, remaining) = Matcher::Keyword(keywords::Begin)
            .paired_with(Matcher::Keyword(keywords::End))
            .match_pair(tokens, context)?
            .unwrap();

        let match_separator = Matcher::Exact(tokens::Semicolon);
        let statements = block_pair.inner.split(|t| match_separator.is_match(t))
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

        Ok(ParseOutput::new(ParsedBlock{
            begin: block_pair.open,
            end: block_pair.close,
            block
        }, block_last, remaining))
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