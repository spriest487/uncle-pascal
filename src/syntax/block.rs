use syntax::*;
use tokens;
use keywords;
use node;
use ToSource;

pub type Block = node::Block<node::Identifier>;

impl Block {
    pub fn parse<TIter>(tokens: TIter, context: &TIter::Item) -> ParseResult<Block, TIter::Item>
        where TIter: IntoIterator + 'static,
              TIter::Item: tokens::AsToken + 'static
    {
        let statement_groups = keywords::Begin.terminated_by(keywords::End)
            .match_groups(tokens::Semicolon, tokens, context)?;

        let statements = statement_groups.value.groups
            .into_iter()
            .map(|statement_group| {
                Expression::parse(statement_group.tokens, &statement_group.context)
            })
            .collect::<Result<_, _>>()?;

        let block = Block {
            statements
        };

        Ok(ParseOutput::new(block,
                            statement_groups.last_token,
                            statement_groups.next_tokens))
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