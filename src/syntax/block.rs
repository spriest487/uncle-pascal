use syntax::*;
use tokens;
use keywords;
use node;
use source;

pub type Block = node::Block<node::Identifier>;

impl Block {
    pub fn parse<TIter>(tokens: TIter, context: &source::Token) -> ParseResult<Block>
        where TIter: IntoIterator<Item=source::Token> + 'static
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
            statements,
            context: context.clone(),
        };

        Ok(ParseOutput::new(block,
                            statement_groups.last_token,
                            statement_groups.next_tokens))
    }
}

impl node::ToSource for Block {
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