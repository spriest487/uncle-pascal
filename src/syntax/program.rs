use syntax::*;
use source;
use tokens;
use tokens::AsToken;
use keywords;
use node;

pub type Program = node::Program<node::Identifier>;

impl Program {
    pub fn parse<TIter>(tokens: TIter,
                        context: &source::Token) -> Result<Self, ParseError>
        where TIter: Iterator<Item=source::Token> + 'static
    {
        let program_statement = keywords::Program
            .and_then(Matcher::AnyIdentifier)
            .match_sequence(tokens, context)?;

        let name = program_statement.value
            .get(1).unwrap()
            .as_token()
            .unwrap_identifier()
            .to_owned();

        let end_name = tokens::Semicolon
            .match_or_endl(program_statement.next_tokens,
                           &program_statement.last_token)?;

        let uses = Unit::parse_uses(end_name.next_tokens,
                              &end_name.last_token)?;

        let decls = Unit::parse_decls(uses.next_tokens, &uses.last_token)?;

        let program_block = Block::parse(decls.next_tokens, &decls.last_token)?;

        let _last_period = tokens::Period.match_one(program_block.next_tokens,
                                                   &program_block.last_token)?
            .finish()?;

        let program = Program {
            name,
            uses: uses.value,

            decls: decls.value,

            program_block: program_block.value,
        };

        Ok(program)
    }
}

impl node::ToSource for Program {
    fn to_source(&self) -> String {
        let mut lines = Vec::new();
        lines.push(format!("program {};", self.name));

        if self.uses.len() > 0 {
            lines.push(format!("uses {};",
                               self.uses.iter().map(|u| format!("{}", u))
                                   .collect::<Vec<_>>()
                                   .join(", ")));
        }

        for decl in self.decls.iter() {
            match decl {
                &node::UnitDeclaration::Record(ref rec_decl) =>
                    lines.push(rec_decl.to_source()),

                &node::UnitDeclaration::Function(ref func_decl) =>
                    lines.push(func_decl.to_source()),

                &node::UnitDeclaration::Vars(ref var_decls) =>
                    lines.push(var_decls.to_source()),
            }
        }

        lines.push(self.program_block.to_source() + ".");

        lines.join("\n\n")
    }
}