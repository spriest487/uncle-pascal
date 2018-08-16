use syntax::*;
use tokens;
use tokens::AsToken;
use keywords;
use node;

pub type Program = node::Program<ParsedSymbol>;

impl Program {
    pub fn parse(mut tokens: TokenStream) -> ParseResult<Self> {
        let program_statement = tokens.match_sequence(keywords::Program
            .and_then(Matcher::AnyIdentifier))?;

        let name = program_statement[1].as_token().unwrap_identifier().to_owned();
        tokens.match_or_endl(tokens::Semicolon)?;

        let uses: Vec<node::UnitReference> = tokens.parse()?;
        let decls: Vec<UnitDeclaration> = tokens.parse()?;
        let program_block: Block = tokens.parse()?;

        tokens.match_one(tokens::Period)?;
        tokens.finish()?;

        let program = Program {
            name,
            uses,
            decls,
            program_block,
        };

        Ok(program)
    }
}
