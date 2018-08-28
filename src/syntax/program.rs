use syntax::*;
use tokens;
use tokens::AsToken;
use keywords;
use node;

pub type Program = node::Program<ParsedContext>;

impl Program {
    pub fn parse(mut tokens: TokenStream, expected_name: &str) -> ParseResult<Self> {
        let name = match tokens.look_ahead().match_one(keywords::Program) {
            Some(_) => {
                tokens.advance(1);
                let name_token = tokens.match_one(Matcher::AnyIdentifier)?;
                tokens.match_or_endl(tokens::Semicolon)?;
                let parsed_name = name_token.unwrap_identifier();

                if parsed_name != expected_name {
                    eprintln!(
                        "program name `{}` @ `{}` does not match expected name `{}`",
                        parsed_name,
                        name_token,
                        expected_name
                    )
                }

                parsed_name.to_string()
            }
            None => expected_name.to_string(),
        };

        let uses: Vec<UnitReference> = tokens.parse()?;
        let decls: Vec<Implementation> = tokens.parse()?;
        let program_block: Block = tokens.parse()?;

        tokens.match_or_endl(tokens::Period)?;
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
