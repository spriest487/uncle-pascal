use std::iter;

use syntax::*;
use source;
use tokens;
use tokens::AsToken;
use keywords;
use node;

pub type Program = node::Program<ParsedSymbol>;

fn transform_blocks(mut program: Program,
                    f: impl Fn(Block) -> Block) -> Program {
    program.decls = program.decls.into_iter()
        .map(|decl| {
            let mut fn_decl = match decl {
                node::UnitDeclaration::Function(fn_decl) => fn_decl,
                _ => return decl,
            };

            fn_decl.body = f(fn_decl.body);

            node::UnitDeclaration::Function(fn_decl)
        })
        .collect();

    program.program_block = f(program.program_block);

    program
}

fn replace_block_string_literals(mut block: Block) -> Block {
    block.statements = block.statements.into_iter()
        .flat_map(|stmt| iter::once(node::transform_expressions(stmt, &|expr: Expression| {
            match expr.value {
                node::ExpressionValue::LiteralString(str) => {
                    let constructor_id = ParsedSymbol(node::Identifier::from("System.StringFromBytes"));

                    let constructor = Expression::identifier(constructor_id, expr.context.clone());
                    let literal_arg = Expression::literal_string(&str, expr.context.clone());
                    let len_arg = Expression::literal_int(str.len() as i64, expr.context);

                    Expression::function_call(constructor, vec![literal_arg, len_arg])
                }

                _ => expr
            }
        })))
        .collect();

    block
}

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

        let mut program = Program {
            name,
            uses: uses.value,

            decls: decls.value,

            program_block: program_block.value,
        };

        program = transform_blocks(program, |block| {
            replace_block_string_literals(block)
        });

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