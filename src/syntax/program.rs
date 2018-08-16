use std::iter;

use syntax::*;
use tokens;
use tokens::AsToken;
use consts::IntConstant;
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

            fn_decl.body = fn_decl.body.map(|mut body| {
                body.block = f(body.block);
                body
            });

            node::UnitDeclaration::Function(fn_decl)
        })
        .collect();

    program.program_block = f(program.program_block);

    program
}

fn replace_block_string_literals(mut block: Block) -> Block {
    block.statements = block.statements.into_iter()
        .flat_map(|stmt| iter::once(node::transform_expressions(stmt, &mut |expr: Expression| {
            match expr.value {
                node::ExpressionValue::LiteralString(str) => {
                    let constructor_id = ParsedSymbol(node::Identifier::from("System.StringFromBytes"));

                    let constructor = Expression::identifier(constructor_id, expr.context.clone());
                    let literal_arg = Expression::literal_string(&str, expr.context.clone());
                    let len_val = IntConstant::I32(str.len() as i32);
                    let len_arg = Expression::literal_int(len_val, expr.context);

                    Expression::function_call(constructor, vec![literal_arg, len_arg])
                }

                _ => expr
            }
        })))
        .collect();

    block
}

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

        let mut program = Program {
            name,
            uses,
            decls,
            program_block,
        };

        program = transform_blocks(program, |block| {
            replace_block_string_literals(block)
        });

        Ok(program)
    }
}
