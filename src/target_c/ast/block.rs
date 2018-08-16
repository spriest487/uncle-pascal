use std::fmt;

use semantic;
use target_c::ast::{
    TranslationUnit,
    TranslationResult,
    Expression,
    Variable,
};
use node::ExpressionValue;

#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    pub statements: Vec<Expression>,
}

impl Block {
    pub fn new(statements: Vec<Expression>) -> Self {
        Block { statements }
    }

    pub fn translate(block: &semantic::Block,
                     locals: Option<&[semantic::VarDecl]>,
                     unit: &mut TranslationUnit)
                     -> TranslationResult<Self> {
        let mut statements = Vec::new();
        for stmt in block.statements.iter() {
            statements.extend(Expression::translate_statement(stmt, unit)?);
        }

        // release all rc vars bound locally in this block
        for stmt in block.statements.iter() {
            if let ExpressionValue::LetBinding { name, value } = &stmt.value {
                if value.expr_type().unwrap().unwrap().is_class() {
                    statements.push(Expression::Raw({
                        format!("System_Internal_Rc_Release({})", name)
                    }));
                }
            }
        }

        if let Some(locals) = locals {
            // declare local vars
            let mut init = Vec::new();
            for decl in locals.iter() {
                init.push(Variable::translate(decl, true, unit)?
                    .decl_statement())
            }
            init.extend(statements.into_iter());
            statements = init;

            // release all rc local vars for this block
            for decl in locals.iter().rev().filter(|decl| decl.decl_type.is_class()) {
                statements.push(Expression::Raw(
                    format!("System_Internal_Rc_Release({})", decl.name)
                ));
            }
        }

        Ok(Block {
            statements
        })
    }

    pub fn write(&self, mut out: impl fmt::Write) -> fmt::Result {
        writeln!(out, "{{")?;
        for stmt in self.statements.iter() {
            stmt.write(&mut out)?;
            writeln!(out, ";")?;

        }
        writeln!(out, "}}")
    }
}