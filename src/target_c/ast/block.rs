use std::fmt;

use semantic;
use target_c::ast::{
    rc_release,
    TranslationUnit,
    TranslationResult,
    Expression,
    Variable,
    Name,
    RcStatement,
};

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
        let rc_block = Self::extract_block_rc_statements(block.clone());

        Self::translate_rc_block(rc_block, locals, unit)
    }

    pub(super) fn extract_block_rc_statements(block: semantic::Block) -> Vec<RcStatement> {
        block.statements.into_iter()
            .map(|block_stmt| Expression::extract_stmt_rc_bindings(block_stmt, 0))
            .collect()
    }

    pub(super) fn translate_rc_block(rc_block: Vec<RcStatement>,
                                     locals: Option<&[semantic::VarDecl]>,
                                     unit: &mut TranslationUnit)
                                     -> TranslationResult<Self> {
        /* any names bound inside this block will have +1 refcount if they're rc
        types, so we need to release them at the close of the block */
        let release_names: Vec<_> = rc_block.iter()
            .filter_map(|stmt| match stmt {
                | RcStatement::Statement { bound_name: Some(bound_name), .. } => {
                    if bound_name.bound_type.is_ref_counted() {
                        Some(bound_name.name.clone())
                    } else {
                        None
                    }
                }

                | RcStatement::Statement { .. }
                | RcStatement::Block(_) => None
            })
            .collect();

        let mut statements = Vec::new();
        for stmt in rc_block {
            statements.extend(Expression::translate_rc_statement(stmt, unit)?);
        }

        for release_name in release_names {
            statements.push(rc_release(Name::local(release_name)));
        }

        if let Some(locals) = locals {
            // declare local vars
            let mut init = Vec::new();
            for decl in locals {
                init.push(Variable::translate(decl, true, unit)?
                    .decl_statement())
            }
            init.extend(statements.into_iter());
            statements = init;

            // release all rc local vars for this block
            for decl in locals.iter().rev().filter(|decl| decl.decl_type.is_ref_counted()) {
                statements.push(rc_release(Name::local(decl.name.clone())));
            }
        }

        Ok(Block {
            statements
        })
    }

    pub fn write(&self, out: &mut fmt::Write) -> fmt::Result {
        writeln!(out, "{{")?;
        for stmt in &self.statements {
            stmt.write(out)?;
            writeln!(out, ";")?;
        }
        writeln!(out, "}}")
    }
}