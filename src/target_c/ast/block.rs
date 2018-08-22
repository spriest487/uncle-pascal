use std::fmt;

use semantic::{
    self,
    arc_transform::{
        ArcStatement,
        extract_block_rc_statements,
        rc_subvalues,
    },
};
use target_c::ast::{
    TranslationUnit,
    TranslationResult,
    Expression,
    Variable,
    Name,
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
        let rc_block = extract_block_rc_statements(block.clone());

        Self::translate_rc_block(&rc_block, locals, unit)
    }

    pub(super) fn translate_rc_block(rc_block: &[ArcStatement],
                                     locals: Option<&[semantic::VarDecl]>,
                                     unit: &mut TranslationUnit)
                                     -> TranslationResult<Self> {
        /* values that are bound to a name have +1 rc after the statement they're created
        in, and at the end of the block we release them all in reverse order */
        let release_vals: Vec<_> = rc_block.iter()
            .flat_map(Expression::bound_value_exprs)
            .collect();

        let mut statements = Vec::new();

        // declare local vars
        if let Some(locals) = locals {
            for decl in locals {
                statements.push(Variable::translate(decl, true, unit)?
                    .decl_statement())
            }
        }

        // main block body
        for stmt in rc_block {
            statements.extend(Expression::translate_rc_statement(stmt, unit)?);
        }

        // release rc values of names bound in the body in reverse order
        statements.extend(release_vals.into_iter().rev()
            .map(|(rc_val, rc_strength)| Expression::rc_release(rc_val, rc_strength)));

        // release block locals in reverse decl order
        if let Some(locals) = locals {
            // todo: bug, don't release uninitialized values!

            for decl in locals.iter().rev() {
                let rc_subvals = rc_subvalues(&decl.decl_type, decl.scope(), None);
                let var_base_expr = Expression::from(Name::local(decl.name.clone()));

                for rc_val in rc_subvals.iter().rev() {
                    let val_expr = Expression::translate_rc_value_expr(
                        rc_val,
                        var_base_expr.clone(),
                    );

                    statements.push(Expression::rc_release(val_expr, rc_val.strength()));
                }
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