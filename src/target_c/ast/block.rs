use std::fmt;

use semantic::{
    self,
    arc_transform::{
        RcStatement,
        extract_block_rc_statements,
        rc_subvalues,
    }
};
use target_c::ast::{
    rc_release,
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

        Self::translate_rc_block(rc_block, locals, unit)
    }

    pub(super) fn translate_rc_block(rc_block: Vec<RcStatement>,
                                     locals: Option<&[semantic::VarDecl]>,
                                     unit: &mut TranslationUnit)
                                     -> TranslationResult<Self> {
        /* values that are bound to a name have +1 rc after the statement they're created
        in, and at the end of the block we release them all in reverse order */
        let release_vals = {
            let mut release_vals = Vec::new();

            let bound_rc_vals: Vec<_> = rc_block.iter()
                .filter_map(|stmt| match stmt {
                    | RcStatement::Statement { bound_name: Some(name), rc_bindings, .. } =>
                        Some((name.name.as_str(), rc_bindings)),

                    | RcStatement::Statement { bound_name: None, .. }
                    | RcStatement::Block(_) => None
                })
                .collect();

            for (bound_name, rc_bindings) in bound_rc_vals {
                for rc_binding in rc_bindings {
                    let base = Expression::from(Name::local(bound_name));

                    release_vals.extend(rc_binding.rc_subvalues.iter()
                        .map(|subval| {
                            Expression::translate_rc_value_expr(subval, base.clone())
                        }));
                }
            }
            release_vals
        };

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
        statements.extend(release_vals.into_iter().rev().map(rc_release));

        // release block locals in reverse decl order
        if let Some(locals) = locals {
            // todo: bug, don't release uninitialized values!

            for decl in locals.iter().rev() {
                let rc_subvals = rc_subvalues(&decl.decl_type, decl.scope(), None);
                let var_base_expr = Expression::from(Name::local(decl.name.clone()));

                for rc_val in rc_subvals.into_iter().rev() {
                    statements.push(rc_release(Expression::translate_rc_value_expr(
                        &rc_val,
                        var_base_expr.clone()
                    )));
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