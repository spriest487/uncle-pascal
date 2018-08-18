use std::fmt;

use semantic::{
    self,
    arc_transform::{
        RcStatement,
        extract_block_rc_statements,
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
        /* any names bound inside this block will have +1 refcount if they're rc
        types, so we need to release them at the close of the block */
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
        for stmt in rc_block {
            statements.extend(Expression::translate_rc_statement(stmt, unit)?);
        }

        statements.extend(release_vals.iter().cloned().map(rc_release));

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