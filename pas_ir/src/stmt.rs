use crate::expr::translate_raise;
use crate::pattern::translate_pattern_match;
use crate::{
    jmp_exists, translate_block, build_call, translate_exit, translate_expr,
    translate_if_cond_stmt, Builder, Instruction, Ref, Type, Value,
};
use pas_common::span::Spanned;
use pas_syn::ast;
use pas_typecheck as pas_ty;

pub fn translate_stmt(stmt: &pas_ty::ast::Statement, builder: &mut Builder) {
    builder.push_debug_context(stmt.annotation().span().clone());
    if builder.opts().debug_info {
        builder.comment(&stmt);
    }

    match stmt {
        ast::Statement::LocalBinding(binding) => {
            translate_binding(binding, builder);
        },

        ast::Statement::Call(call) => {
            build_call(call, builder);
        },

        ast::Statement::Block(block) => {
            translate_block(block, Ref::Discard, builder);
        },

        ast::Statement::Exit(exit) => {
            translate_exit(exit, builder);
        },

        ast::Statement::ForLoop(for_loop) => {
            translate_for_loop(for_loop, builder);
        },

        ast::Statement::WhileLoop(while_loop) => {
            translate_while_loop(while_loop, builder);
        },

        ast::Statement::Assignment(assignment) => {
            translate_assignment(assignment, builder);
        },

        ast::Statement::CompoundAssignment(assignment) => {
            translate_compound_assignment(assignment, builder);
        },

        ast::Statement::If(if_stmt) => {
            translate_if_cond_stmt(if_stmt, builder);
        },

        ast::Statement::Raise(raise) => {
            translate_raise(raise, builder);
        },

        ast::Statement::Break(_) => {
            builder.break_loop();
        },

        ast::Statement::Continue(_) => {
            builder.continue_loop();
        },

        ast::Statement::Case(case) => {
            translate_case_stmt(case, builder);
        },

        ast::Statement::Match(match_stmt) => {
            translate_match_stmt(match_stmt, builder);
        },
    }

    builder.pop_debug_context()
}

fn translate_binding(binding: &pas_ty::ast::LocalBinding, builder: &mut Builder) {
    let bound_ty = builder.translate_type(&binding.ty);

    let binding_ref = builder.local_new(bound_ty.clone(), Some(binding.name.to_string()));

    if let Some(init_expr) = &binding.val {
        builder.scope(|builder| {
            let val_ref = translate_expr(init_expr, builder);

            builder.mov(binding_ref.clone(), val_ref);
            builder.retain(binding_ref, &bound_ty);
        });
    };
}

pub fn translate_for_loop(for_loop: &pas_ty::ast::ForLoop, builder: &mut Builder) {
    let top_label = builder.alloc_label();
    let continue_label = builder.alloc_label();
    let break_label = builder.alloc_label();

    let loop_instructions = builder.scope(|builder| {
        // counter
        let counter_ty = builder.translate_type(&for_loop.init_binding.ty);
        if counter_ty != Type::I32 {
            unimplemented!("non-i32 counters");
        }

        let counter_init = for_loop
            .init_binding
            .val
            .as_ref()
            .expect("for loop counter binding must have an init expr");

        assert!(
            !for_loop.init_binding.ty.is_rc_reference(),
            "counter type must not be ref counted"
        );

        let inc_val = Value::LiteralI32(1);

        // temp value to store the result of evaluating the break condition
        let break_cond_val = builder.local_temp(Type::Bool);

        let counter_binding_name = for_loop.init_binding.name.to_string();
        let counter_val = builder.local_new(counter_ty, Some(counter_binding_name));
        let init_val = translate_expr(&counter_init, builder);

        let to_val = translate_expr(&for_loop.to_expr, builder);

        builder.mov(counter_val.clone(), init_val);

        builder.label(top_label);

        // break check: break_cond_val := counter_val > to_val
        builder.gt(break_cond_val.clone(), counter_val.clone(), to_val);

        // jump to break label if loop_val
        builder.jmp_if(break_label, break_cond_val);

        let body_instructions = builder.loop_body_scope(continue_label, break_label, |builder| {
            translate_stmt(&for_loop.body, builder);
        });

        if jmp_exists(body_instructions, continue_label) {
            builder.label(continue_label);
        }

        // counter_val := counter_val + inc_val
        builder.add(counter_val.clone(), counter_val.clone(), inc_val);

        // return to top of loop
        builder.append(Instruction::Jump { dest: top_label });
    });

    if jmp_exists(&loop_instructions, break_label) {
        builder.label(break_label);
    }
}

pub fn translate_while_loop(while_loop: &pas_ty::ast::WhileLoop, builder: &mut Builder) {
    let top_label = builder.alloc_label();
    let continue_label = builder.alloc_label();
    let break_label = builder.alloc_label();

    let loop_instructions = builder.scope(|builder| {
        let not_cond = builder.local_temp(Type::Bool);

        builder.label(top_label);

        // evaluate condition
        builder.scope(|builder| {
            let cond_val = translate_expr(&while_loop.condition, builder);
            builder.not(not_cond.clone(), cond_val);
        });

        // break now if condition is false
        builder.jmp_if(break_label, not_cond);

        // run loop body
        let body_instructions = builder.loop_body_scope(continue_label, break_label, |builder| {
            translate_stmt(&while_loop.body, builder);
        });

        if jmp_exists(body_instructions, continue_label) {
            builder.label(continue_label);
        }

        // start next iteration
        builder.jmp(top_label);
    });

    if jmp_exists(loop_instructions, break_label) {
        builder.label(break_label);
    }
}

pub fn translate_assignment(assignment: &pas_ty::ast::Assignment, builder: &mut Builder) {
    let lhs = translate_expr(&assignment.lhs, builder);
    let rhs = translate_expr(&assignment.rhs, builder);

    // the new value is being stored in a new location, retain it
    let rhs_ty = builder.translate_type(&assignment.rhs.annotation().ty());
    builder.retain(rhs.clone(), &rhs_ty);

    // the old value is being replaced, release it. local variables can be uninitialized,
    // or ambiguously initialized (initialized in one branch and not another), so we can't check
    // if the pointer is initialized here.
    //
    // if it's uninitialized it'll be NULL and we need to
    // handle that in the backend's release mechanism.
    //
    // the alternative would be to store an initialization flag alongside each rc variable
    let lhs_ty = builder.translate_type(&assignment.lhs.annotation().ty());
    builder.release(lhs.clone(), &lhs_ty);

    builder.append(Instruction::Move {
        out: lhs,
        new_val: rhs.into(),
    });
}

pub fn translate_compound_assignment(
    assignment: &pas_ty::ast::CompoundAssignment,
    builder: &mut Builder,
) {
    builder.scope(|builder| {
        let lhs = translate_expr(&assignment.lhs, builder);
        let rhs = translate_expr(&assignment.rhs, builder);

        // result type must be the same as the lhs ty, that's where we're storing it
        let lhs_ty = builder.translate_type(&assignment.lhs.annotation().ty());

        let op_result = builder.local_temp(lhs_ty.clone());
        match assignment.op {
            pas_syn::CompoundAssignmentOperator::AddAssign => {
                builder.add(op_result.clone(), lhs.clone(), rhs.clone())
            },
            pas_syn::CompoundAssignmentOperator::SubAssign => {
                builder.sub(op_result.clone(), lhs.clone(), rhs.clone())
            }
            pas_syn::CompoundAssignmentOperator::MulAssign => {
                builder.mul(op_result.clone(), lhs.clone(), rhs.clone())
            }
            pas_syn::CompoundAssignmentOperator::FDivAssign => {
                builder.idiv(op_result.clone(), lhs.clone(), rhs.clone())
            }
        };

        // the new value is being stored in a new location, release the old value and retain it
        builder.retain(op_result.clone(), &lhs_ty);
        builder.release(lhs.clone(), &lhs_ty);

        builder.append(Instruction::Move {
            out: lhs,
            new_val: op_result.into(),
        });
    });
}

fn translate_case_stmt(case: &pas_ty::ast::CaseStatement, builder: &mut Builder) {
    build_case_block(case, builder, |item, builder| translate_stmt(item, builder))
}

pub fn build_case_block<Item, ItemFn>(
    case: &pas_ty::ast::CaseBlock<Item>,
    builder: &mut Builder,
    mut translate_item: ItemFn,
) where
    ItemFn: FnMut(&Item, &mut Builder),
{
    builder.scope(|builder| {
        let cond_expr_val = translate_expr(&case.cond_expr, builder);

        let break_label = builder.alloc_label();

        let mut branch_labels = Vec::new();
        for _ in 0..case.branches.len() {
            branch_labels.push(builder.alloc_label());
        }

        let else_label = match &case.else_branch {
            Some(_) => Some(builder.alloc_label()),
            _ => None,
        };

        // jump to the branch statement where the actual value is equal to the branch value
        for (branch, branch_label) in case.branches.iter().zip(branch_labels.iter()) {
            builder.scope(|builder| {
                let branch_val = translate_expr(&branch.value, builder);

                let cond_eq = builder.local_temp(Type::Bool);

                builder.eq(cond_eq.clone(), branch_val, cond_expr_val.clone());
                builder.jmp_if(*branch_label, cond_eq);
            });
        }

        if let Some(else_label) = else_label {
            builder.jmp(else_label);
        } else {
            builder.jmp(break_label);
        }

        // write the branch statements after their respective labels
        for (branch, branch_label) in case.branches.iter().zip(branch_labels.iter()) {
            builder.label(*branch_label);

            builder.scope(|builder| {
                translate_item(&branch.item, builder);
            });

            builder.jmp(break_label);
        }
        if let (Some(else_item), Some(else_label)) = (&case.else_branch, else_label) {
            builder.label(else_label);

            builder.scope(|builder| {
                translate_item(&else_item, builder);
            });

            builder.jmp(break_label);
        }

        builder.label(break_label);
    });
}

fn translate_match_stmt(match_stmt: &pas_ty::ast::MatchStmt, builder: &mut Builder) {
    builder.scope(|builder| {
        let cond_expr = translate_expr(&match_stmt.cond_expr, builder);
        let cond_ty = builder.translate_type(&match_stmt.cond_expr.annotation().ty());

        let break_label = builder.alloc_label();

        let else_label = if match_stmt.else_branch.is_some() {
            Some(builder.alloc_label())
        } else {
            None
        };

        let is_skip = builder.local_temp(Type::Bool);

        for branch in &match_stmt.branches {
            builder.scope(|builder| {
                // label to skip this branch if it isn't a match
                let skip_label = builder.alloc_label();

                let pattern_match =
                    translate_pattern_match(&branch.pattern, &cond_expr, &cond_ty, builder);

                // jump to skip label if pattern match return false
                builder.not(is_skip.clone(), pattern_match.is_match.clone());
                builder.jmp_if(skip_label, is_skip.clone());

                // code to run if we didn't skip - the actual branch
                builder.scope(|builder| {
                    for binding in pattern_match.bindings {
                        binding.bind_local(builder);
                    }

                    translate_stmt(&branch.item, builder);
                });

                // only one branch must run so break out of the block now
                builder.jmp(break_label);

                builder.label(skip_label);
            });
        }

        // write the else branch - will fall through to here if we didn't run any branches
        if let Some(else_branch) = &match_stmt.else_branch {
            builder.scope(|builder| {
                builder.label(else_label.unwrap());

                translate_stmt(else_branch, builder);
            });
        }

        builder.label(break_label);
    });
}
