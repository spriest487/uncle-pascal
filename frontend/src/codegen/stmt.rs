use crate::ast;
use crate::ast::ForLoopRange;
use crate::codegen::builder::jmp_exists;
use crate::codegen::expr::call;
use crate::codegen::expr::expr_to_val;
use crate::codegen::expr::translate_raise;
use crate::codegen::ir;
use crate::codegen::pattern::translate_pattern_match;
use crate::codegen::syn;
use crate::codegen::translate_block;
use crate::codegen::translate_exit;
use crate::codegen::translate_expr;
use crate::codegen::translate_if_cond_stmt;
use crate::codegen::typ;
use crate::codegen::Builder;
use crate::typ::is_builtin_string_name;
use common::span::Spanned;

pub fn translate_stmt(stmt: &typ::ast::Stmt, builder: &mut Builder) {
    builder.push_debug_context(stmt.annotation().span().clone());
    builder.comment(stmt);
    
    match stmt {
        syn::Stmt::Ident(..) => {
            unreachable!("should be turned into no-args calls during typechecking")
        }

        syn::Stmt::LocalBinding(binding) => {
            build_binding(binding, builder);
        },

        syn::Stmt::Call(call) => {
            call::build_call(call, builder);
        },

        syn::Stmt::Block(block) => {
            translate_block(block, ir::Ref::Discard, builder);
        },

        syn::Stmt::Exit(exit) => {
            translate_exit(exit, builder);
        },

        syn::Stmt::ForLoop(for_loop) => {
            build_for_loop(for_loop, builder);
        },

        syn::Stmt::WhileLoop(while_loop) => {
            translate_while_loop(while_loop, builder);
        },

        syn::Stmt::Assignment(assignment) => {
            translate_assignment(assignment, builder);
        },

        syn::Stmt::CompoundAssignment(assignment) => {
            translate_compound_assignment(assignment, builder);
        },

        syn::Stmt::If(if_stmt) => {
            translate_if_cond_stmt(if_stmt, builder);
        },

        syn::Stmt::Raise(raise) => {
            translate_raise(raise, builder);
        },

        syn::Stmt::Break(_) => {
            builder.break_loop();
        },

        syn::Stmt::Continue(_) => {
            builder.continue_loop();
        },

        syn::Stmt::Case(case) => {
            translate_case_stmt(case, builder);
        },

        syn::Stmt::Match(match_stmt) => {
            translate_match_stmt(match_stmt, builder);
        },
    }

    builder.pop_debug_context()
}

fn build_binding(binding: &typ::ast::VarBinding, builder: &mut Builder) {
    let bound_ty = builder.translate_type(&binding.ty);

    let binding_ref = builder.local_new(bound_ty.clone(), Some(binding.name.to_string()));

    if let Some(init_expr) = &binding.val {
        builder.scope(|builder| {
            let val = expr_to_val(init_expr, builder);

            builder.mov(binding_ref.clone(), val);
            builder.retain(binding_ref, &bound_ty);
        });
    };
}

pub fn build_for_loop(for_loop: &typ::ast::ForLoop, builder: &mut Builder) {
    match &for_loop.range {
        ForLoopRange::UpTo(range) => {
            build_for_loop_up_to(range, &for_loop.body, builder);
        }

        ForLoopRange::InSequence(range) => {
            build_for_loop_sequence(range, &for_loop.body, builder);
        }
    }
}

fn build_for_loop_up_to(
    range: &ast::ForLoopCounterRange<typ::Value>,
    body: &typ::ast::Stmt,
    builder: &mut Builder,
) {
    builder.scope(|builder| {
        let (counter_val, counter_init_val, counter_ty) = match &range.init {
            syn::ForLoopCounterInit::Assignment { counter, value } => {
                let counter_ty = builder.translate_type(counter.annotation().ty().as_ref());

                let counter_ref = translate_expr(counter, builder);
                let init_val = expr_to_val(&value, builder);

                (counter_ref, init_val, counter_ty)
            }

            syn::ForLoopCounterInit::Binding { name, init, ty } => {
                let counter_binding_name = name.to_string();
                let counter_ty = builder.translate_type(&ty);

                let counter_val = builder.local_new(counter_ty.clone(), Some(counter_binding_name));
                let init_val = expr_to_val(&init, builder);

                (counter_val, init_val, counter_ty)
            }
        };

        let to_val = expr_to_val(&range.to_expr, builder);

        let inc_val = match counter_ty {
            ir::Type::I8 => ir::Value::LiteralI8(1),
            ir::Type::U8 => ir::Value::LiteralU8(1),
            ir::Type::I16 => ir::Value::LiteralI16(1),
            ir::Type::U16 => ir::Value::LiteralU16(1),
            ir::Type::I32 => ir::Value::LiteralI32(1),
            ir::Type::U32 => ir::Value::LiteralU32(1),
            ir::Type::I64 => ir::Value::LiteralI64(1),
            ir::Type::U64 => ir::Value::LiteralU64(1),
            ir::Type::ISize => ir::Value::LiteralISize(1),
            ir::Type::USize => ir::Value::LiteralUSize(1),
            _ => ir::Value::LiteralI32(1),
        };

        build_for_loop_with_counter(
            counter_val,
            counter_init_val,
            inc_val,
            to_val,
            builder,
            |builder| translate_stmt(body, builder),
        );
    });
}

fn build_for_loop_with_counter<BodyFn>(
    counter_val: ir::Ref,
    counter_init_val: ir::Value,
    inc_val: ir::Value,
    high_val: ir::Value,
    builder: &mut Builder,
    body_fn: BodyFn,
) 
where
    BodyFn: FnOnce(&mut Builder)
{
    let top_label = builder.alloc_label();
    let continue_label = builder.alloc_label();
    let break_label = builder.alloc_label();
    
    let loop_instructions = builder.scope(|builder| {
        // temp value to store the result of evaluating the break condition
        let break_cond_val = builder.local_temp(ir::Type::Bool);

        builder.mov(counter_val.clone(), counter_init_val);

        builder.label(top_label);

        // break check: break_cond_val := counter_val > high_val
        builder.gt(break_cond_val.clone(), counter_val.clone(), high_val);

        // jump to break label if loop_val
        builder.jmp_if(break_label, break_cond_val);

        let body_instructions = builder.loop_body_scope(continue_label, break_label, body_fn);

        if jmp_exists(body_instructions, continue_label) {
            builder.label(continue_label);
        }

        // counter_val := counter_val + inc_val
        builder.add(counter_val.clone(), counter_val.clone(), inc_val);

        // return to top of loop
        builder.append(ir::Instruction::Jump { dest: top_label });
    });

    if jmp_exists(&loop_instructions, break_label) {
        builder.label(break_label);
    }
}

fn build_for_loop_sequence(
    range: &ast::ForLoopSequenceRange<typ::Value>,
    body: &typ::ast::Stmt,
    builder: &mut Builder
) {
    builder.scope(|builder| {
        let seq_val = translate_expr(&range.seq_expr, builder);

        let binding_ty = builder.translate_type(&range.binding_ty);
        let binding_name = range.binding_name.to_string();
        let binding_ref = builder.local_new(binding_ty.clone(), Some(binding_name));

        let counter_ref = builder.local_temp(ir::Type::I32);

        match &range.seq_expr.annotation().ty().as_ref() {
            typ::Type::Array(array_ty) => {
                let high_index = i32::try_from(array_ty.dim)
                    .expect("array dimension is out of range for i32");

                if high_index == 0 {
                    return
                }
                
                let high_val = ir::Value::LiteralI32(high_index - 1);

                let element_ty = builder.translate_type(&array_ty.element_ty);

                build_for_loop_with_counter(
                    counter_ref.clone(),
                    ir::Value::LiteralI32(0),
                    ir::Value::LiteralI32(1),
                    high_val,
                    builder,
                    |builder| {
                        let element_val = builder.local_temp(element_ty.clone());

                        builder.element_val(element_val.clone(), seq_val, counter_ref, element_ty);
                        builder.cast(binding_ref, element_val, binding_ty);

                        translate_stmt(body, builder);
                    }
                );
            },
            
            typ::Type::DynArray { element } => {
                let element_ty = builder.translate_type(element);
                let dynarray_ty = builder.translate_type(&range.seq_expr.annotation().ty());
                
                // high := seq_val.length
                let high_index_ref = builder.local_temp(ir::Type::I32);
                builder.field_val(
                    high_index_ref.clone(),
                    seq_val.clone(),
                    dynarray_ty.clone(),
                    ir::DYNARRAY_LEN_FIELD,
                    ir::Type::I32
                );
                
                // high -= 1;
                builder.sub(high_index_ref.clone(), high_index_ref.clone(), ir::Value::LiteralI32(1));

                let array_ty = element_ty.clone().ptr();
                let array_ptr = builder.field_to_val(seq_val, dynarray_ty, ir::DYNARRAY_PTR_FIELD, array_ty);

                build_for_loop_with_counter(
                    counter_ref.clone(),
                    ir::Value::LiteralI32(0),
                    ir::Value::LiteralI32(1),
                    high_index_ref.value(),
                    builder,
                    |builder| {
                        let element_ptr = builder.local_temp(element_ty.clone().ptr());
                        builder.add(element_ptr.clone(), array_ptr, counter_ref.clone());
                        builder.cast(binding_ref, element_ptr.to_deref(), binding_ty);

                        translate_stmt(body, builder);
                    }
                );
            },

            typ::Type::Class(sym) if is_builtin_string_name(sym) => {
                let char_ty = builder.translate_type(&typ::Type::from(typ::STRING_CHAR_TYPE));
                let string_ty = builder.translate_type(&range.seq_expr.annotation().ty());

                // high := str.length
                let high_index_ref = builder.local_temp(ir::Type::I32);
                builder.field_val(
                    high_index_ref.clone(),
                    seq_val.clone(),
                    string_ty.clone(),
                    ir::STRING_LEN_FIELD,
                    ir::Type::I32
                );

                // high -= 1;
                builder.sub(high_index_ref.clone(), high_index_ref.clone(), ir::Value::LiteralI32(1));

                let chars_ty = char_ty.clone().ptr();
                let chars_ptr = builder.local_temp(chars_ty.clone());
                builder.field_val(chars_ptr.clone(), seq_val, string_ty, ir::STRING_CHARS_FIELD, chars_ty);

                build_for_loop_with_counter(
                    counter_ref.clone(),
                    ir::Value::LiteralI32(0),
                    ir::Value::LiteralI32(1),
                    high_index_ref.value(),
                    builder,
                    |builder| {
                        let char_ptr = builder.local_temp(char_ty.clone().ptr());
                        builder.add(char_ptr.clone(), chars_ptr, counter_ref.clone());
                        builder.cast(binding_ref, char_ptr.to_deref(), binding_ty);

                        translate_stmt(body, builder);
                    }
                );
            },
            
            ty => {
                unimplemented!("sequence type: {}", ty)
            }
        };
    });
}

pub fn translate_while_loop(while_loop: &typ::ast::WhileLoop, builder: &mut Builder) {
    let top_label = builder.alloc_label();
    let continue_label = builder.alloc_label();
    let break_label = builder.alloc_label();

    let loop_instructions = builder.scope(|builder| {
        let not_cond = builder.local_temp(ir::Type::Bool);

        builder.comment("while-loop top");
        builder.label(top_label);

        // evaluate condition
        builder.scope(|builder| {
            let cond_val = expr_to_val(&while_loop.condition, builder);
            builder.not(not_cond.clone(), cond_val);
        });

        // break now if condition is false
        builder.jmp_if(break_label, not_cond);

        // run loop body
        let body_instructions = builder.loop_body_scope(continue_label, break_label, |builder| {
            translate_stmt(&while_loop.body, builder);
        });

        if jmp_exists(body_instructions, continue_label) {
            builder.comment("while-loop continue");
            builder.label(continue_label);
        }

        // start next iteration
        builder.jmp(top_label);
    });

    if jmp_exists(loop_instructions, break_label) {
        builder.comment("while-loop break");
        builder.label(break_label);
    }
}

pub fn translate_assignment(assignment: &typ::ast::Assignment, builder: &mut Builder) {
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

    builder.append(ir::Instruction::Move {
        out: lhs,
        new_val: rhs.into(),
    });
}

pub fn translate_compound_assignment(
    assignment: &typ::ast::CompoundAssignment,
    builder: &mut Builder,
) {
    builder.scope(|builder| {
        let lhs = translate_expr(&assignment.lhs, builder);
        let rhs = expr_to_val(&assignment.rhs, builder);

        // result type must be the same as the lhs ty_def, that's where we're storing it
        let lhs_ty = builder.translate_type(&assignment.lhs.annotation().ty());

        let op_result = builder.local_temp(lhs_ty.clone());
        match assignment.op {
            syn::CompoundAssignmentOperator::AddAssign => {
                builder.add(op_result.clone(), lhs.clone(), rhs.clone())
            },
            syn::CompoundAssignmentOperator::SubAssign => {
                builder.sub(op_result.clone(), lhs.clone(), rhs.clone())
            }
            syn::CompoundAssignmentOperator::MulAssign => {
                builder.mul(op_result.clone(), lhs.clone(), rhs.clone())
            }
            syn::CompoundAssignmentOperator::FDivAssign => {
                builder.fdiv(op_result.clone(), lhs.clone(), rhs.clone())
            }
        };

        // the new value is being stored in a new location, release the old value and retain it
        builder.retain(op_result.clone(), &lhs_ty);
        builder.release(lhs.clone(), &lhs_ty);

        builder.append(ir::Instruction::Move {
            out: lhs,
            new_val: op_result.into(),
        });
    });
}

fn translate_case_stmt(case: &typ::ast::CaseStmt, builder: &mut Builder) {
    build_case_block(case, builder, |item, builder| translate_stmt(item, builder))
}

pub fn build_case_block<Item, ItemFn>(
    case: &typ::ast::CaseBlock<Item>,
    builder: &mut Builder,
    mut translate_item: ItemFn,
) where
    ItemFn: FnMut(&Item, &mut Builder),
{
    builder.scope(|builder| {
        let cond_expr_val = expr_to_val(&case.cond_expr, builder);

        let break_label = builder.alloc_label();

        let mut branch_labels = Vec::new();
        for _ in 0..case.branches.len() {
            branch_labels.push(builder.alloc_label());
        }

        let else_label = match &case.else_branch {
            Some(_) => Some(builder.alloc_label()),
            _ => None,
        };

        // jump to the branch stmt where the actual value is equal to any of the branch values
        for (branch, branch_label) in case.branches.iter().zip(branch_labels.iter()) {
            builder.scope(|builder| {
                for case_expr in &branch.case_values {
                    let branch_val = expr_to_val(case_expr, builder);
                    let matches_cond = builder.eq_to_val(branch_val, cond_expr_val.clone());

                    builder.jmp_if(*branch_label, matches_cond);
                }
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

fn translate_match_stmt(match_stmt: &typ::ast::MatchStmt, builder: &mut Builder) {
    builder.scope(|builder| {
        let cond_expr = translate_expr(&match_stmt.cond_expr, builder);
        let cond_ty = builder.translate_type(&match_stmt.cond_expr.annotation().ty());

        let break_label = builder.alloc_label();

        let else_label = if match_stmt.else_branch.is_some() {
            Some(builder.alloc_label())
        } else {
            None
        };

        let is_skip = builder.local_temp(ir::Type::Bool);

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
