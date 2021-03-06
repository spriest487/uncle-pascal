use crate::{
    prelude::*, translate_block, translate_call, translate_expr, translate_if_cond, Builder,
};
use pas_syn::ast;
use pas_typecheck as pas_ty;

pub fn translate_stmt(stmt: &pas_ty::ast::Statement, builder: &mut Builder) {
    if stmt.as_block().is_none() && builder.opts().annotate_stmts {
        // write the first line of the statement as a comment
        let stmt_str = stmt.to_string();
        let mut comment = String::new();
        for (line_num, line) in stmt_str.lines().enumerate() {
            if line_num > 0 {
                comment.push_str("...");
                break;
            }
            comment.push_str(line);
        }

        builder.comment(&comment);
    }

    match stmt {
        ast::Statement::LocalBinding(binding) => {
            translate_binding(binding, builder);
        }

        ast::Statement::Call(call) => {
            translate_call(call, builder);
        }

        ast::Statement::Block(block) => {
            translate_block(block, builder);
        }

        ast::Statement::Exit(_) => unimplemented!(),

        ast::Statement::ForLoop(for_loop) => {
            translate_for_loop(for_loop, builder);
        }

        ast::Statement::WhileLoop(while_loop) => {
            translate_while_loop(while_loop, builder);
        }

        ast::Statement::Assignment(assignment) => {
            translate_assignment(assignment, builder);
        }

        ast::Statement::If(if_stmt) => {
            translate_if_cond(if_stmt, builder, true);
        }

        ast::Statement::Break(_) => {
            builder.break_loop();
        }

        ast::Statement::Continue(_) => {
            builder.continue_loop();
        }
    }
}

fn translate_binding(binding: &pas_ty::ast::LocalBinding, builder: &mut Builder) {
    let bound_ty = builder.translate_type(&binding.val_ty);

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

    builder.scope(|builder| {
        // counter
        let counter_ty = builder.translate_type(&for_loop.init_binding.val_ty);
        if counter_ty != Type::I32 {
            unimplemented!("non-i32 counters");
        }

        let counter_init = for_loop
            .init_binding
            .val
            .as_ref()
            .expect("for loop counter binding must have an init expr");

        assert!(
            !for_loop.init_binding.val_ty.is_rc(),
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

        builder.append(Instruction::Label(top_label));

        // break check: break_cond_val := counter_val > to_val
        builder.append(Instruction::Gt {
            a: Value::Ref(counter_val.clone()),
            b: to_val.into(),
            out: break_cond_val.clone(),
        });

        // jump to break label if loop_val
        builder.append(Instruction::JumpIf {
            test: Value::Ref(break_cond_val),
            dest: break_label,
        });

        builder.begin_loop_body_scope(continue_label, break_label);
        translate_stmt(&for_loop.body, builder);
        builder.end_loop_body_scope();

        builder.append(Instruction::Label(continue_label));

        // counter_val := counter_val + inc_val
        builder.append(Instruction::Add {
            a: Value::Ref(counter_val.clone()),
            b: inc_val,
            out: counter_val.clone(),
        });

        // return to top of loop
        builder.append(Instruction::Jump {
            dest: top_label,
        });

        builder.append(Instruction::Label(break_label));
    });
}

pub fn translate_while_loop(while_loop: &pas_ty::ast::WhileLoop, builder: &mut Builder) {
    let top_label = builder.alloc_label();
    let continue_label = builder.alloc_label();
    let break_label = builder.alloc_label();

    builder.scope(|builder| {
        let not_cond = builder.local_temp(Type::Bool);

        builder.append(Instruction::Label(top_label));

        // evaluate condition
        let cond_val = translate_expr(&while_loop.condition, builder);
        builder.append(Instruction::Not {
            a: Value::Ref(cond_val),
            out: not_cond.clone(),
        });

        // break now if condition is false
        builder.append(Instruction::JumpIf {
            test: Value::Ref(not_cond),
            dest: break_label,
        });

        // run loop body
        builder.begin_loop_body_scope(continue_label, break_label);
        translate_stmt(&while_loop.body, builder);
        builder.end_loop_body_scope();

        builder.append(Instruction::Label(continue_label));

        // start next iteration
        builder.append(Instruction::Jump { dest: top_label });
        builder.append(Instruction::Label(break_label));
    });
}

pub fn translate_assignment(assignment: &pas_ty::ast::Assignment, builder: &mut Builder) {
    let lhs = translate_expr(&assignment.lhs, builder);
    let rhs = translate_expr(&assignment.rhs, builder);

    // the new value is being stored in a new location, retain it
    let rhs_ty = builder.translate_type(assignment.rhs.annotation().ty());
    builder.retain(rhs.clone(), &rhs_ty);

    // the old value is being replaced, release it. local variables can be uninitialized,
    // or ambiguously initialized (initialized in one branch and not another), so we can't check
    // if the pointer is initialized here.
    //
    // if it's uninitialized it'll be NULL and we need to
    // handle that in the backend's release mechanism.
    //
    // the alternative would be to store an initialization flag alongside each rc variable
    let lhs_ty = builder.translate_type(assignment.lhs.annotation().ty());
    builder.release(lhs.clone(), &lhs_ty);

    builder.append(Instruction::Move {
        out: lhs,
        new_val: rhs.into(),
    });
}
