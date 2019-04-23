use crate::{prelude::*, translate_block, translate_call, translate_expr};
use pas_syn::ast;
use pas_typecheck as pas_ty;

pub fn translate_stmt(stmt: &pas_ty::ast::Statement, builder: &mut Builder) {
    builder.comment(stmt);

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

        ast::Statement::Assignment(assignment) => {
            translate_assignment(assignment, builder);
        }

        ast::Statement::If(if_stmt) => {
            translate_if_stmt(if_stmt, builder);
        }
    }
}

fn translate_binding(binding: &pas_ty::ast::LocalBinding, builder: &mut Builder) {
    let bound_ty = builder.metadata.translate_type(&binding.val_ty);

    let binding_ref = builder.local_new(bound_ty.clone(), Some(binding.name.to_string()));

    builder.begin_scope();
    let init_val = translate_expr(&binding.val, builder);

    builder.mov(binding_ref.clone(), init_val);
    builder.retain(binding_ref, &bound_ty);

    builder.end_scope();
}

pub fn translate_for_loop(for_loop: &pas_ty::ast::ForLoop, builder: &mut Builder) {
    builder.begin_scope();

    // counter
    let counter_ty = builder
        .metadata
        .translate_type(&for_loop.init_binding.val_ty);
    if counter_ty != Type::I32 {
        unimplemented!("non-i32 counters");
    }

    assert_eq!(Type::I32, counter_ty, "counter type must be i32");
    assert!(
        !for_loop.init_binding.val.annotation.ty().is_rc(),
        "counter type must not be ref counted"
    );

    let inc_val = Value::LiteralI32(1);
    let loop_val = builder.local_temp(Type::Bool);

    let counter_val = builder.local_new(counter_ty, Some(for_loop.init_binding.name.to_string()));
    let init_val = translate_expr(&for_loop.init_binding.val, builder);

    let to_val = translate_expr(&for_loop.to_expr, builder);

    builder.mov(counter_val.clone(), init_val);

    let loop_top = builder.alloc_label();

    builder.append(Instruction::Label(loop_top));

    builder.begin_scope();
    translate_stmt(&for_loop.body, builder);
    builder.end_scope();

    // if not ++loop_counter > loop_val then jmp to loop_top
    builder.append(Instruction::Add {
        out: counter_val.clone(),
        a: Value::Ref(counter_val.clone()),
        b: inc_val,
    });

    builder.append(Instruction::Gt {
        out: loop_val.clone(),
        a: Value::Ref(counter_val),
        b: to_val.into(),
    });
    builder.append(Instruction::Not {
        out: loop_val.clone(),
        a: Value::Ref(loop_val.clone()),
    });
    builder.append(Instruction::JumpIf {
        test: Value::Ref(loop_val),
        dest: loop_top,
    });

    builder.end_scope();
}

pub fn translate_assignment(assignment: &pas_ty::ast::Assignment, builder: &mut Builder) {
    let lhs = translate_expr(&assignment.lhs, builder);
    let rhs = translate_expr(&assignment.rhs, builder);

    // the new value is being stored in a new location, retain it
    let rhs_ty = builder
        .metadata
        .translate_type(assignment.rhs.annotation.ty());
    builder.retain(rhs.clone(), &rhs_ty);

    // the old value is being replaced, release it
    let lhs_ty = builder
        .metadata
        .translate_type(assignment.lhs.annotation.ty());
    builder.release(lhs.clone(), &lhs_ty);

    builder.append(Instruction::Move {
        out: lhs,
        new_val: rhs.into(),
    });
}

fn translate_if_stmt(if_stmt: &pas_ty::ast::IfStatement, builder: &mut Builder) {
    builder.begin_scope();
    let then_label = builder.alloc_label();
    let end_label = builder.alloc_label();
    let else_label = if_stmt.else_branch.as_ref().map(|_| builder.alloc_label());

    let test_val = translate_expr(&if_stmt.cond, builder);
    builder.append(Instruction::JumpIf {
        test: test_val.into(),
        dest: then_label,
    });
    if let Some(else_label) = else_label {
        builder.append(Instruction::Jump { dest: else_label });
    } else {
        builder.append(Instruction::Jump { dest: end_label });
    }

    builder.append(Instruction::Label(then_label));
    builder.begin_scope();
    translate_stmt(&if_stmt.then_branch, builder);
    builder.end_scope();
    builder.append(Instruction::Jump { dest: end_label });

    if let Some(else_branch) = &if_stmt.else_branch {
        builder.append(Instruction::Label(else_label.unwrap()));
        builder.begin_scope();
        translate_stmt(else_branch, builder);
        builder.end_scope();
    }

    builder.append(Instruction::Label(end_label));

    builder.end_scope();
}
