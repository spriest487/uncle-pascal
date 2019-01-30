use {
    crate::{
        prelude::*,
        translate_expr,
        translate_block,
        translate_call,
    },
    pas_syn::ast as ast,
    pas_typecheck as pas_ty,
};

pub fn translate_stmt(stmt: &pas_ty::ast::Statement, builder: &mut Builder) {
    match stmt {
        ast::Statement::LocalBinding(binding) => {
            let binding_val_ty = binding.val_ty.as_ref().unwrap();
            let bound_ty = builder.metadata.translate_type(binding_val_ty);

            let rc = binding_val_ty.is_rc();
            let binding_id = builder.new_local(bound_ty, rc, Some(binding.name.to_string()));

            builder.begin_scope();
            let init_val = translate_expr(&binding.val, builder);

            if binding.val.annotation.ty.is_rc() {
                let init_val_ref = init_val.as_ref_val()
                    .expect("rc pointer always exists at referenceable location");
                builder.append(Instruction::Retain(init_val_ref.clone()));
            }

            builder.append(Instruction::Set { out: binding_id, new_val: init_val });


            builder.end_scope();
        }

        ast::Statement::Call(call) => {
            translate_call(call, builder);
        }

        ast::Statement::Block(block) => {
            let block_out = translate_block(block, builder);
            assert!(block_out.is_none(), "block statement should not produce a value");
        }

        ast::Statement::Exit(_) => {
            unimplemented!()
        }

        ast::Statement::ForLoop(for_loop) => {
            translate_for_loop(for_loop, builder);
        }

        ast::Statement::Assignment(assignment) => {
            translate_assignment(assignment, builder);
        }
    }
}

pub fn translate_for_loop(for_loop: &pas_ty::ast::ForLoop, builder: &mut Builder) {
    builder.begin_scope();

    // counter
    let counter_ty = builder.metadata.translate_type(for_loop.init_binding.val_ty.as_ref().unwrap());
    if counter_ty !=  Type::I32 {
        unimplemented!("non-i32 counters");
    }

    assert_eq!(Type::I32, counter_ty, "counter type must be i32");
    assert!(!for_loop.init_binding.val.annotation.ty.is_rc(), "counter type must not be ref counted");

    let inc_val = Value::LiteralI32(1);
    let loop_val = builder.new_local(Type::Bool, false, None);

    let counter_val = builder.new_local(counter_ty, false, Some(for_loop.init_binding.name.to_string()));
    let init_val = translate_expr(&for_loop.init_binding.val, builder);

    let to_val = translate_expr(&for_loop.to_expr, builder);

    builder.append(Instruction::Set { out: counter_val.clone(), new_val: init_val });

    let loop_top = builder.alloc_label();

    builder.append(Instruction::Label(loop_top));

    translate_stmt(&for_loop.body, builder);

    // if not ++loop_counter > loop_val then jmp to loop_top
    builder.append(Instruction::Add { out: counter_val.clone(), a: Value::Ref(counter_val.clone()), b: inc_val });

    builder.append(Instruction::Gt { out: loop_val.clone(), a: Value::Ref(counter_val), b: to_val });
    builder.append(Instruction::Not { out: loop_val.clone(), a: Value::Ref(loop_val.clone()) });
    builder.append(Instruction::JumpIf { test: Value::Ref(loop_val), dest: loop_top });

    builder.end_scope();
}

pub fn translate_assignment(assignment: &pas_ty::ast::Assignment, builder: &mut Builder) {
    let lhs_ref = match translate_expr(&assignment.lhs, builder) {
        Value::Ref(lhs_ref) => lhs_ref,
        _ => panic!("lhs of assignment op must be a referenceable value"),
    };

    let rhs = translate_expr(&assignment.rhs, builder);

    if assignment.lhs.annotation.ty.is_rc() {
        let rhs_ref = rhs.as_ref_val()
            .expect("rc pointer values always exist at referenceable locations");
        builder.append(Instruction::Retain(rhs_ref.clone()));

        builder.append(Instruction::Release(lhs_ref.clone()));
    }

    builder.append(Instruction::Set { out: lhs_ref, new_val: rhs });
}

