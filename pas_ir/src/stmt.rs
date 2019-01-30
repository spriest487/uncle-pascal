use {
    crate::{
        prelude::*,
        translate_expr,
        translate_stmt,
    },
    pas_typecheck as pas_ty,
};

pub fn translate_for_loop(for_loop: &pas_ty::ast::ForLoop, builder: &mut Builder) {
    builder.begin_scope();

    // counter
    let counter_ty = builder.metadata.translate_type(&for_loop.init_binding.val_ty);
    if counter_ty !=  Type::I32 {
        unimplemented!("non-i32 counters");
    }

    let inc_val = Value::LiteralI32(1);
    let loop_val = builder.new_local(Type::Bool, None);

    let counter_val = builder.new_local(counter_ty, Some(for_loop.init_binding.name.to_string()));
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