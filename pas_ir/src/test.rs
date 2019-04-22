use super::*;

#[test]
fn end_loop_scope_ends_at_right_scope_level() {
    let mut metadata = Metadata::new();
    let mut builder = Builder::new(&mut metadata);

    let initial_scope = builder.scopes.len();

    let continue_label = builder.alloc_label();
    let break_label = builder.alloc_label();
    builder.begin_loop_body_scope(continue_label, break_label);
    builder.end_loop_body_scope();

    assert_eq!(initial_scope, builder.scopes.len());
}

#[test]
fn break_cleans_up_loop_locals() {
    let mut metadata = Metadata::new();
    let mut builder = Builder::new(&mut metadata);

    let continue_label = builder.alloc_label();
    let break_label = builder.alloc_label();

    builder.begin_loop_body_scope(continue_label, break_label);
    builder.local_new(Type::RcPointer(None), Some("local1".to_string()));
    builder.local_new(Type::RcPointer(None), Some("local2".to_string()));

    builder.comment("before_break");
    builder.break_loop();

    let from = builder.instructions.iter()
        .position(|i| match i {
            Instruction::Comment(c) => c == "before_break",
            _ => false
        })
        .unwrap();

    // Both locals should be freed, which since they're RC variables means each one should
    // be release then have NULL mov'd into it. Because the implementation of `release` uses
    // `visit_deep` it'll create a block around each one too.
    let expect = &[
        Instruction::LocalBegin,
        Instruction::Release { at: Ref::Local(LocalID(1))},
        Instruction::Move { out: Ref::Local(LocalID(1)), new_val: Value::LiteralNull, },
        Instruction::LocalEnd,

        Instruction::LocalBegin,
        Instruction::Release { at: Ref::Local(LocalID(0)) },
        Instruction::Move { out: Ref::Local(LocalID(0)), new_val: Value::LiteralNull, },
        Instruction::LocalEnd,

        // and the final jmp for the break
        Instruction::Jump { dest: break_label },
    ];

    assert_eq!(expect, &builder.instructions[from + 1..]);
}