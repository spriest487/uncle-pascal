use super::*;

#[test]
fn end_loop_scope_ends_at_right_scope_level() {
    let ctx = typ::Context::root(Span::zero("test"));
    let mut module = Module::new(ctx, Metadata::default(), IROptions::default());
    let mut builder = Builder::new(&mut module);

    let initial_scope = builder.scopes.len();

    let continue_label = builder.alloc_label();
    let break_label = builder.alloc_label();
    builder.begin_loop_body_scope(continue_label, break_label);
    builder.end_loop_body_scope();

    assert_eq!(initial_scope, builder.scopes.len());
}

#[test]
fn break_cleans_up_loop_locals() {
    let ctx = typ::Context::root(Span::zero("test"));
    let mut module = Module::new(ctx, Metadata::default(), IROptions::default());
    let mut builder = Builder::new(&mut module);

    let continue_label = builder.alloc_label();
    let break_label = builder.alloc_label();

    builder.begin_loop_body_scope(continue_label, break_label);
    builder.local_new(
        Type::RcPointer(VirtualTypeID::Any),
        Some("local1".to_string()),
    );
    builder.local_new(
        Type::RcPointer(VirtualTypeID::Any),
        Some("local2".to_string()),
    );

    builder.comment("before_break");
    builder.break_loop();

    let from = builder
        .instructions
        .iter()
        .position(|i| match i {
            Instruction::Comment(c) => c == "before_break",
            _ => false,
        })
        .unwrap();

    // Both locals should be released
    let expect = &[
        Instruction::Release {
            at: Ref::Local(LocalID(1)),
        },
        Instruction::Release {
            at: Ref::Local(LocalID(0)),
        },
        // and the final jmp for the break
        Instruction::Jump { dest: break_label },
    ];

    assert_eq!(expect, &builder.instructions[from + 1..]);
}
