use {
    crate::{
        Ref,
        interpret::{
            Interpreter,
        },
    }
};

/// $1: Integer -> $0: ^String
pub(super) fn int_to_str(state: &mut Interpreter) {
    let return_ref = Ref::Local(0);
    let arg_0 = Ref::Local(1);

    let int = state.load(&arg_0).as_i32()
        .unwrap_or_else(|| panic!("IntToStr expected I32 argument"));

    let string = state.create_string(&int.to_string());
    state.store(&return_ref, string);
}

/// $0: ^String -> Nothing
pub(super) fn write_ln(state: &mut Interpreter) {
    let arg_1 = Ref::Local(0);
    let string = state.read_string(&arg_1.deref());

    println!("{}", string);
}