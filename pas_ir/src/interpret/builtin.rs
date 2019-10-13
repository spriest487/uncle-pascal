use crate::{
    interpret::{Interpreter, MemCell, Pointer},
    LocalID, Ref,
};

/// $1: Integer -> $0: String
pub(super) fn int_to_str(state: &mut Interpreter) {
    let return_ref = Ref::Local(LocalID(0));
    let arg_0 = Ref::Local(LocalID(1));

    let int = state
        .load(&arg_0)
        .as_i32()
        .unwrap_or_else(|| panic!("IntToStr expected I32 argument"));

    let string = state.create_string(&int.to_string());
    state.store(&return_ref, string);
}

/// $1: String -> $0: Integer
pub(super) fn str_to_int(state: &mut Interpreter) {
    let return_ref = Ref::Local(LocalID(0));
    let arg_0 = Ref::Local(LocalID(1));

    let string = state.read_string(&arg_0.deref());
    let int: i32 = string.parse().unwrap_or_else(|_| {
        panic!("IntToStr failed: could not convert `{}` to int", string);
    });

    state.store(&return_ref, MemCell::I32(int));
}

/// $0: String -> Nothing
pub(super) fn write_ln(state: &mut Interpreter) {
    let arg_0 = Ref::Local(LocalID(0));
    let string = state.read_string(&arg_0.deref());

    println!("{}", string);
}

/// $1: Integer -> $0: ^Byte
pub(super) fn get_mem(state: &mut Interpreter) {
    let ret = Ref::Local(LocalID(0));
    let arg_0 = Ref::Local(LocalID(1));

    let len = state
        .load(&arg_0)
        .as_i32()
        .unwrap_or_else(|| panic!("GetMem expected I32 argument"));

    let empty_bytes = vec![MemCell::U8(0); len as usize];
    let mem = state.heap.alloc(empty_bytes);

    state.store(&ret, MemCell::Pointer(Pointer::Heap(mem)));
}

/// $0: ^Byte -> Nothing
pub(super) fn free_mem(state: &mut Interpreter) {
    let arg_0 = Ref::Local(LocalID(0));

    let ptr = state
        .load(&arg_0)
        .as_pointer()
        .and_then(Pointer::as_heap_addr)
        .unwrap_or_else(|| panic!("FreeMem expected heap pointer argument"));

    state.heap.free(ptr);
}
