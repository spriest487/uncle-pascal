use pas_ir::{
    RETURN_REF,
    metadata::DYNARRAY_LEN_FIELD,
    LocalID, Ref,
};
use crate::{Interpreter, MemCell, Pointer};
use std::io::{self, BufRead};

/// $1: Integer -> $0: String
pub(super) fn int_to_str(state: &mut Interpreter) {
    let arg_0 = Ref::Local(LocalID(1));

    let int = state
        .load(&arg_0)
        .as_i32()
        .unwrap_or_else(|| panic!("IntToStr expected I32 argument"));

    let string = state.create_string(&int.to_string());
    state.store(&RETURN_REF, string);
}

/// $1: String -> $0: Integer
pub(super) fn str_to_int(state: &mut Interpreter) {
    let arg_0 = Ref::Local(LocalID(1));

    let string = state.read_string(&arg_0.deref());
    let int: i32 = string.parse().unwrap_or_else(|_| {
        panic!("IntToStr failed: could not convert `{}` to int", string);
    });

    state.store(&RETURN_REF, MemCell::I32(int));
}

/// $0: String -> Nothing
pub(super) fn write_ln(state: &mut Interpreter) {
    let arg_0 = Ref::Local(LocalID(0));
    let string = state.read_string(&arg_0.deref());

    println!("{}", string);
}

/// $0: Nothing -> String
pub(super) fn read_ln(state: &mut Interpreter) {
    let stdin = io::stdin();
    let mut line = String::new();

    if let Err(_) = stdin.lock().read_line(&mut line) {
        line = String::new();
    }

    // remove the newline
    line.remove(line.len() - 1);

    let result_str = state.create_string(&line);

    state.store(&RETURN_REF, result_str);
}

/// $1: Integer -> $0: ^Byte
pub(super) fn get_mem(state: &mut Interpreter) {
    let arg_0 = Ref::Local(LocalID(1));

    let len = state
        .load(&arg_0)
        .as_i32()
        .unwrap_or_else(|| panic!("GetMem expected I32 argument"));

    let empty_bytes = vec![MemCell::U8(0); len as usize];
    let mem = state.heap.alloc(empty_bytes);

    state.store(&RETURN_REF, MemCell::Pointer(Pointer::Heap(mem)));
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

/// $1: <any dyn array ref> -> Integer
pub(super) fn array_length(state: &mut Interpreter) {
    let array_ref = Ref::Local(LocalID(1));

    // the type should be Any (pointer to an rc cell)
    let array_ref_cell = state.load(&array_ref);
    let rc_cell_ptr = array_ref_cell
        .as_pointer()
        .expect("array_length: argument cell must be pointer");
    let rc_cell = state.deref_ptr(rc_cell_ptr);

    match state.deref_rc(rc_cell) {
        MemCell::Structure(dyn_array_struct_cell) => {
            let len = dyn_array_struct_cell[DYNARRAY_LEN_FIELD]
                .as_i32()
                .expect("array_length: argument cell must contain array length field");
            state.store(&RETURN_REF, MemCell::I32(len));
        }

        other => panic!("array_length: expected array cell, got {:?}", other),
    }
}
