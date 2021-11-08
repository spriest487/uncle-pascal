use pas_ir::{
    RETURN_REF,
    metadata::DYNARRAY_LEN_FIELD,
    LocalID, Ref,
};
use crate::{Interpreter, MemCell, Pointer, RcCell, StructCell};
use std::io::{self, BufRead};
use pas_ir::metadata::DYNARRAY_PTR_FIELD;

/// %1: Integer -> %0: String
pub(super) fn int_to_str(state: &mut Interpreter) {
    let arg_0 = Ref::Local(LocalID(1));

    let int = state
        .load(&arg_0)
        .as_i32()
        .unwrap_or_else(|| panic!("IntToStr expected I32 argument"));

    let string = state.create_string(&int.to_string());
    state.store(&RETURN_REF, string);
}

/// %1: String -> %0: Integer
pub(super) fn str_to_int(state: &mut Interpreter) {
    let arg_0 = Ref::Local(LocalID(1));

    let string = state.read_string(&arg_0.deref());
    let int: i32 = string.parse().unwrap_or_else(|_| {
        panic!("IntToStr failed: could not convert `{}` to int", string);
    });

    state.store(&RETURN_REF, MemCell::I32(int));
}

/// %0: String -> Nothing
pub(super) fn write_ln(state: &mut Interpreter) {
    let arg_0 = Ref::Local(LocalID(0));
    let string = state.read_string(&arg_0.deref());

    println!("{}", string);
}

/// %0: Nothing -> String
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

/// %1: Integer -> %0: ^Byte
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

/// %0: ^Byte -> Nothing
pub(super) fn free_mem(state: &mut Interpreter) {
    let arg_0 = Ref::Local(LocalID(0));

    let ptr = state
        .load(&arg_0)
        .as_pointer()
        .and_then(Pointer::as_heap_addr)
        .unwrap_or_else(|| panic!("FreeMem expected heap pointer argument"));

    state.heap.free(ptr);
}

fn get_dyn_array_struct(state: &Interpreter, at: Ref) -> &StructCell {
    let array_ref_cell = state.load(&at);
    let rc_cell_ptr = array_ref_cell
        .as_pointer()
        .expect("array_length: argument cell must be pointer");
    let rc_cell = state.deref_ptr(rc_cell_ptr);

    match state.deref_rc(rc_cell) {
        MemCell::Structure(dyn_array_struct_cell) => dyn_array_struct_cell,
        other => panic!("array_length: expected array cell, got {:?}", other),
    }
}

/// %1: <any dyn array ref> -> %0: Integer
pub(super) fn array_length(state: &mut Interpreter) {
    let array_ref = Ref::Local(LocalID(1));

    let array_struct = get_dyn_array_struct(state, array_ref);

    // the type of %1 should be Any (pointer to an rc cell)
    let len = array_struct[DYNARRAY_LEN_FIELD]
        .as_i32()
        .expect("array_length: argument cell must contain array length field");

    state.store(&RETURN_REF, MemCell::I32(len));
}

/// %1: <any dyn array ref>; %2: Integer; %3: ^Element; -> %0: <dyn array ref of same type>
pub(super) fn set_length(state: &mut Interpreter) {
    let array_arg = Ref::Local(LocalID(1));

    let new_len_arg = Ref::Local(LocalID(2));
    let new_len = state.load(&new_len_arg).as_i32()
        .expect("len arg passed to set_length must be i32");

    let default_val_arg = Ref::Local(LocalID(3));
    let default_val_ptr = state.load(&default_val_arg).as_pointer()
        .expect("default value arg must be a pointer");

    let array_class = state.load(&array_arg)
        .as_pointer()
        .and_then(Pointer::as_heap_addr)
        .and_then(|addr| state.heap.get(addr))
        .and_then(MemCell::as_rc)
        .expect("array arg passed to set_length must refer to an rc cell on the heap")
        .struct_id;

    let new_data = if new_len > 0 {
        let (dyn_array_el_ty, _) = state.metadata.dyn_array_structs().iter()
            .filter(|(_el_ty, struct_id)| **struct_id == array_class)
            .next()
            .unwrap_or_else(|| panic!(
                "array arg passed set_length must have a matching internal struct def for {} in {:#?}",
                array_class,
                state.metadata.dyn_array_structs()
            ));

        let old_array_struct = get_dyn_array_struct(state, array_arg);
        let old_len = old_array_struct[DYNARRAY_LEN_FIELD].as_i32().unwrap();
        let old_data_ptr = old_array_struct[DYNARRAY_PTR_FIELD].as_pointer();

        let mut data_cells = vec![
            state.default_init_cell(dyn_array_el_ty);
            new_len as usize
        ];

        // copy old data (if any) into the new array
        if let Some(old_data_addr) = old_data_ptr.and_then(Pointer::as_heap_addr) {
            for i in 0..new_len.min(old_len) {
                data_cells[i as usize] = state.heap[old_data_addr + i as usize].clone();
            }
        }

        // copy default value into any cells beyond the length of the original array
        let default_val = state.deref_ptr(default_val_ptr);
        for i in old_len..new_len {
            data_cells[i as usize] = default_val.clone();
        }

        Pointer::Heap(state.heap.alloc(data_cells))
    } else {
        Pointer::Null
    };

    let mut new_struct_fields = Vec::new();
    new_struct_fields.push(MemCell::I32(new_len)); // 0 = len
    new_struct_fields.push(MemCell::Pointer(new_data)); // 1 = data ptr

    let new_array_struct = StructCell {
        fields: new_struct_fields,
        id: array_class,
    };

    let new_array_resource = state.heap.alloc(vec![
        MemCell::Structure(Box::new(new_array_struct)),
    ]);

    let new_array_rc = RcCell {
        struct_id: array_class,
        ref_count: 1,
        resource_addr: new_array_resource
    };
    let new_array_cell = MemCell::RcCell(Box::new(new_array_rc));
    let new_array_rc_addr = state.heap.alloc(vec![new_array_cell]);

    state.store(&RETURN_REF, MemCell::Pointer(Pointer::Heap(new_array_rc_addr)));
}