use crate::{DynValue, ExecError, ExecResult, Interpreter, Pointer, RcState, StructValue};
use pas_ir::{
    metadata::{
        DYNARRAY_LEN_FIELD,
        DYNARRAY_PTR_FIELD
    },
    GlobalRef, LocalID, Ref, Type, RETURN_REF,
};
use std::{
    borrow::Cow,
    fmt,
    io::{self, BufRead},
};

fn primitive_to_str<T, UnwrapFn>(state: &mut Interpreter, unwrap_fn: UnwrapFn) -> ExecResult<()>
where
    T: fmt::Display,
    UnwrapFn: FnOnce(&DynValue) -> Option<T>,
{
    let arg_0 = Ref::Local(LocalID(1));

    let arg_0_dyn = state.load(&arg_0)?;
    let value = unwrap_fn(&arg_0_dyn).ok_or_else(|| {
        ExecError::illegal_state(format!("primitive_to_str argument is not the correct type"))
    })?;

    let string = state.create_string(&value.to_string())?;
    state.store(&RETURN_REF, string)?;

    Ok(())
}

/// %1: I8 -> %0: String
pub(super) fn i8_to_str(state: &mut Interpreter) -> ExecResult<()> {
    primitive_to_str(state, DynValue::as_i8)
}

/// %1: I8 -> %0: String
pub(super) fn u8_to_str(state: &mut Interpreter) -> ExecResult<()> {
    primitive_to_str(state, DynValue::as_u8)
}

/// %1: Integer -> %0: String
pub(super) fn i16_to_str(state: &mut Interpreter) -> ExecResult<()> {
    primitive_to_str(state, DynValue::as_i16)
}

/// %1: Integer -> %0: String
pub(super) fn u16_to_str(state: &mut Interpreter) -> ExecResult<()> {
    primitive_to_str(state, DynValue::as_u16)
}

/// %1: Integer -> %0: String
pub(super) fn i32_to_str(state: &mut Interpreter) -> ExecResult<()> {
    primitive_to_str(state, DynValue::as_i32)
}

/// %1: Integer -> %0: String
pub(super) fn u32_to_str(state: &mut Interpreter) -> ExecResult<()> {
    primitive_to_str(state, DynValue::as_u32)
}

/// %1: Integer -> %0: String
pub(super) fn i64_to_str(state: &mut Interpreter) -> ExecResult<()> {
    primitive_to_str(state, DynValue::as_i64)
}

/// %1: Integer -> %0: String
pub(super) fn u64_to_str(state: &mut Interpreter) -> ExecResult<()> {
    primitive_to_str(state, DynValue::as_u64)
}

/// %1: Integer -> %0: String
pub(super) fn isize_to_str(state: &mut Interpreter) -> ExecResult<()> {
    primitive_to_str(state, DynValue::as_isize)
}

/// %1: Integer -> %0: String
pub(super) fn usize_to_str(state: &mut Interpreter) -> ExecResult<()> {
    primitive_to_str(state, DynValue::as_usize)
}

/// %1: String -> %0: Integer
pub(super) fn str_to_int(state: &mut Interpreter) -> ExecResult<()> {
    let arg_0 = Ref::Local(LocalID(1));

    let string = state.read_string(&arg_0)?;
    let int: i32 = string.parse().map_err(|_| ExecError::Raised {
        msg: format!("could not convert `{}` to Integer", string),
    })?;

    state.store(&RETURN_REF, DynValue::I32(int))?;

    Ok(())
}

/// %0: String -> Nothing
pub(super) fn write_ln(state: &mut Interpreter) -> ExecResult<()> {
    let arg_0 = Ref::Local(LocalID(0));
    let string = state.read_string(&arg_0)?;

    println!("{}", string);

    Ok(())
}

/// %0: Nothing -> String
pub(super) fn read_ln(state: &mut Interpreter) -> ExecResult<()> {
    let stdin = io::stdin();
    let mut line = String::new();

    if let Err(_) = stdin.lock().read_line(&mut line) {
        line = String::new();
    }

    // remove the newline
    line.remove(line.len() - 1);

    let result_str = state.create_string(&line)?;

    state.store(&RETURN_REF, result_str)?;

    Ok(())
}

/// %1: Integer -> %0: ^Byte
pub(super) fn get_mem(state: &mut Interpreter) -> ExecResult<()> {
    let arg_0 = Ref::Local(LocalID(1));

    let len = state
        .load(&arg_0)?
        .as_i32()
        .ok_or_else(|| ExecError::illegal_state("GetMem expected I32 argument"))?;

    let mem_ptr = if len != 0 {
        state.dynalloc(&Type::U8, len as usize)?
    } else {
        Pointer::null(Type::U8)
    };

    state.store(&RETURN_REF, DynValue::Pointer(mem_ptr))?;

    Ok(())
}

/// %0: ^Byte -> Nothing
pub(super) fn free_mem(state: &mut Interpreter) -> ExecResult<()> {
    let arg_0 = Ref::Local(LocalID(0));

    let ptr_val = state.load(&arg_0)?.into_owned();

    let ptr = ptr_val
        .as_pointer()
        .ok_or_else(|| ExecError::illegal_state("FreeMem expected heap pointer argument"))?;

    state.dynfree(ptr)?;

    Ok(())
}

fn get_dyn_array_struct(state: &Interpreter, at: Ref) -> ExecResult<Cow<StructValue>> {
    let array_ptr_val = state.load(&at)?.into_owned();
    let array_ptr = array_ptr_val.as_pointer()
        .ok_or_else(|| ExecError::illegal_state("array_length: argument val must be pointer"))?;

    match state.load_indirect(array_ptr)? {
        Cow::Borrowed(DynValue::Structure(arr_struct)) => Ok(Cow::Borrowed(arr_struct.as_ref())),
        Cow::Owned(DynValue::Structure(arr_struct)) => Ok(Cow::Owned(*arr_struct)),
        _ => Err(ExecError::illegal_state(
            "value pointed to by dynarray pointer is not a dynarray",
        )),
    }
}

/// %1: <any dyn array ref> -> %0: Integer
pub(super) fn array_length(state: &mut Interpreter) -> ExecResult<()> {
    let array_arg_ref = Ref::Local(LocalID(1));

    let array_struct = get_dyn_array_struct(state, array_arg_ref)?;

    // the type of %1 should be Any (pointer to an rc val)
    let len_val = &array_struct[DYNARRAY_LEN_FIELD];
    let len = len_val.as_i32().ok_or_else(|| {
        let msg = format!("array_length argument val not an I32");
        ExecError::illegal_state(msg)
    })?;

    state.store(&RETURN_REF, DynValue::I32(len))?;

    Ok(())
}

/// %1: <any dyn array ref>; %2: Integer; %3: ^Element; -> %0: <dyn array ref of same type>
pub(super) fn set_length(state: &mut Interpreter) -> ExecResult<()> {
    let array_arg = Ref::Local(LocalID(1));
    let new_len_arg = Ref::Local(LocalID(2));
    let default_val_arg = Ref::Local(LocalID(3));
    let default_val_len_arg = Ref::Local(LocalID(4));

    let new_len = state
        .load(&new_len_arg)?
        .as_i32()
        .ok_or_else(|| ExecError::illegal_state("len arg passed to set_length must be i32"))?;

    let array_class_ptr = state
        .load(&array_arg)?
        .as_pointer()
        .cloned()
        .ok_or_else(|| {
            let msg = "array arg passed to set_length must be a heap pointer";
            ExecError::illegal_state(msg)
        })?;

    let array_val = state.load_indirect(&array_class_ptr)?.into_owned();
    let array_ty_id = match &array_val {
        DynValue::Structure(s) => s.type_id,
        _ => {
            let msg = "array arg passed to set_length must point to a struct";
            return Err(ExecError::illegal_state(msg));
        },
    };

    let dyn_array_el_ty = state
        .metadata
        .dyn_array_element_ty(array_ty_id)
        .cloned()
        .unwrap();

    let default_val = state.load(&default_val_arg)?.into_owned();
    let default_val_ptr = default_val
        .as_pointer()
        .ok_or_else(|| ExecError::illegal_state("default value arg must be a pointer"))?
        .reinterpret(dyn_array_el_ty.clone());
    let _default_val_len = state
        .load(&default_val_len_arg)?
        .as_i32()
        .ok_or_else(|| ExecError::illegal_state("default value len arg must be an i32"))?;

    let new_data = if new_len > 0 {
        let dyn_array_el_marshal_ty = state.marshaller.get_ty(&dyn_array_el_ty)?;

        let el_rc_funcs = state
            .metadata
            .find_rc_boilerplate(&dyn_array_el_ty)
            .map(|rc_info| {
                let retain_func_ref = GlobalRef::Function(rc_info.retain);
                let retain_func = match state.globals.get(&retain_func_ref).map(|c| &c.value) {
                    Some(DynValue::Function(func)) => func.clone(),
                    _ => panic!(
                        "missing element retain func for dynarray {}",
                        dyn_array_el_ty
                    ),
                };

                let release_func_ref = GlobalRef::Function(rc_info.release);
                let release_func = match state.globals.get(&release_func_ref).map(|c| &c.value) {
                    Some(DynValue::Function(func)) => func.clone(),
                    _ => panic!(
                        "missing element release func for dynarray {}",
                        dyn_array_el_ty
                    ),
                };

                (retain_func, release_func)
            });

        let old_array_struct = get_dyn_array_struct(state, array_arg)?.into_owned();
        let len_field_val = &old_array_struct[DYNARRAY_LEN_FIELD];
        let old_len = len_field_val.as_i32().ok_or_else(|| {
            let msg = format!("dynarray length is not I32");
            ExecError::illegal_state(msg)
        })?;

        let old_data_ptr = old_array_struct[DYNARRAY_PTR_FIELD]
            .as_pointer()
            .ok_or_else(|| {
                let msg = format!("dynarray ptr is not a valid ptr");
                ExecError::illegal_state(msg)
            })?;

        // copy default value into any vals beyond the length of the original array
        let default_val = state.load_indirect(&default_val_ptr)?.into_owned();

        // put the initialized array onto the heap before retaining it because passing pointers
        // to retain funcs is a lot easier when the target is already on the heap
        let new_data_ptr = state.dynalloc(&dyn_array_el_ty, new_len as usize)?;

        for i in 0..new_len {
            let i = i as usize;
            let el_offset = i * dyn_array_el_marshal_ty.size();

            let el_ptr = Pointer {
                addr: new_data_ptr.addr + el_offset,
                ty: dyn_array_el_ty.clone(),
            };

            if i < old_len as usize {
                // copy old elements
                let old_el_ptr = Pointer {
                    addr: old_data_ptr.addr + el_offset,
                    ty: dyn_array_el_ty.clone(),
                };

                let old_el = state.load_indirect(&old_el_ptr)?.into_owned();
                state.store_indirect(&el_ptr, old_el)?;
            } else {
                // assign default val for new elements
                state.store_indirect(&el_ptr, default_val.clone())?;
            }

            if dyn_array_el_ty.is_rc() {
                let el_rc_ref = state.load_indirect(&el_ptr)?.into_owned();
                state.retain_dyn_val(&el_rc_ref)?;
            } else if let Some((el_retain_func_id, _)) = &el_rc_funcs {
                state.call(*el_retain_func_id, &[DynValue::Pointer(el_ptr)], None)?;
            }
        }

        new_data_ptr
    } else {
        Pointer::null(dyn_array_el_ty)
    };

    assert_eq!(new_data.is_null(), new_len == 0);

    let new_array_rc_state = RcState {
        strong_count: 1,
        weak_count: 0,
    };

    let new_array_struct = StructValue {
        type_id: array_ty_id,
        rc: Some(new_array_rc_state),
        fields: vec![
            DynValue::I32(new_len),
            DynValue::Pointer(new_data)
        ]
    };

    let new_array_val = DynValue::Structure(Box::new(new_array_struct));

    let new_array_ptr = state.dynalloc_init(&Type::Struct(array_ty_id), vec![new_array_val])?;

    state.store(&RETURN_REF, DynValue::Pointer(new_array_ptr))?;
    Ok(())
}
