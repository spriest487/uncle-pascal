use crate::DynValue;
use crate::ExecError;
use crate::ExecResult;
use crate::Interpreter;
use crate::Pointer;
use crate::ir;
use std::borrow::Cow;
use std::env::consts::OS;
use std::fmt;
use std::io;
use std::io::BufRead;
use std::io::Write;
use rand::Rng;
use ir_lang::{LocalID, Ref, Type, TypeDefID, Value, RETURN_REF};

fn primitive_to_str<T, UnwrapFn>(state: &mut Interpreter, unwrap_fn: UnwrapFn) -> ExecResult<()>
where
    T: fmt::Display,
    UnwrapFn: FnOnce(&DynValue) -> Option<T>,
{
    let arg_0 = Ref::Local(LocalID(1));

    let arg_0_dyn = state.load(&arg_0)?;
    let value = unwrap_fn(&arg_0_dyn).ok_or_else(|| {
        ExecError::illegal_state("primitive_to_str argument is not the correct type".to_string())
    })?;

    let string = state.create_string(&value.to_string())?;
    state.store(&ir::RETURN_REF, string)?;

    Ok(())
}

/// %1: I8 -> %0: String
pub(super) fn i8_to_str(state: &mut Interpreter) -> ExecResult<()> {
    primitive_to_str(state, DynValue::as_i8)
}

/// %1: U8 -> %0: String
pub(super) fn u8_to_str(state: &mut Interpreter) -> ExecResult<()> {
    primitive_to_str(state, DynValue::as_u8)
}

/// %1: I16 -> %0: String
pub(super) fn i16_to_str(state: &mut Interpreter) -> ExecResult<()> {
    primitive_to_str(state, DynValue::as_i16)
}

/// %1: U16 -> %0: String
pub(super) fn u16_to_str(state: &mut Interpreter) -> ExecResult<()> {
    primitive_to_str(state, DynValue::as_u16)
}

/// %1: I32 -> %0: String
pub(super) fn i32_to_str(state: &mut Interpreter) -> ExecResult<()> {
    primitive_to_str(state, DynValue::as_i32)
}

/// %1: U32 -> %0: String
pub(super) fn u32_to_str(state: &mut Interpreter) -> ExecResult<()> {
    primitive_to_str(state, DynValue::as_u32)
}

/// %1: I64 -> %0: String
pub(super) fn i64_to_str(state: &mut Interpreter) -> ExecResult<()> {
    primitive_to_str(state, DynValue::as_i64)
}

/// %1: U64 -> %0: String
pub(super) fn u64_to_str(state: &mut Interpreter) -> ExecResult<()> {
    primitive_to_str(state, DynValue::as_u64)
}

/// %1: ISize -> %0: String
pub(super) fn isize_to_str(state: &mut Interpreter) -> ExecResult<()> {
    primitive_to_str(state, DynValue::as_isize)
}

/// %1: USize -> %0: String
pub(super) fn usize_to_str(state: &mut Interpreter) -> ExecResult<()> {
    primitive_to_str(state, DynValue::as_usize)
}

/// %1: Pointer -> %0: String
pub(super) fn pointer_to_str(state: &mut Interpreter) -> ExecResult<()> {
    primitive_to_str(state, |val| {
        val.as_pointer().cloned()
    })
}

/// %1: String -> %0: I32
pub(super) fn str_to_int(state: &mut Interpreter) -> ExecResult<()> {
    let arg_0 = Ref::Local(LocalID(1));

    let string = state.read_string(&arg_0)?;
    let int: i32 = string.parse().map_err(|_| ExecError::Raised {
        msg: format!("could not convert `{}` to Integer", string),
    })?;

    state.store(&ir::RETURN_REF, DynValue::I32(int))?;

    Ok(())
}

/// %0: String -> Nothing
pub(super) fn write(state: &mut Interpreter) -> ExecResult<()> {
    let arg_0 = Ref::Local(LocalID(0));
    let string = state.read_string(&arg_0)?;

    _ = io::stdout().lock().write_all(string.as_bytes()).unwrap();

    Ok(())
}

/// %0: String -> Nothing
pub(super) fn write_ln(state: &mut Interpreter) -> ExecResult<()> {
    let arg_0 = Ref::Local(LocalID(0));
    let string = state.read_string(&arg_0)?;

    let mut stdout = io::stdout().lock();

    _ = stdout.write_all(string.as_bytes());
    _ = stdout.write_all(match OS {
       "windows" => "\r\n".as_bytes(),
        _ => "\n".as_bytes(),
    });
    _ = stdout.flush();

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
    if line.len() > 0 {
        line.remove(line.len() - 1);
    }

    let result_str = state.create_string(&line)?;

    state.store(&ir::RETURN_REF, result_str)?;

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

    state.store(&ir::RETURN_REF, DynValue::Pointer(mem_ptr))?;

    Ok(())
}

/// %0: ^Byte -> Nothing
pub(super) fn free_mem(state: &mut Interpreter) -> ExecResult<()> {
    let arg_0 = Ref::Local(LocalID(0));

    let ptr_val = state.load(&arg_0)?.into_owned();

    let ptr = ptr_val
        .as_pointer()
        .ok_or_else(|| ExecError::illegal_state("FreeMem expected heap pointer argument"))?;
    
    if !ptr.is_null() {
        state.dynfree(ptr)?;
    }

    Ok(())
}

fn get_ref_type_id(state: &Interpreter, at: &Ref) -> ExecResult<TypeDefID> {
    let ptr = state.load(at)?
        .into_owned()
        .as_pointer()
        .cloned()
        .ok_or_else(|| ExecError::illegal_state("get_ref_type_id: argument val must be pointer"))?;

    match state.load_indirect(&ptr)? {
        Cow::Borrowed(DynValue::Structure(struct_val)) => Ok(struct_val.type_id),
        Cow::Owned(DynValue::Structure(struct_val)) => Ok(struct_val.type_id),
        _ => Err(ExecError::illegal_state(
            "value pointed to by dynarray pointer is not a dynarray",
        )),
    }
}

/// %1: <any dyn array ref> -> %0: Integer
pub(super) fn array_length(state: &mut Interpreter) -> ExecResult<()> {
    let array_arg_ref = Ref::Local(LocalID(1));
    let array_type_id = get_ref_type_id(state, &array_arg_ref)?;

    let len_func = state.metadata.dyn_array_element_ty(array_type_id)
        .and_then(|elem_ty| {
            let rtti = state.metadata.get_dynarray_runtime_type(elem_ty)?;
            Some(rtti.length)
        })
        .ok_or_else(|| {
            let msg = format!("missing dynarray length generated function for type ID {}", array_type_id);
            ExecError::illegal_state(msg)
        })?;

    let arr_arg = state.evaluate(&Value::Ref(array_arg_ref))?;
    state.call(len_func, &[arr_arg], Some(&ir::RETURN_REF))?;

    Ok(())
}

/// %1: <any dyn array ref>; %2: Integer; %3: ^Element; -> %0: <dyn array ref of same type>
pub(super) fn set_length(state: &mut Interpreter) -> ExecResult<()> {
    let array_arg_ref = Ref::Local(LocalID(1));
    let new_len_arg = Ref::Local(LocalID(2));
    let default_val_arg = Ref::Local(LocalID(3));

    let array_type_id = get_ref_type_id(state, &array_arg_ref)?;
    let new_len = state.evaluate(&Value::Ref(new_len_arg))?;
    let default_val_ptr = state.evaluate(&Value::Ref(default_val_arg))?;

    let alloc_func = state.metadata.dyn_array_element_ty(array_type_id)
        .and_then(|elem_ty| {
            let rtti = state.metadata.get_dynarray_runtime_type(elem_ty)?;
            Some(rtti.alloc)
        })
        .ok_or_else(|| {
            let msg = format!("missing dynarray length generated function for type ID {}", array_type_id);
            ExecError::illegal_state(msg)
        })?;

    let new_arr_struct = state.init_struct(array_type_id)?;

    let old_arr = state.evaluate(&Value::Ref(array_arg_ref))?;
    let new_arr = state.rc_alloc(new_arr_struct)?;
    let new_arr_val = DynValue::Pointer(new_arr);

    state.call(alloc_func, &[new_arr_val.clone(), new_len, old_arr, default_val_ptr], None)?;

    state.store(&ir::RETURN_REF, new_arr_val)?;

    Ok(())
}

fn load_integer(state: &mut Interpreter, at: &Ref) -> ExecResult<i32> {
    state.load(at)?
        .as_i32()
        .ok_or_else(|| {
            let msg = format!("bad type: expected i32 at {}", at);
            ExecError::IllegalState { msg }
        })
}

fn load_single(state: &mut Interpreter, at: &Ref) -> ExecResult<f32> {
    state.load(at)?
        .as_f32()
        .ok_or_else(|| {
            let msg = format!("bad type: expected f32 at {}", at);
            ExecError::IllegalState { msg }
        })
}

pub(super) fn random_integer(state: &mut Interpreter) -> ExecResult<()> {
    let from = load_integer(state, &Ref::Local(LocalID(1)))?;
    let to = load_integer(state, &Ref::Local(LocalID(2)))?;

    let range = from..to;
    let val = if range.is_empty() {
        from
    } else {
        rand::thread_rng().gen_range(from..to)
    };

    state.store(&RETURN_REF, DynValue::I32(val))
}

pub(super) fn random_single(state: &mut Interpreter) -> ExecResult<()> {
    let from = load_single(state, &Ref::Local(LocalID(1)))?;
    let to = load_single(state, &Ref::Local(LocalID(2)))?;

    let range = from..to;
    let val = if range.is_empty() {
        from
    } else {
        rand::thread_rng().gen_range(from..to)
    };

    state.store(&RETURN_REF, DynValue::F32(val))
}

pub(super) fn pow(state: &mut Interpreter) -> ExecResult<()> {
    let val = load_single(state, &Ref::Local(LocalID(1)))?;
    let power = load_single(state, &Ref::Local(LocalID(2)))?;

    state.store(&RETURN_REF, DynValue::F32(val.powf(power)))
}

pub(super) fn sqrt(state: &mut Interpreter) -> ExecResult<()> {
    let val = load_single(state, &Ref::Local(LocalID(1)))?;
    
    state.store(&RETURN_REF, DynValue::F32(val.sqrt()))
}

pub(super) fn sin(state: &mut Interpreter) -> ExecResult<()> {
    let val = load_single(state, &Ref::Local(LocalID(1)))?;

    state.store(&RETURN_REF, DynValue::F32(val.sin()))
}

pub(super) fn arc_sin(state: &mut Interpreter) -> ExecResult<()> {
    let val = load_single(state, &Ref::Local(LocalID(1)))?;

    state.store(&RETURN_REF, DynValue::F32(val.asin()))
}

pub(super) fn cos(state: &mut Interpreter) -> ExecResult<()> {
    let val = load_single(state, &Ref::Local(LocalID(1)))?;

    state.store(&RETURN_REF, DynValue::F32(val.cos()))
}

pub(super) fn arc_cos(state: &mut Interpreter) -> ExecResult<()> {
    let val = load_single(state, &Ref::Local(LocalID(1)))?;

    state.store(&RETURN_REF, DynValue::F32(val.acos()))
}

pub(super) fn tan(state: &mut Interpreter) -> ExecResult<()> {
    let val = load_single(state, &Ref::Local(LocalID(1)))?;

    state.store(&RETURN_REF, DynValue::F32(val.tan()))
}

pub(super) fn arc_tan(state: &mut Interpreter) -> ExecResult<()> {
    let val = load_single(state, &Ref::Local(LocalID(1)))?;

    state.store(&RETURN_REF, DynValue::F32(val.atan()))
}




