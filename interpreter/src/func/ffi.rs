use std::{ptr::slice_from_raw_parts};
use std::ffi::c_void;
use crate::{
    ExecResult, Interpreter,
    marshal::ForeignType,
};
use ::libffi::{
    middle::Cif,
    raw::ffi_call as ffi_raw_call,
};
use pas_ir::{LocalID, Ref, Type};

pub struct FfiInvoker {
    cif: Cif,

    symbol: *const (),

    param_tys: Vec<Type>,
    ffi_param_tys: Vec<ForeignType>,

    return_ty: Type,
    ffi_return_ty: ForeignType,
}

impl FfiInvoker {
    pub fn new(
        cif: Cif,
        symbol: *const (),
        param_tys: Vec<Type>,
        ffi_param_tys: Vec<ForeignType>,
        return_ty: Type,
        ffi_return_ty: ForeignType
    ) -> Self {
        Self {
            cif,
            symbol,

            param_tys,
            ffi_param_tys,

            return_ty,
            ffi_return_ty,
        }
    }

    pub fn invoke(&self, state: &mut Interpreter) -> ExecResult<()> {
        // marshal args into a byte vec
        let mut args = Vec::new();
        let mut args_ptrs = Vec::new();
        let first_param_local = match self.return_ty { Type::Nothing => 0, _ => 1 };

        let param_ty_both = self.param_tys.iter().zip(self.ffi_param_tys.iter());

        for (i, (_param_ty, ffi_param_ty)) in param_ty_both.enumerate() {
            let local_id = LocalID(first_param_local + i);
            let arg_foreign_len = ffi_param_ty.size();

            let arg_start = args.len();
            args.resize(args.len() + arg_foreign_len, 0);

            let arg_val = state.load(&Ref::Local(local_id))?;
            let bytes_copied = state.marshaller().marshal(&arg_val, &mut args[arg_start..])?;

            assert_eq!(bytes_copied, ffi_param_ty.size());

            let arg_ptr = unsafe {
                (args.as_mut_ptr() as *mut c_void).offset(arg_start as isize)
            };
            args_ptrs.push(arg_ptr);
        }

        let mut result_buf: Vec<u8> = if self.return_ty != Type::Nothing {
            vec![0; self.ffi_return_ty.size()]
        } else {
            Vec::new()
        };

        unsafe {
            let fn_addr: extern "C" fn() = std::mem::transmute(self.symbol);

            ffi_raw_call(
                &self.cif as *const Cif as _,
                Some(fn_addr),
                result_buf.as_mut_ptr() as _,
                args_ptrs.as_mut_ptr(),
            );
        }

        if self.return_ty != Type::Nothing {
            unsafe {
                let result_slice = slice_from_raw_parts(result_buf.as_ptr(), result_buf.len());
                let return_val = state.marshaller().unmarshal(result_slice.as_ref().unwrap(), &self.return_ty)?;

                let return_local = Ref::Local(LocalID(0));
                state.store(&return_local, return_val.value)?;
            }
        }

        Ok(())
    }
}