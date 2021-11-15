use std::{iter, collections::HashMap, cmp::max, ptr::slice_from_raw_parts, rc::Rc};
use std::convert::TryInto;
use std::ffi::c_void;
use crate::{ExecError, ExecResult, Interpreter, ValueCell, Pointer};
use ::dlopen::{
    Error as DlopenError,
    raw as dlopen,
};
use ::libffi::{
    middle::{Cif, Type as ForeignType, Builder as FfiBuilder},
    raw::ffi_call as ffi_raw_call,
};
use pas_common::span::Span;
use pas_ir::{metadata::Metadata,ExternalFunctionRef, LocalID, Ref, Type};

#[derive(Debug)]
pub struct FfiCache {
    types: HashMap<Type, ForeignType>,
    libs: HashMap<String, Rc<dlopen::Library>>,
}

impl FfiCache {
    pub fn new() -> Self {
        Self {
            types: HashMap::new(),
            libs: HashMap::new(),
        }
    }

    pub fn build_invoker(
        &mut self,
        func_ref: &ExternalFunctionRef,
        metadata: &Metadata,
    ) -> ExecResult<FfiInvoker> {
        let sym_load_err = |err: DlopenError| {
            ExecError::ExternSymbolLoadFailed {
                lib: func_ref.src.clone(),
                symbol: func_ref.symbol.clone(),
                span: func_ref.src_span.clone(),
                msg: err.to_string(),
            }
        };

        let ffi_return_ty = self.type_to_ffi(&func_ref.return_ty, metadata);
        let ffi_param_tys: Vec<_> = func_ref
            .params
            .iter()
            .map(|ty| self.type_to_ffi(ty, metadata))
            .collect();

        let cif = FfiBuilder::new()
            .args(ffi_param_tys.iter().cloned())
            .res(ffi_return_ty.clone())
            .into_cif();

        let lib = match self.libs.get(&func_ref.src) {
            Some(lib_rc) => lib_rc.clone(),
            None => {
                let lib = dlopen::Library::open(&func_ref.src).map_err(sym_load_err)?;
                let lib_rc = Rc::new(lib);
                self.libs.insert(func_ref.src.clone(), lib_rc.clone());
                lib_rc
            }
        };

        let symbol = unsafe {
            lib.symbol::<*const ()>(&func_ref.symbol).map_err(sym_load_err)?
        };

        Ok(FfiInvoker {
            cif,
            symbol,
            ffi_param_tys,

            param_tys: func_ref.params.clone(),
            return_ty: func_ref.return_ty.clone(),
            ffi_return_ty,
        })
    }

    pub fn type_to_ffi(&mut self, ty: &Type, metadata: &Metadata) -> ForeignType {
        if let Some(cached_ty) = self.types.get(ty) {
            return cached_ty.clone();
        }

        match ty {
            Type::Pointer(..) | Type::RcPointer(..) => ForeignType::pointer(),
            Type::I32 => ForeignType::i32(),
            Type::U8 => ForeignType::u8(),
            Type::F32 => ForeignType::f32(),

            Type::Array { element, dim } => {
                // pascal static arrays are treated as a struct of elements laid out sequentially
                let el_ty = self.type_to_ffi(element, metadata);
                let el_tys = vec![el_ty; *dim];

                ForeignType::structure(el_tys)
            }
            Type::Struct(id) => match metadata.get_struct_def(*id) {
                None => ForeignType::void(),
                Some(struct_def) => {
                    let mut def_fields: Vec<_> = struct_def.fields.iter().collect();
                    def_fields.sort_by_key(|(f_id, _)| **f_id);

                    ForeignType::structure(
                        def_fields
                            .into_iter()
                            .map(|(_, field)| self.type_to_ffi(&field.ty, metadata)),
                    )
                }
            },
            // variants are marshalled as an i32 discriminator + a number of bytes equal to the
            // foreign size of the largest variant
            Type::Variant(id) => match metadata.get_variant_def(*id) {
                None => ForeignType::void(),
                Some(variant_def) => {
                    let mut max_case_size = 0;
                    for case in &variant_def.cases {
                        let case_size = match case.ty.as_ref() {
                            Some(case_ty) => {
                                let case_ty = self.type_to_ffi(case_ty, metadata);
                                foreign_type_size(&case_ty)
                            },
                            None => 0,
                        };
                        max_case_size = max(case_size, max_case_size);
                    }

                    let variant_layout: Vec<_> = iter::once(ForeignType::i32())
                        .chain(iter::repeat(ForeignType::u8()).take(max_case_size))
                        .collect();

                    ForeignType::structure(variant_layout)
                },
            },
            Type::Bool => ForeignType::c_int(),
            Type::Nothing => ForeignType::void(),
        }
    }
}

pub struct FfiInvoker {
    cif: Cif,

    ffi_param_tys: Vec<ForeignType>,

    param_tys: Vec<Type>,
    return_ty: Type,

    symbol: *const (),
    ffi_return_ty: ForeignType,
}

impl FfiInvoker {
    pub fn invoke(&self, state: &mut Interpreter) -> ExecResult<()> {
        let has_return = self.return_ty != Type::Nothing;

        // marshal args into a byte vec
        let mut args = Vec::new();
        let mut args_ptrs = Vec::new();
        let first_param_local = match has_return { true => 1, false => 0 };

        let param_ty_both = self.param_tys.iter().zip(self.ffi_param_tys.iter());

        for (i, (param_ty, ffi_param_ty)) in param_ty_both.enumerate() {
            let local_id = LocalID(first_param_local + i);
            let arg_foreign_len = foreign_type_size(&ffi_param_ty);

            let arg_start = args.len();
            args.resize(args.len() + arg_foreign_len, 0);

            let arg_val = state.load(&Ref::Local(local_id))?;
            let bytes_copied = marshal(arg_val, &mut args[arg_start..], param_ty, &state.debug_ctx())?;

            assert_eq!(bytes_copied, foreign_type_size(ffi_param_ty));

            let arg_ptr = unsafe {
                (args.as_mut_ptr() as *mut c_void).offset(arg_start as isize)
            };
            args_ptrs.push(arg_ptr);
        }

        let mut result_buf: Vec<u8> = if has_return {
            vec![0; foreign_type_size(&self.ffi_return_ty)]
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

        if has_return {
            unsafe {
                let result_slice = slice_from_raw_parts(result_buf.as_ptr(), result_buf.len());
                let return_val = unmarshal(result_slice.as_ref().unwrap(), &self.return_ty, &state.debug_ctx())?;

                let return_local = Ref::Local(LocalID(0));
                state.store(&return_local, return_val)?;
            }
        }

        Ok(())
    }
}

fn marshal(val: &ValueCell, out_bytes: &mut [u8], ty: &Type, span: &Span) -> ExecResult<usize> {
    let from_bytes = |bytes: &[u8], out_bytes: &mut [u8]| {
        out_bytes[0..bytes.len()].copy_from_slice(bytes);
        bytes.len()
    };

    let size = match val {
        ValueCell::I32(x) => from_bytes(&x.to_ne_bytes(), out_bytes),
        ValueCell::F32(x) => from_bytes(&x.to_ne_bytes(), out_bytes),
        ValueCell::U8(x) => from_bytes(&x.to_ne_bytes(), out_bytes),
        ValueCell::Bool(x) => {
            out_bytes[0] = if *x { 1 } else { 0 };
            1
        },
        ValueCell::Array(x) => {
            // let el_foreign_ty = cache.type_to_ffi(&x.el_ty, metadata)?;
            // let el_size = foreign_type_size(&el_foreign_ty);

            let dim = cast::i32(x.elements.len())
                .map_err(|_err| ExecError::MarshallingFailed {
                    failed_ty: Type::Array {
                        element: Box::new(x.el_ty.clone()),
                        dim: x.elements.len(),
                    },
                    span: span.clone(),
                })?;

            let mut el_offset = from_bytes(&dim.to_ne_bytes(), out_bytes);

            for i in 0..dim {
                let el = &x.elements[i as usize];
                let el_bytes = &mut out_bytes[el_offset..];
                el_offset += marshal(el, el_bytes, &x.el_ty, span)?;
            }

            el_offset
        }

        ValueCell::Pointer(Pointer::External(ptr)) => {
            let ptr_bytes = (*ptr as isize).to_ne_bytes();
            from_bytes(&ptr_bytes, out_bytes)
        }

        _ => {
            return Err(ExecError::MarshallingFailed { failed_ty: ty.clone(), span: span.clone() })
        }
    };

    Ok(size)
}

fn unmarshal(in_bytes: &[u8], ty: &Type, span: &Span) -> ExecResult<ValueCell> {
    let make_err = || ExecError::MarshallingFailed {
        failed_ty: ty.clone(),
        span: span.clone(),
    };

    match ty {
        Type::I32 => {
            let in_bytes = in_bytes.try_into().map_err(|_| make_err())?;
            let val = i32::from_ne_bytes(in_bytes);
            Ok(ValueCell::I32(val))
        }

        Type::Pointer(..) => {
            let in_bytes = in_bytes.try_into().map_err(|_| make_err())?;
            let val = isize::from_ne_bytes(in_bytes);
            Ok(ValueCell::Pointer(Pointer::External(val)))
        }

        _ => Err(make_err())
    }
}

fn foreign_type_size(ty: &ForeignType) -> usize {
    unsafe { *ty.as_raw_ptr() }.size
}