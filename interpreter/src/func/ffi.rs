use std::{iter, collections::HashMap};
use std::cmp::max;
use std::ptr::slice_from_raw_parts;
use std::rc::Rc;
use crate::{ExecError, ExecResult, Interpreter, MemCell};
use ::dlopen::{
    Error as DlopenError,
    raw as dlopen,
};
use ::libffi::{
    middle::{Cif, Type as ForeignType, Builder as FfiBuilder},
    raw::ffi_raw_call as ffi_raw_call,
};
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

    return_ty: Type,

    symbol: *const (),
    ffi_return_ty: ForeignType,
}

impl FfiInvoker {
    pub fn invoke(&self, state: &mut Interpreter) -> ExecResult<()> {
        let has_return = self.return_ty != Type::Nothing;

        // marshal args into a byte vec
        let mut args = Vec::new();
        let first_param_local = match has_return { true => 1, false => 0 };

        for (i, param_ty) in self.ffi_param_tys.iter().enumerate() {
            let local_id = LocalID(first_param_local + i);
            let arg_foreign_len = foreign_type_size(&param_ty);

            let arg_start = args.len();
            args.resize(args.len() + arg_foreign_len, 0);

            let arg_val = state.load(&Ref::Local(local_id))?;
            marshal(arg_val, &mut args[arg_start..]);
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
                args.as_mut_ptr() as _,
                result_buf.as_mut_ptr() as _,
            );
        }

        if has_return {
            unsafe {
                let result_slice = slice_from_raw_parts(result_buf.as_ptr(), result_buf.len());
                let return_val = unmarshal(result_slice.as_ref().unwrap(), &self.return_ty);

                let return_local = Ref::Local(LocalID(0));
                state.store(&return_local, return_val)?;
            }
        }

        Ok(())
    }
}

fn marshal(_val: &MemCell, _out_bytes: &mut [u8]) {
    unimplemented!()
}

fn unmarshal(_in_bytes: &[u8], _ty: &Type) -> MemCell {
    unimplemented!()
}

fn foreign_type_size(ty: &ForeignType) -> usize {
    unsafe { *ty.as_raw_ptr() }.size
}