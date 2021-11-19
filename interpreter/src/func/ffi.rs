use std::{iter, collections::HashMap, cmp::max, ptr::slice_from_raw_parts, rc::Rc, fmt};
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
use pas_ir::{metadata::Metadata, ExternalFunctionRef, LocalID, Ref, Type};
use pas_ir::metadata::Variant;
use pas_ir::prelude::{Struct, StructID};
use crate::heap::native_heap::NativePointer;

#[derive(Debug, Clone)]
pub struct FfiCache {
    types: HashMap<Type, ForeignType>,
    libs: HashMap<String, Rc<dlopen::Library>>,
}

#[derive(Clone, Debug)]
pub enum MarshalError {
    UnsupportedType(Type),
    UnsupportedValue(ValueCell),

    ExternSymbolLoadFailed {
        lib: String,
        symbol: String,
        msg: String,
    }
}

impl fmt::Display for MarshalError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            MarshalError::UnsupportedValue(val) => write!(f, "unable to marshal value: {:?}", val),
            MarshalError::UnsupportedType(ty) => write!(f, "unable to marshal type: {}", ty),
            MarshalError::ExternSymbolLoadFailed { lib, symbol, msg } => write!(f, "external symbol {}::{} failed to load: {}", lib, symbol, msg),
        }
    }
}

pub type MarshalResult<T> = Result<T, MarshalError>;

impl FfiCache {
    pub fn new() -> Self {
        let mut ffi_cache = Self {
            types: HashMap::new(),
            libs: HashMap::new(),
        };

        ffi_cache.types.insert(Type::I32, ForeignType::i32());
        ffi_cache.types.insert(Type::F32, ForeignType::f32());
        ffi_cache.types.insert(Type::U8, ForeignType::u8());
        ffi_cache.types.insert(Type::Bool, ForeignType::u8());
        ffi_cache.types.insert(Type::Nothing, ForeignType::void());

        ffi_cache
    }

    pub fn add_struct(&mut self, id: StructID, def: &Struct, metadata: &Metadata) -> MarshalResult<ForeignType> {
        let struct_ty = Type::Struct(id);
        if let Some(cached) = self.types.get(&struct_ty) {
            return Ok(cached.clone());
        }

        let mut def_fields: Vec<_> = def.fields.iter().collect();
        def_fields.sort_by_key(|(f_id, _)| **f_id);

        let struct_ffi_ty = ForeignType::structure({
            let mut field_ffi_tys = Vec::new();
            for (_, def_field) in def_fields {
                field_ffi_tys.push(self.build_marshalled_type(&def_field.ty, metadata)?);
            }
            field_ffi_tys
        });

        self.types.insert(struct_ty, struct_ffi_ty.clone());
        Ok(struct_ffi_ty)
    }

    pub fn add_variant(&mut self, id: StructID, def: &Variant, metadata: &Metadata) -> MarshalResult<ForeignType> {
        let variant_ty = Type::Variant(id);
        if let Some(cached) = self.types.get(&variant_ty) {
            return Ok(cached.clone());
        }

        let mut max_case_size = 0;
        for case in &def.cases {
            let case_size = match case.ty.as_ref() {
                Some(case_ty) => {
                    let case_ty = self.build_marshalled_type(case_ty, metadata)?;
                    case_ty.size()
                },
                None => 0,
            };
            max_case_size = max(case_size, max_case_size);
        }

        let variant_layout: Vec<_> = iter::once(ForeignType::i32())
            .chain(iter::repeat(ForeignType::u8()).take(max_case_size))
            .collect();

        let variant_ffi_ty = ForeignType::structure(variant_layout);

        self.types.insert(variant_ty, variant_ffi_ty.clone());
        Ok(variant_ffi_ty)
    }

    pub fn build_invoker(
        &mut self,
        func_ref: &ExternalFunctionRef,
        metadata: &Metadata,
    ) -> MarshalResult<FfiInvoker> {
        let sym_load_err = |err: DlopenError| {
            MarshalError::ExternSymbolLoadFailed {
                lib: func_ref.src.clone(),
                symbol: func_ref.symbol.clone(),
                msg: err.to_string(),
            }
        };

        let ffi_return_ty = self.build_marshalled_type(&func_ref.return_ty, metadata)?;

        let ffi_param_tys: Vec<_> = func_ref
            .params
            .iter()
            .map(|ty| self.build_marshalled_type(ty, metadata))
            .collect::<MarshalResult<_>>()?;

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

    fn build_marshalled_type(&mut self, ty: &Type, metadata: &Metadata) -> MarshalResult<ForeignType> {
        if let Some(cached) = self.types.get(ty) {
            return Ok(cached.clone());
        }

        match ty {
            Type::Variant(id) => {
                let def = metadata.get_variant_def(*id)
                    .ok_or_else(|| MarshalError::UnsupportedType(ty.clone()))?;

                self.add_variant(*id, def, metadata)
            }

            Type::Struct(id) => {
                let def = metadata.get_struct_def(*id)
                    .ok_or_else(|| MarshalError::UnsupportedType(ty.clone()))?;

                self.add_struct(*id, def, metadata)
            }

            Type::Pointer(..) => {
                Ok(ForeignType::pointer())
            }

            Type::Array { element, dim } => {
                let el_ty = self.build_marshalled_type(&element, metadata)?;
                let el_tys = vec![el_ty; *dim];

                Ok(ForeignType::structure(el_tys))
            }

            // all primitives/builtins should be in the cache already
            _ => Err(MarshalError::UnsupportedType(ty.clone()))
        }
    }

    pub fn get_ty(&self, ty: &Type) -> MarshalResult<ForeignType> {
        match ty {
            // we can't cache array types so always build them on demand
            // pascal static arrays are treated as a struct of elements laid out sequentially
            Type::Array { element, dim } => {
                let el_ty = self.get_ty(&element)?;
                let el_tys = vec![el_ty; *dim];

                Ok(ForeignType::structure(el_tys))
            }

            Type::Pointer(..) => {
                Ok(ForeignType::pointer())
            }

            ty => match self.types.get(ty) {
                Some(cached_ty) => Ok(cached_ty.clone()),
                None => Err(MarshalError::UnsupportedType(ty.clone())),
            }
        }
    }
}

pub fn get_marshal_ty(value: &ValueCell) -> MarshalResult<Type> {
    let ty = match value {
        ValueCell::Bool(_) => Type::Bool,
        ValueCell::U8(_) => Type::U8,
        ValueCell::I32(_) => Type::I32,
        ValueCell::F32(_) => Type::F32,
        ValueCell::Structure(struct_cell) => struct_cell.struct_ty(),
        ValueCell::Variant(variant_cell) => variant_cell.variant_ty(),
        ValueCell::Pointer(..) => Type::Nothing.ptr(),
        ValueCell::Function(_) => Type::Nothing.ptr(),
        ValueCell::Array(array_cell) => array_cell.array_ty(),
        ValueCell::RcCell(_) => {
            return Err(MarshalError::UnsupportedValue(value.clone()));
        },
    };

    Ok(ty)
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
            let bytes_copied = marshal(&arg_val, &mut args[arg_start..])
                .map_err(|err| ExecError::MarshallingFailed {
                    err,
                    span: state.debug_ctx().into_owned(),
                })?;

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
                let return_val = unmarshal(result_slice.as_ref().unwrap(), &self.return_ty)
                    .map_err(|err| ExecError::MarshallingFailed {
                        err,
                        span: state.debug_ctx().into_owned(),
                    })?;

                let return_local = Ref::Local(LocalID(0));
                state.store(&return_local, return_val)?;
            }
        }

        Ok(())
    }
}

pub fn marshal(val: &ValueCell, out_bytes: &mut [u8]) -> MarshalResult<usize> {
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
                .map_err(|_err| {
                    MarshalError::UnsupportedType(x.array_ty())
                })?;

            let mut el_offset = from_bytes(&dim.to_ne_bytes(), out_bytes);

            for i in 0..dim {
                let el = &x.elements[i as usize];
                let el_bytes = &mut out_bytes[el_offset..];
                el_offset += marshal(el, el_bytes)?;
            }

            el_offset
        }

        ValueCell::Pointer(Pointer::Native(NativePointer { addr, .. })) => {
            let ptr_bytes = (*addr).to_ne_bytes();
            from_bytes(&ptr_bytes, out_bytes)
        }

        unsupported => {
            return Err(MarshalError::UnsupportedValue(unsupported.clone()))
        }
    };

    Ok(size)
}

pub fn unmarshal(in_bytes: &[u8], ty: &Type) -> MarshalResult<ValueCell> {
    match ty {
        Type::I32 => {
            let in_bytes = in_bytes.try_into()
                .map_err(|_| MarshalError::UnsupportedType(ty.clone()))?;
            let val = i32::from_ne_bytes(in_bytes);
            Ok(ValueCell::I32(val))
        }

        Type::Pointer(deref_ty) => {
            let in_bytes = in_bytes.try_into()
                .map_err(|_| MarshalError::UnsupportedType(ty.clone()))?;
            let addr = usize::from_ne_bytes(in_bytes);
            Ok(ValueCell::Pointer(Pointer::Native(NativePointer {
                addr,
                ty: (**deref_ty).clone()
            })))
        }

        _ => Err(MarshalError::UnsupportedType(ty.clone()))
    }
}

pub trait ForeignTypeExt {
    fn size(&self) -> usize;
}

impl ForeignTypeExt for ForeignType {
    fn size(&self) -> usize {
        unsafe { *self.as_raw_ptr() }.size
    }
}