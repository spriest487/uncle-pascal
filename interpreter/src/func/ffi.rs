use std::{iter, collections::HashMap, cmp::max, ptr::slice_from_raw_parts, rc::Rc, fmt};
use std::borrow::Cow;
use std::collections::BTreeMap;
use std::convert::TryInto;
use std::ffi::c_void;
use std::mem::{size_of, transmute};
use crate::{ExecError, ExecResult, Interpreter, ValueCell, Pointer, ArrayCell, VariantCell, StructCell, RcCell};
use ::dlopen::{
    Error as DlopenError,
    raw as dlopen,
};
use ::libffi::{
    middle::{Cif, Type as ForeignType, Builder as FfiBuilder},
    raw::ffi_call as ffi_raw_call,
};
use libffi::low::ffi_type;
use libffi::raw::FFI_TYPE_STRUCT;
use pas_ir::{metadata::Metadata, ExternalFunctionRef, LocalID, Ref, Type};
use pas_ir::metadata::Variant;
use pas_ir::prelude::{Struct, StructID};
use crate::heap::native_heap::NativePointer;

#[derive(Clone, Debug)]
pub enum MarshalError {
    InvalidData,

    UnsupportedType(Type),
    UnsupportedValue(ValueCell),

    VariantTagOutOfRange {
        variant_id: StructID,
        tag: ValueCell,
    },

    InvalidRefCountValue(ValueCell),

    ExternSymbolLoadFailed {
        lib: String,
        symbol: String,
        msg: String,
    }
}

impl fmt::Display for MarshalError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            MarshalError::InvalidData => write!(f, "invalid data"),
            MarshalError::UnsupportedValue(val) => write!(f, "unable to marshal value: {:?}", val),
            MarshalError::UnsupportedType(ty) => write!(f, "unable to marshal type: {}", ty),
            MarshalError::ExternSymbolLoadFailed { lib, symbol, msg } => write!(f, "external symbol {}::{} failed to load: {}", lib, symbol, msg),
            MarshalError::VariantTagOutOfRange { variant_id, tag } => write!(f, "tag {:?} for variant {} was out of range", tag, variant_id),
            MarshalError::InvalidRefCountValue(val) => write!(f, "value is not a valid ref count: {:?}", val),
        }
    }
}

pub type MarshalResult<T> = Result<T, MarshalError>;

#[derive(Debug, Clone)]
pub struct UnmarshalledValue {
    pub value: ValueCell,
    pub byte_count: usize,
}

impl From<UnmarshalledValue> for ValueCell {
    fn from(val: UnmarshalledValue) -> Self {
        val.value
    }
}

#[derive(Debug, Clone)]
pub struct FfiCache {
    types: HashMap<Type, ForeignType>,
    libs: HashMap<String, Rc<dlopen::Library>>,

    struct_field_types: BTreeMap<StructID, Vec<Type>>,
    variant_case_types: BTreeMap<StructID, Vec<Option<Type>>>,

    rc_cell_type: ForeignType,
}

impl FfiCache {
    pub fn new() -> Self {
        let rc_cell_type = ForeignType::structure(vec![
            ForeignType::pointer(), // resource address
            ForeignType::usize(), // ref count
            ForeignType::usize(), // struct ID
        ]);

        let mut ffi_cache = Self {
            types: HashMap::new(),
            libs: HashMap::new(),

            struct_field_types: BTreeMap::new(),
            variant_case_types: BTreeMap::new(),

            rc_cell_type,
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
        let def_field_tys: Vec<_> = def_fields.into_iter().map(|(_, field)| field.ty.clone()).collect();

        let mut field_ffi_tys = Vec::with_capacity(def_field_tys.len());
        for def_field_ty in &def_field_tys {
            field_ffi_tys.push(self.build_marshalled_type(def_field_ty, metadata)?);
        }

        let struct_ffi_ty = ForeignType::structure(field_ffi_tys.iter().cloned());

        self.types.insert(struct_ty, struct_ffi_ty.clone());
        self.struct_field_types.insert(id, def_field_tys);

        Ok(struct_ffi_ty)
    }

    pub fn add_variant(&mut self, id: StructID, def: &Variant, metadata: &Metadata) -> MarshalResult<ForeignType> {
        let variant_ty = Type::Variant(id);
        if let Some(cached) = self.types.get(&variant_ty) {
            return Ok(cached.clone());
        }

        let case_tys: Vec<_> = def.cases.iter().map(|case| case.ty.clone()).collect();

        let mut case_ffi_tys = Vec::with_capacity(case_tys.len());
        let mut max_case_size = 0;

        for case_ty in &case_tys {
            let case_ffi_ty = match case_ty.as_ref() {
                Some(case_ty) => self.build_marshalled_type(case_ty, metadata)?,
                None => ForeignType::void(),
            };

            max_case_size = max(case_ffi_ty.size(), max_case_size);

            case_ffi_tys.push(case_ffi_ty);
        }

        let variant_layout: Vec<_> = iter::once(ForeignType::i32())
            .chain(iter::repeat(ForeignType::u8()).take(max_case_size))
            .collect();

        let variant_ffi_ty = ForeignType::structure(variant_layout);

        self.types.insert(variant_ty, variant_ffi_ty.clone());

        self.variant_case_types.insert(id, case_tys);

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
                    .ok_or_else(|| {
                        MarshalError::UnsupportedType(ty.clone())
                    })?;

                self.add_variant(*id, def, metadata)
            }

            Type::Struct(id) => {
                let def = metadata.get_struct_def(*id)
                    .ok_or_else(|| {
                        MarshalError::UnsupportedType(ty.clone())
                    })?;

                self.add_struct(*id, def, metadata)
            }

            Type::RcPointer(..)
            | Type::Pointer(..) => {
                Ok(ForeignType::pointer())
            }

            Type::Array { element, dim } => {
                let el_ty = self.build_marshalled_type(&element, metadata)?;
                let el_tys = vec![el_ty; *dim];

                Ok(ForeignType::structure(el_tys))
            }

            Type::RcObject(..) => {
                Ok(self.rc_cell_type.clone())
            }

            // all primitives/builtins should be in the cache already
            _ => {
                Err(MarshalError::UnsupportedType(ty.clone()))
            }
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

            Type::RcObject(..) => {
                Ok(self.rc_cell_type.clone())
            }

            ty => match self.types.get(ty) {
                Some(cached_ty) => Ok(cached_ty.clone()),
                None => Err(MarshalError::UnsupportedType(ty.clone())),
            }
        }
    }

    pub fn marshal(&self, val: &ValueCell, out_bytes: &mut [u8]) -> MarshalResult<usize> {
        let size = match val {
            ValueCell::I32(x) => marshal_bytes(&x.to_ne_bytes(), out_bytes),
            ValueCell::F32(x) => marshal_bytes(&x.to_ne_bytes(), out_bytes),
            ValueCell::U8(x) => marshal_bytes(&x.to_ne_bytes(), out_bytes),
            ValueCell::Bool(x) => {
                out_bytes[0] = if *x { 1 } else { 0 };
                1
            },

            ValueCell::Array(x) => {
                let mut el_offset = 0;

                for i in 0..x.elements.len() {
                    let el = &x.elements[i as usize];
                    let el_bytes = &mut out_bytes[el_offset..];
                    el_offset += self.marshal(el, el_bytes)?;
                }

                el_offset
            }

            ValueCell::Structure(struct_cell) => {
                let mut fields_size = 0;
                for field in &struct_cell.fields {
                    let field_bytes = &mut out_bytes[fields_size..];
                    fields_size += self.marshal(field, field_bytes)?;
                }

                fields_size
            }

            ValueCell::Variant(variant_cell) => {
                let tag_size = self.marshal(&variant_cell.tag, out_bytes)?;
                let data_size = self.marshal(&variant_cell.data, &mut out_bytes[tag_size..])?;

                tag_size + data_size
            }

            ValueCell::Pointer(ptr) => {
                self.marshal_ptr(ptr, out_bytes)?
            }

            ValueCell::RcCell(rc_cell) => {
                let mut offset = self.marshal_ptr(&rc_cell.resource_ptr, out_bytes)?;

                let ref_count_bytes = rc_cell.ref_count.to_ne_bytes();
                offset += marshal_bytes(&ref_count_bytes, &mut out_bytes[offset..]);

                let struct_id_bytes = rc_cell.struct_id.0.to_ne_bytes();
                marshal_bytes(&struct_id_bytes, &mut out_bytes[offset..])
            }

            unsupported => {
                return Err(MarshalError::UnsupportedValue(unsupported.clone()))
            }
        };

        Ok(size)
    }

    fn marshal_ptr(&self, ptr: &Pointer, out_bytes: &mut [u8]) -> MarshalResult<usize> {
        let native_ptr = self.flatten_ptr(ptr).ok_or_else(|| {
            MarshalError::UnsupportedValue(ValueCell::Pointer(ptr.clone()))
        })?;

        let ptr_bytes = native_ptr.addr.to_ne_bytes();
        let len = marshal_bytes(&ptr_bytes, out_bytes);
        Ok(len)
    }

    fn unmarshal_ptr(&self, deref_ty: &Type, in_bytes: &[u8]) -> MarshalResult<(NativePointer, usize)> {
        let addr = usize::from_ne_bytes(unmarshal_bytes(in_bytes)?);
        let ptr = NativePointer {
            addr,
            ty: deref_ty.clone(),
        };

        Ok((ptr, size_of::<usize>()))
    }

    fn flatten_ptr<'a>(&self, ptr: &'a Pointer) -> Option<Cow<'a, NativePointer>> {
        match ptr {
            Pointer::Null => Some(Cow::Owned(NativePointer {
                addr: 0,
                ty: Type::Nothing,
            })),

            Pointer::Native(native_ptr) => Some(Cow::Borrowed(native_ptr)),

            Pointer::IntoArray { array, offset } => {
                let array = self.flatten_ptr(array)?;
                let element_ty = match &array.ty {
                    Type::Array { element, .. } => Some(*element.clone()),
                    _ => None,
                }?;

                Some(Cow::Owned(NativePointer {
                    addr: array.addr + *offset,
                    ty: element_ty,
                }))
            }

            Pointer::IntoStruct { structure, field } => {
                let struct_ptr = self.flatten_ptr(structure)?;
                let struct_id = struct_ptr.ty.as_struct()?;
                let struct_fields = self.struct_field_types.get(&struct_id)?;

                let mut offset = 0;
                for (field_id, field_ty) in struct_fields.iter().enumerate() {
                    if field_id == field.0 {
                        let field_addr = struct_ptr.addr + offset;
                        return Some(Cow::Owned(NativePointer {
                            addr: field_addr,
                            ty: field_ty.clone(),
                        }));
                    } else {
                        let field_ffi_ty = self.get_ty(&field_ty).ok()?;
                        offset += field_ffi_ty.size();
                    }
                }

                None
            },

            Pointer::VariantTag { variant } => {
                // variant tag is the first member of the variant
                self.flatten_ptr(variant)
            }

            Pointer::VariantData { variant, tag } => {
                let variant_ptr = self.flatten_ptr(variant)?;
                let variant_id = match &variant_ptr.ty {
                    Type::Variant(id) => Some(*id),
                    _ => None,
                }?;

                let variant_cases = self.variant_case_types.get(&variant_id)?;

                // double ? because the variant must have this case AND this case must not be empty
                let case_ty = variant_cases.get(*tag)?.as_ref()?;

                Some(Cow::Owned(NativePointer {
                    addr: variant_ptr.addr + ForeignType::i32().size(),
                    ty: case_ty.clone(),
                }))
            }

            // unsupported
            _ => None,
        }
    }

    pub fn unmarshal(&self, in_bytes: &[u8], ty: &Type) -> MarshalResult<UnmarshalledValue> {
        match ty {
            Type::I32 => {
                let value = ValueCell::I32(i32::from_ne_bytes(unmarshal_bytes(in_bytes)?));
                Ok(UnmarshalledValue {
                    value,
                    byte_count: size_of::<i32>(),
                })
            }

            Type::F32 => {
                let value = ValueCell::F32(f32::from_ne_bytes(unmarshal_bytes(in_bytes)?));
                Ok(UnmarshalledValue {
                    value,
                    byte_count: size_of::<f32>(),
                })
            }

            Type::U8 => {
                let value = ValueCell::U8(u8::from_ne_bytes(unmarshal_bytes(in_bytes)?));

                Ok(UnmarshalledValue {
                    value,
                    byte_count: size_of::<u8>(),
                })
            }

            Type::Bool => {
                if in_bytes.len() == 0 {
                    return Err(MarshalError::InvalidData);
                }

                let value = ValueCell::Bool(in_bytes[0] != 0);
                Ok(UnmarshalledValue {
                    value,
                    byte_count: 1,
                })
            }

            Type::RcPointer(..) => {
                let (raw_ptr, size) = self.unmarshal_ptr(&Type::Nothing, in_bytes)?;

                // deref the object to get its struct ID for the type
                let rc_struct_bytes_ptr = raw_ptr.addr as *const u8;
                if rc_struct_bytes_ptr.is_null() {
                    return Err(MarshalError::InvalidData);
                }

                // the struct ID is the 3rd field after the ptr and the ref count
                let rc_cell_fields = self.rc_cell_type.elements();
                let struct_id_offset = rc_cell_fields[0].size() + rc_cell_fields[1].size();

                let rc_struct_bytes = unsafe {
                    slice_from_raw_parts(rc_struct_bytes_ptr, rc_cell_fields[2].size())
                        .as_ref()
                        .unwrap()
                };

                let struct_id_bytes = &rc_struct_bytes[struct_id_offset..];
                let struct_id = usize::from_ne_bytes(unmarshal_bytes(struct_id_bytes)?);

                Ok(UnmarshalledValue {
                    value: ValueCell::Pointer(Pointer::Native(NativePointer {
                        addr: raw_ptr.addr,
                        ty: Type::RcObject(StructID(struct_id)),
                    })),
                    byte_count: size,
                })
            }

            Type::RcObject(id) => {
                let (resource_ptr, mut offset) = self.unmarshal_ptr(&Type::Struct(*id), in_bytes)?;

                let ref_count_bytes = unmarshal_bytes(in_bytes)?;
                let ref_count = usize::from_ne_bytes(ref_count_bytes);
                offset += ref_count_bytes.len();

                // we don't need to read the struct ID, but include it in the byte count
                offset += ForeignType::usize().size();

                Ok(UnmarshalledValue {
                    value: ValueCell::RcCell(Box::new(RcCell {
                        resource_ptr: Pointer::Native(resource_ptr),
                        struct_id: *id,
                        ref_count,
                    })),
                    byte_count: offset,
                })
            }

            Type::Pointer(deref_ty) => {
                let (ptr, size) = self.unmarshal_ptr(deref_ty, in_bytes)?;

                Ok(UnmarshalledValue {
                    value: ValueCell::Pointer(Pointer::Native(ptr)),
                    byte_count: size,
                })
            }

            Type::Array { element, dim } => {
                let mut offset = 0;
                let mut elements = Vec::with_capacity(*dim as usize);
                for _ in 0..*dim {
                    let el_val = self.unmarshal(&in_bytes[offset..], &element)?;
                    offset += el_val.byte_count;
                    elements.push(el_val.value);
                }

                Ok(UnmarshalledValue {
                    byte_count: offset,
                    value: ValueCell::Array(Box::new(ArrayCell {
                        el_ty: *element.clone(),
                        elements
                    }))
                })
            }

            // these need field offset/tag type info from the ffi cache so marshal/unmarshla should
            // be members
            Type::Struct(struct_id) => {
                let field_tys = self.struct_field_types.get(struct_id)
                    .ok_or_else(|| {
                        MarshalError::UnsupportedType(ty.clone())
                    })?;

                let mut offset = 0;
                let mut fields = Vec::new();

                for field_ty in field_tys {
                    let field_val = self.unmarshal(&in_bytes[offset..], field_ty)?;
                    offset += field_val.byte_count;
                    fields.push(field_val.value);
                }

                Ok(UnmarshalledValue {
                    value: ValueCell::Structure(Box::new(StructCell {
                        id: *struct_id,
                        fields,
                    })),
                    byte_count: offset,
                })
            },

            Type::Variant(variant_id) => {
                let tag_val = self.unmarshal(in_bytes, &Type::I32)?;
                let tag = tag_val.value.as_i32().unwrap();

                let case_index = cast::usize(tag).map_err(|_| {
                    MarshalError::VariantTagOutOfRange {
                        variant_id: *variant_id,
                        tag: tag_val.value.clone(),
                    }
                })?;

                let case_tys = self.variant_case_types.get(variant_id)
                    .ok_or_else(|| {
                        MarshalError::UnsupportedType(ty.clone())
                    })?;

                let case_ty = case_tys.get(case_index)
                    .ok_or_else(|| MarshalError::VariantTagOutOfRange {
                        variant_id: *variant_id,
                        tag: tag_val.value.clone(),
                    })?
                    .as_ref();

                let data_val = match case_ty {
                    Some(case_ty) => {
                        self.unmarshal(&in_bytes[tag_val.byte_count..], case_ty)?
                    }
                    None => UnmarshalledValue {
                        value: ValueCell::Pointer(Pointer::Null),
                        byte_count: 0,
                    }
                };

                Ok(UnmarshalledValue {
                    byte_count: tag_val.byte_count + data_val.byte_count,
                    value: ValueCell::Variant(Box::new(VariantCell {
                        id: *variant_id,
                        tag: Box::new(ValueCell::I32(tag as i32)),
                        data: Box::new(data_val.value),
                    }))
                })
            }

            _ => {
                Err(MarshalError::UnsupportedType(ty.clone()))
            }
        }
    }
}

fn marshal_bytes(bytes: &[u8], out_bytes: &mut [u8]) -> usize {
    out_bytes[0..bytes.len()].copy_from_slice(bytes);
    bytes.len()
}

fn unmarshal_bytes<const COUNT: usize>(in_bytes: &[u8]) -> MarshalResult<[u8; COUNT]> {
    if in_bytes.len() < COUNT {
        return Err(MarshalError::InvalidData);
    }

    in_bytes[0..COUNT].try_into().map_err(|_| MarshalError::InvalidData)
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
            let bytes_copied = state.marshaller().marshal(&arg_val, &mut args[arg_start..])
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
                let return_val = state.marshaller().unmarshal(result_slice.as_ref().unwrap(), &self.return_ty)
                    .map_err(|err| ExecError::MarshallingFailed {
                        err,
                        span: state.debug_ctx().into_owned(),
                    })?;

                let return_local = Ref::Local(LocalID(0));
                state.store(&return_local, return_val.value)?;
            }
        }

        Ok(())
    }
}

pub trait ForeignTypeExt {
    fn size(&self) -> usize;
    fn elements(&self) -> &[ForeignType];
}

impl ForeignTypeExt for ForeignType {
    fn size(&self) -> usize {
        let raw_ty = self.as_raw_ptr();

        unsafe {
            if (*raw_ty).type_ as u32 == FFI_TYPE_STRUCT {
                let elements = self.elements();
                let mut total = 0;
                for element in elements {
                    total += element.size();
                }

                total
            } else {
                (*raw_ty).size
            }
        }
    }

    fn elements(&self) -> &[ForeignType] {
        let raw_ty = self.as_raw_ptr();

        unsafe {
            let mut i: usize = 0;
            let count = loop {
                let next_element: *mut *mut ffi_type = (*raw_ty).elements.offset(i as isize);
                if (*next_element).is_null() {
                    break i;
                } else {
                    i += 1;
                }
            };

            let slice = slice_from_raw_parts::<ForeignType>(transmute((*raw_ty).elements), count);
            slice.as_ref().unwrap()
        }
    }
}