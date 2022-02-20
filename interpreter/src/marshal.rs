#[cfg(test)]
mod test;

use crate::func::ffi::FfiInvoker;
use crate::{ArrayValue, DynValue, Pointer, RcValue, StructValue, VariantValue};
use ::dlopen::{raw as dlopen, Error as DlopenError};
use libffi::{
    low::ffi_type,
    middle::{Builder as FfiBuilder, Type as FfiType},
    raw::FFI_TYPE_STRUCT,
};
use pas_ir::{prelude::*, ExternalFunctionRef, InstructionFormatter, RawInstructionFormatter};
use std::{
    cmp::max,
    collections::{BTreeMap, HashMap},
    convert::TryInto,
    fmt, iter,
    mem::transmute,
    ptr::{slice_from_raw_parts, slice_from_raw_parts_mut},
    rc::Rc,
};

#[derive(Clone, Debug)]
pub enum MarshalError {
    InvalidData,

    UnsupportedType(Type),
    UnsupportedValue(DynValue),

    VariantTagOutOfRange {
        variant_id: StructID,
        tag: DynValue,
    },

    InvalidStructID {
        expected: StructID,
        actual: StructID,
    },

    InvalidRefCountValue(DynValue),

    ExternSymbolLoadFailed {
        lib: String,
        symbol: String,
        msg: String,
    },
}

impl fmt::Display for MarshalError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.fmt_pretty(f, &RawInstructionFormatter)
    }
}

impl MarshalError {
    pub fn fmt_pretty<Fmt>(&self, f: &mut fmt::Formatter, format: &Fmt) -> fmt::Result
    where
        Fmt: InstructionFormatter,
    {
        match self {
            MarshalError::InvalidData => write!(f, "invalid data"),
            MarshalError::InvalidStructID { expected, actual } => {
                write!(f, "expected struct ")?;
                format.format_type(&Type::Struct(*expected), f)?;
                write!(f, ", got ")?;
                format.format_type(&Type::Struct(*actual), f)?;
                Ok(())
            },
            MarshalError::UnsupportedValue(val) => {
                write!(f, "unable to marshal value: {:?}", val)
            },
            MarshalError::UnsupportedType(ty) => {
                write!(f, "unable to marshal type: ")?;
                format.format_type(ty, f)?;
                Ok(())
            },
            MarshalError::ExternSymbolLoadFailed { lib, symbol, msg } => write!(
                f,
                "external symbol {}::{} failed to load ({})",
                lib, symbol, msg
            ),
            MarshalError::VariantTagOutOfRange { variant_id, tag } => write!(
                f,
                "tag {:?} for variant {} was out of range",
                tag, variant_id
            ),
            MarshalError::InvalidRefCountValue(val) => {
                write!(f, "value is not a valid ref count: {:?}", val)
            },
        }
    }
}

pub type MarshalResult<T> = Result<T, MarshalError>;

#[derive(Debug, Clone)]
pub struct UnmarshalledValue<T> {
    pub value: T,
    pub byte_count: usize,
}

impl From<UnmarshalledValue<DynValue>> for DynValue {
    fn from(val: UnmarshalledValue<DynValue>) -> Self {
        val.value
    }
}

impl<T> UnmarshalledValue<T> {
    pub fn map<U, F>(self, f: F) -> UnmarshalledValue<U>
    where
        F: FnOnce(T) -> U,
    {
        UnmarshalledValue {
            value: f(self.value),
            byte_count: self.byte_count,
        }
    }
}

#[repr(transparent)]
#[derive(Clone, Debug)]
pub struct ForeignType(FfiType);

impl ForeignType {
    pub fn structure<I>(fields: I) -> Self
    where
        I: IntoIterator<Item = ForeignType>,
        I::IntoIter: ExactSizeIterator<Item = ForeignType>,
    {
        Self(FfiType::structure(fields.into_iter().map(|t| t.0)))
    }

    pub fn pointer() -> Self {
        Self(FfiType::pointer())
    }

    pub fn i8() -> Self {
        Self(FfiType::i8())
    }

    pub fn u8() -> Self {
        Self(FfiType::u8())
    }

    pub fn i16() -> Self {
        Self(FfiType::i16())
    }

    pub fn u16() -> Self {
        Self(FfiType::u16())
    }

    pub fn i32() -> Self {
        Self(FfiType::i32())
    }

    pub fn u32() -> Self {
        Self(FfiType::u32())
    }

    pub fn i64() -> Self {
        Self(FfiType::i64())
    }

    pub fn u64() -> Self {
        Self(FfiType::u64())
    }

    pub fn isize() -> Self {
        Self(FfiType::isize())
    }

    pub fn usize() -> Self {
        Self(FfiType::usize())
    }

    pub fn f32() -> Self {
        Self(FfiType::f32())
    }

    pub fn size(&self) -> usize {
        let raw_ty = self.0.as_raw_ptr();

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

    pub fn elements(&self) -> &[ForeignType] {
        let raw_ty = self.0.as_raw_ptr();

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

#[derive(Debug, Clone)]
pub struct Marshaller {
    types: HashMap<Type, ForeignType>,
    libs: HashMap<String, Rc<dlopen::Library>>,

    struct_field_types: BTreeMap<StructID, Vec<Type>>,
    variant_case_types: BTreeMap<StructID, Vec<Option<Type>>>,

    rc_val_type: ForeignType,
}

impl Marshaller {
    pub fn new() -> Self {
        let rc_val_type = ForeignType::structure(vec![
            ForeignType::pointer(), // resource address
            ForeignType::usize(),   // ref count
            ForeignType::usize(),   // struct ID
        ]);

        let mut marshaller = Self {
            types: HashMap::new(),
            libs: HashMap::new(),

            struct_field_types: BTreeMap::new(),
            variant_case_types: BTreeMap::new(),

            rc_val_type,
        };

        marshaller.types.insert(Type::I8, ForeignType::i8());
        marshaller.types.insert(Type::U8, ForeignType::u8());
        marshaller.types.insert(Type::I16, ForeignType::i16());
        marshaller.types.insert(Type::U16, ForeignType::u16());
        marshaller.types.insert(Type::I32, ForeignType::i32());
        marshaller.types.insert(Type::U32, ForeignType::u32());
        marshaller.types.insert(Type::I64, ForeignType::i64());
        marshaller.types.insert(Type::U64, ForeignType::u64());
        marshaller.types.insert(Type::ISize, ForeignType::isize());
        marshaller.types.insert(Type::USize, ForeignType::usize());
        marshaller.types.insert(Type::F32, ForeignType::f32());
        marshaller.types.insert(Type::Bool, ForeignType::u8());

        marshaller
    }

    pub fn variant_tag_type(&self) -> ForeignType {
        ForeignType::i32()
    }

    pub fn add_struct(
        &mut self,
        id: StructID,
        def: &Struct,
        metadata: &Metadata,
    ) -> MarshalResult<ForeignType> {
        let struct_ty = Type::Struct(id);
        if let Some(cached) = self.types.get(&struct_ty) {
            return Ok(cached.clone());
        }

        let mut def_fields: Vec<_> = def.fields.iter().collect();
        def_fields.sort_by_key(|(f_id, _)| **f_id);
        let def_field_tys: Vec<_> = def_fields
            .into_iter()
            .map(|(_, field)| field.ty.clone())
            .collect();

        let mut field_ffi_tys = Vec::with_capacity(def_field_tys.len());
        for def_field_ty in &def_field_tys {
            field_ffi_tys.push(self.build_marshalled_type(def_field_ty, metadata)?);
        }

        let struct_ffi_ty = ForeignType::structure(field_ffi_tys.iter().cloned());

        self.types.insert(struct_ty, struct_ffi_ty.clone());
        self.struct_field_types.insert(id, def_field_tys);

        Ok(struct_ffi_ty)
    }

    pub fn add_variant(
        &mut self,
        id: StructID,
        def: &Variant,
        metadata: &Metadata,
    ) -> MarshalResult<ForeignType> {
        let variant_ty = Type::Variant(id);
        if let Some(cached) = self.types.get(&variant_ty) {
            return Ok(cached.clone());
        }

        let case_tys: Vec<_> = def.cases.iter().map(|case| case.ty.clone()).collect();

        let mut case_ffi_tys = Vec::with_capacity(case_tys.len());
        let mut max_case_size = 0;

        for case_ty in &case_tys {
            if let Some(case_ty) = case_ty.as_ref() {
                let case_ffi_ty = self.build_marshalled_type(case_ty, metadata)?;

                max_case_size = max(case_ffi_ty.size(), max_case_size);

                case_ffi_tys.push(case_ffi_ty);
            };
        }

        let variant_layout: Vec<_> = iter::once(self.variant_tag_type())
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
        let sym_load_err = |err: DlopenError| MarshalError::ExternSymbolLoadFailed {
            lib: func_ref.src.clone(),
            symbol: func_ref.symbol.clone(),
            msg: err.to_string(),
        };

        // the "nothing" type is usually not allowed by the marshaller because it can't be
        // instantiated, but here we need to map it to the void ffi type
        let ffi_return_ty = match &func_ref.return_ty {
            Type::Nothing => ForeignType(FfiType::void()),
            return_ty => self.build_marshalled_type(return_ty, metadata)?,
        };

        let ffi_param_tys: Vec<_> = func_ref
            .params
            .iter()
            .map(|ty| self.build_marshalled_type(ty, metadata))
            .collect::<MarshalResult<_>>()?;

        let cif = FfiBuilder::new()
            .args(ffi_param_tys.iter().map(|t| t.0.clone()))
            .res(ffi_return_ty.0.clone())
            .into_cif();

        let lib = match self.libs.get(&func_ref.src) {
            Some(lib_rc) => lib_rc.clone(),
            None => {
                let lib = dlopen::Library::open(&func_ref.src).map_err(sym_load_err)?;

                let lib_rc = Rc::new(lib);
                self.libs.insert(func_ref.src.clone(), lib_rc.clone());
                lib_rc
            },
        };

        let symbol = unsafe {
            lib.symbol::<*const ()>(&func_ref.symbol)
                .map_err(sym_load_err)?
        };

        Ok(FfiInvoker::new(
            cif,
            symbol,
            ffi_param_tys,
            func_ref.return_ty.clone(),
            ffi_return_ty,
        ))
    }

    fn build_marshalled_type(
        &mut self,
        ty: &Type,
        metadata: &Metadata,
    ) -> MarshalResult<ForeignType> {
        if let Some(cached) = self.types.get(ty) {
            return Ok(cached.clone());
        }

        match ty {
            Type::Variant(id) => {
                let def = metadata
                    .get_variant_def(*id)
                    .ok_or_else(|| MarshalError::UnsupportedType(ty.clone()))?;

                self.add_variant(*id, def, metadata)
            },

            Type::Struct(id) => {
                let def = metadata
                    .get_struct_def(*id)
                    .ok_or_else(|| MarshalError::UnsupportedType(ty.clone()))?;

                self.add_struct(*id, def, metadata)
            },

            Type::RcPointer(..) | Type::Pointer(..) | Type::Function(..) => Ok(ForeignType::pointer()),

            Type::Array { element, dim } => {
                let el_ty = self.build_marshalled_type(&element, metadata)?;
                let el_tys = vec![el_ty; *dim];

                Ok(ForeignType::structure(el_tys))
            },

            Type::RcObject(..) => Ok(self.rc_val_type.clone()),

            // all primitives/builtins should be in the cache already
            _ => Err(MarshalError::UnsupportedType(ty.clone())),
        }
    }

    pub fn get_ty(&self, ty: &Type) -> MarshalResult<ForeignType> {
        match ty {
            Type::Nothing => {
                // "nothing" is not a marshallable type!
                Err(MarshalError::UnsupportedType(Type::Nothing))
            },

            // we can't cache array types so always build them on demand
            // pascal static arrays are treated as a struct of elements laid out sequentially
            Type::Array { element, dim } => {
                let el_ty = self.get_ty(&element)?;
                let el_tys = vec![el_ty; *dim];

                Ok(ForeignType::structure(el_tys))
            },

            Type::Pointer(..) | Type::RcPointer(..) | Type::Function(..) => Ok(ForeignType::pointer()),

            Type::RcObject(..) => Ok(self.rc_val_type.clone()),

            ty => match self.types.get(ty) {
                Some(cached_ty) => Ok(cached_ty.clone()),
                None => Err(MarshalError::UnsupportedType(ty.clone())),
            },
        }
    }

    pub fn marshal(&self, val: &DynValue, out_bytes: &mut [u8]) -> MarshalResult<usize> {
        let size = match val {
            DynValue::I8(x) => marshal_bytes(&x.to_ne_bytes(), out_bytes),
            DynValue::U8(x) => marshal_bytes(&x.to_ne_bytes(), out_bytes),
            DynValue::I16(x) => marshal_bytes(&x.to_ne_bytes(), out_bytes),
            DynValue::U16(x) => marshal_bytes(&x.to_ne_bytes(), out_bytes),
            DynValue::I32(x) => marshal_bytes(&x.to_ne_bytes(), out_bytes),
            DynValue::U32(x) => marshal_bytes(&x.to_ne_bytes(), out_bytes),
            DynValue::I64(x) => marshal_bytes(&x.to_ne_bytes(), out_bytes),
            DynValue::U64(x) => marshal_bytes(&x.to_ne_bytes(), out_bytes),
            DynValue::ISize(x) => marshal_bytes(&x.to_ne_bytes(), out_bytes),
            DynValue::USize(x) => marshal_bytes(&x.to_ne_bytes(), out_bytes),
            DynValue::F32(x) => marshal_bytes(&x.to_ne_bytes(), out_bytes),
            DynValue::Bool(x) => {
                out_bytes[0] = if *x { 1 } else { 0 };
                1
            },

            DynValue::Array(x) => {
                let mut el_offset = 0;

                for i in 0..x.elements.len() {
                    let el = &x.elements[i as usize];
                    let el_bytes = &mut out_bytes[el_offset..];
                    el_offset += self.marshal(el, el_bytes)?;
                }

                el_offset
            },

            DynValue::Structure(struct_val) => {
                let mut fields_size = 0;
                for field in &struct_val.fields {
                    let field_bytes = &mut out_bytes[fields_size..];
                    fields_size += self.marshal(field, field_bytes)?;
                }

                fields_size
            },

            DynValue::Variant(variant_val) => self.marshal_variant(variant_val, out_bytes)?,

            DynValue::Pointer(ptr) => self.marshal_ptr(ptr, out_bytes)?,

            DynValue::Function(func_id) => {
                marshal_bytes(&func_id.0.to_ne_bytes(), out_bytes)
            }

            DynValue::Rc(rc_val) => self.marshal_rc(rc_val, out_bytes)?,
        };

        Ok(size)
    }

    fn marshal_ptr(&self, ptr: &Pointer, out_bytes: &mut [u8]) -> MarshalResult<usize> {
        let ptr_bytes = ptr.addr.to_ne_bytes();
        let len = marshal_bytes(&ptr_bytes, out_bytes);
        Ok(len)
    }

    fn unmarshal_ptr(&self, deref_ty: Type, in_bytes: &[u8]) -> MarshalResult<(Pointer, usize)> {
        let addr_bytes = unmarshal_bytes(in_bytes)?;
        let addr = usize::from_ne_bytes(addr_bytes);
        let ptr = Pointer {
            addr,
            ty: deref_ty.clone(),
        };

        Ok((ptr, addr_bytes.len()))
    }

    pub fn unmarshal_from_ptr(&self, into_ptr: &Pointer) -> MarshalResult<DynValue> {
        if into_ptr.addr == 0 {
            return Err(MarshalError::InvalidData);
        }

        let marshal_ty = self.get_ty(&into_ptr.ty)?;

        let mem_slice = slice_from_raw_parts(into_ptr.addr as *const u8, marshal_ty.size());
        let result = unsafe { self.unmarshal(mem_slice.as_ref().unwrap(), &into_ptr.ty)? };

        Ok(result.value)
    }

    pub fn marshal_into(&self, val: &DynValue, into_ptr: &Pointer) -> MarshalResult<usize> {
        if into_ptr.addr == 0 {
            return Err(MarshalError::InvalidData);
        }

        let marshal_ty = self.get_ty(&into_ptr.ty)?;

        let mem_slice = slice_from_raw_parts_mut(into_ptr.addr as *mut u8, marshal_ty.size());
        let size = unsafe { self.marshal(val, mem_slice.as_mut().unwrap())? };

        Ok(size)
    }

    pub fn unmarshal(
        &self,
        in_bytes: &[u8],
        ty: &Type,
    ) -> MarshalResult<UnmarshalledValue<DynValue>> {
        let dyn_val = match ty {
            Type::I8 => unmarshal_from_ne_bytes(in_bytes, i8::from_ne_bytes)?.map(DynValue::I8),
            Type::U8 => unmarshal_from_ne_bytes(in_bytes, u8::from_ne_bytes)?.map(DynValue::U8),
            Type::I16 => unmarshal_from_ne_bytes(in_bytes, i16::from_ne_bytes)?.map(DynValue::I16),
            Type::U16 => unmarshal_from_ne_bytes(in_bytes, u16::from_ne_bytes)?.map(DynValue::U16),
            Type::I32 => unmarshal_from_ne_bytes(in_bytes, i32::from_ne_bytes)?.map(DynValue::I32),
            Type::U32 => unmarshal_from_ne_bytes(in_bytes, u32::from_ne_bytes)?.map(DynValue::U32),
            Type::I64 => unmarshal_from_ne_bytes(in_bytes, i64::from_ne_bytes)?.map(DynValue::I64),
            Type::U64 => unmarshal_from_ne_bytes(in_bytes, u64::from_ne_bytes)?.map(DynValue::U64),
            Type::ISize => {
                unmarshal_from_ne_bytes(in_bytes, isize::from_ne_bytes)?.map(DynValue::ISize)
            },
            Type::USize => {
                unmarshal_from_ne_bytes(in_bytes, usize::from_ne_bytes)?.map(DynValue::USize)
            },

            Type::F32 => unmarshal_from_ne_bytes(in_bytes, f32::from_ne_bytes)?.map(DynValue::F32),

            Type::Bool => {
                if in_bytes.len() == 0 {
                    return Err(MarshalError::InvalidData);
                }

                let value = DynValue::Bool(in_bytes[0] != 0);
                UnmarshalledValue {
                    value,
                    byte_count: 1,
                }
            },

            Type::RcPointer(class_id) => {
                let (raw_ptr, size) = self.unmarshal_ptr(Type::Nothing, in_bytes)?;

                // null rcpointers can exist - e.g. uninitialized stack values
                if raw_ptr.is_null() {
                    // it shouldn't really matter because you can't do anything with a null pointer,
                    // but let's try to return as correct a value as we can
                    let null_ty = match class_id {
                        // expected pointer to a concrete RC class object
                        Some(ClassID::Class(struct_id)) => Type::RcObject(Some(*struct_id)),

                        // abstract pointer - no idea what the actual rc type was
                        None | Some(ClassID::Interface(..)) => Type::RcObject(None),
                    };

                    UnmarshalledValue {
                        value: DynValue::Pointer(Pointer::null(null_ty)),
                        byte_count: size,
                    }
                } else {
                    // the struct ID is the 3rd field after the ptr and the ref count
                    let rc_fields = self.rc_val_type.elements();
                    let struct_id_offset = rc_fields[0].size() + rc_fields[1].size();

                    let rc_struct_bytes = unsafe {
                        slice_from_raw_parts(raw_ptr.addr as *const u8, self.rc_val_type.size())
                            .as_ref()
                            .unwrap()
                    };

                    let struct_id_bytes = &rc_struct_bytes[struct_id_offset..];
                    let struct_id = usize::from_ne_bytes(unmarshal_bytes(struct_id_bytes)?);

                    UnmarshalledValue {
                        value: DynValue::Pointer(Pointer {
                            addr: raw_ptr.addr,
                            ty: Type::RcObject(Some(StructID(struct_id))),
                        }),
                        byte_count: size,
                    }
                }
            },

            Type::RcObject(expect_id) => {
                let rc_val = self.unmarshal_rc(in_bytes)?;

                // we only need to check the rc type if we were expecting a particular concrete type
                if let Some(expect_id) = expect_id {
                    if rc_val.value.struct_id != *expect_id {
                        return Err(MarshalError::InvalidStructID {
                            expected: *expect_id,
                            actual: rc_val.value.struct_id,
                        });
                    }
                }

                rc_val.map(Box::new).map(DynValue::Rc)
            },

            Type::Pointer(deref_ty) => {
                let (ptr, size) = self.unmarshal_ptr((**deref_ty).clone(), in_bytes)?;

                UnmarshalledValue {
                    value: DynValue::Pointer(ptr),
                    byte_count: size,
                }
            },

            Type::Function(..) => {
                let func_id = unmarshal_from_ne_bytes(in_bytes, usize::from_ne_bytes)?;

                func_id.map(|id| {
                    DynValue::Function(FunctionID(id))
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

                UnmarshalledValue {
                    byte_count: offset,
                    value: DynValue::Array(Box::new(ArrayValue {
                        el_ty: (**element).clone(),
                        elements,
                    })),
                }
            },

            // these need field offset/tag type info from the ffi cache so marshal/unmarshla should
            // be members
            Type::Struct(struct_id) => {
                let field_tys = self
                    .struct_field_types
                    .get(struct_id)
                    .ok_or_else(|| MarshalError::UnsupportedType(ty.clone()))?;

                let mut offset = 0;
                let mut fields = Vec::new();

                for field_ty in field_tys {
                    let field_val = self.unmarshal(&in_bytes[offset..], field_ty)?;
                    offset += field_val.byte_count;
                    fields.push(field_val.value);
                }

                UnmarshalledValue {
                    value: DynValue::Structure(Box::new(StructValue {
                        id: *struct_id,
                        fields,
                    })),
                    byte_count: offset,
                }
            },

            Type::Variant(variant_id) => {
                let variant_val = self.unmarshal_variant(*variant_id, in_bytes)?;
                variant_val.map(Box::new).map(DynValue::Variant)
            },

            _ => {
                return Err(MarshalError::UnsupportedType(ty.clone()));
            },
        };

        Ok(dyn_val)
    }

    pub fn stack_alloc_size(&self, instructions: &[Instruction]) -> MarshalResult<usize> {
        let mut local_sizes = Vec::new();
        for instruction in instructions {
            if let Instruction::LocalAlloc(id, ty) = instruction {
                while local_sizes.len() <= id.0 {
                    local_sizes.push(0);
                }

                let ty_size = self.get_ty(ty)?.size();
                let local_size = &mut local_sizes[id.0];
                *local_size = max(ty_size, *local_size);
            }
        }

        let size_sum = local_sizes.into_iter().sum();
        Ok(size_sum)
    }

    fn marshal_variant(
        &self,
        variant_val: &VariantValue,
        out_bytes: &mut [u8],
    ) -> MarshalResult<usize> {
        let case_tys = self
            .variant_case_types
            .get(&variant_val.id)
            .ok_or_else(|| MarshalError::UnsupportedType(Type::Variant(variant_val.id)))?;

        let tag = variant_val
            .tag
            .as_i32()
            .ok_or_else(|| MarshalError::InvalidData)?;

        let case_ty = case_tys
            .get(tag as usize)
            .ok_or_else(|| MarshalError::InvalidData)?;

        // we still need to refer to the type size, because we always always marshal/unmarshal
        // the entire size of the variant regardless of which case is active
        let variant_size = self.get_ty(&Type::Variant(variant_val.id))?.size();

        let tag_size = self.marshal(&variant_val.tag, out_bytes)?;

        // don't marshal anything for data-less cases
        let data_size = match case_ty {
            None => 0,
            Some(case_ty) => {
                let data_size = self.marshal(&variant_val.data, &mut out_bytes[tag_size..])?;
                let ty_size = self.get_ty(case_ty)?.size();
                assert_eq!(ty_size, data_size);

                data_size
            },
        };

        let marshalled_size = tag_size + data_size;
        assert!(
            marshalled_size <= variant_size,
            "marshalled size {} <= type size {} for variant {}",
            marshalled_size,
            variant_size,
            Type::Variant(variant_val.id)
        );

        Ok(variant_size)
    }

    fn unmarshal_variant(
        &self,
        variant_id: StructID,
        in_bytes: &[u8],
    ) -> MarshalResult<UnmarshalledValue<VariantValue>> {
        let tag_val = self.unmarshal(in_bytes, &Type::I32)?;
        let tag = tag_val.value.as_i32().unwrap();

        let case_index = cast::usize(tag).map_err(|_| MarshalError::VariantTagOutOfRange {
            variant_id,
            tag: tag_val.value.clone(),
        })?;

        let variant_size = self.get_ty(&Type::Variant(variant_id))?.size();

        let case_tys = self
            .variant_case_types
            .get(&variant_id)
            .ok_or_else(|| MarshalError::UnsupportedType(Type::Variant(variant_id)))?;

        let case_ty = case_tys
            .get(case_index)
            .ok_or_else(|| MarshalError::VariantTagOutOfRange {
                variant_id,
                tag: tag_val.value.clone(),
            })?
            .as_ref();

        let data_val = match case_ty {
            Some(case_ty) => self.unmarshal(&in_bytes[tag_val.byte_count..], case_ty)?,
            None => UnmarshalledValue {
                value: DynValue::Pointer(Pointer::null(Type::Nothing)),
                byte_count: 0,
            },
        };

        assert!(tag_val.byte_count + data_val.byte_count <= variant_size);

        Ok(UnmarshalledValue {
            byte_count: variant_size,
            value: VariantValue {
                id: variant_id,
                tag: Box::new(DynValue::I32(tag as i32)),
                data: Box::new(data_val.value),
            },
        })
    }

    fn marshal_rc(&self, rc_val: &RcValue, out_bytes: &mut [u8]) -> MarshalResult<usize> {
        let mut offset = self.marshal_ptr(&rc_val.resource_ptr, out_bytes)?;

        let ref_count_bytes = rc_val.ref_count.to_ne_bytes();
        offset += marshal_bytes(&ref_count_bytes, &mut out_bytes[offset..]);

        let struct_id_bytes = rc_val.struct_id.0.to_ne_bytes();
        offset += marshal_bytes(&struct_id_bytes, &mut out_bytes[offset..]);

        Ok(offset)
    }

    fn unmarshal_rc(&self, in_bytes: &[u8]) -> MarshalResult<UnmarshalledValue<RcValue>> {
        let (resource_ptr, mut offset) = self.unmarshal_ptr(Type::Nothing, in_bytes)?;

        let ref_count_bytes = unmarshal_bytes(&in_bytes[offset..])?;
        offset += ref_count_bytes.len();

        let ref_count = usize::from_ne_bytes(ref_count_bytes);

        let struct_id_bytes = unmarshal_bytes(&in_bytes[offset..])?;
        offset += struct_id_bytes.len();

        let struct_id = StructID(usize::from_ne_bytes(struct_id_bytes));
        let resource_ptr = resource_ptr.reinterpret(Type::Struct(struct_id));

        Ok(UnmarshalledValue {
            value: RcValue {
                resource_ptr,
                struct_id,
                ref_count,
            },
            byte_count: offset,
        })
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

    match in_bytes[0..COUNT].try_into() {
        Ok(bytes) => Ok(bytes),
        Err(..) => Err(MarshalError::InvalidData),
    }
}

fn unmarshal_from_ne_bytes<T, FUn, const COUNT: usize>(
    in_bytes: &[u8],
    f_un: FUn,
) -> MarshalResult<UnmarshalledValue<T>>
where
    FUn: Fn([u8; COUNT]) -> T,
{
    let bytes = unmarshal_bytes(in_bytes)?;
    let value = f_un(bytes);

    Ok(UnmarshalledValue {
        value,
        byte_count: bytes.len(),
    })
}
