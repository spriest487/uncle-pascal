use std::collections::BTreeMap;
use std::fmt;
use std::mem::size_of;
use std::ptr::{slice_from_raw_parts, slice_from_raw_parts_mut};
use std::rc::Rc;
use pas_ir::Type;
use crate::{FfiCache, ValueCell};
use crate::func::ffi::{marshal, get_marshal_ty, MarshalError, ForeignTypeExt, unmarshal};

#[derive(Clone, Debug)]
pub enum NativeHeapError {
    MarshallingError(MarshalError),
    Unallocated(NativePointer),
}

impl From<MarshalError> for NativeHeapError {
    fn from(err: MarshalError) -> Self {
        NativeHeapError::MarshallingError(err)
    }
}

pub type NativeHeapResult<T> = Result<T, NativeHeapError>;

#[derive(Debug)]
pub struct NativeHeap {
    ffi_cache: Rc<FfiCache>,

    allocs: BTreeMap<usize, Box<[u8]>>,

    trace_allocs: bool,
}

impl NativeHeap {
    pub fn new(ffi_cache: Rc<FfiCache>, trace_allocs: bool) -> Self {
        Self {
            ffi_cache,
            trace_allocs,
            allocs: BTreeMap::new(),
        }
    }

    pub fn set_ffi_cache(&mut self, ffi_cache: Rc<FfiCache>) {
        self.ffi_cache = ffi_cache;
    }

    pub fn alloc(&mut self, vals: Vec<ValueCell>) -> NativeHeapResult<NativePointer> {
        if vals.len() == 0 {
            return Ok(NativePointer {
                ty: Type::Nothing,
                addr: 0,
            });
        }

        let ty = get_marshal_ty(&vals[0])?;
        let total_len = self.ffi_cache.get_ty(&ty)?.size() * vals.len();

        let mut alloc_mem = vec![0; total_len];
        let out_pos = 0;
        for val in &vals {
            marshal(val, &mut alloc_mem[out_pos..])?;
        }

        let addr = alloc_mem.as_ptr() as usize;
        self.allocs.insert(addr, alloc_mem.into_boxed_slice());

        if self.trace_allocs {
            eprintln!("NativeHeap: alloc {} bytes @ {}", total_len, addr);
        }

        Ok(NativePointer { addr, ty })
    }

    pub fn free(&mut self, addr: NativePointer) -> NativeHeapResult<()> {
        if self.trace_allocs {
            eprintln!("NativeHeap: free @ {}", addr);
        }

        if self.allocs.remove(&addr.addr).is_none() {
            return Err(NativeHeapError::Unallocated(addr));
        }

        Ok(())
    }

    pub fn load(&self, addr: &NativePointer) -> NativeHeapResult<ValueCell> {
        if addr.addr == 0 {
            return Err(NativeHeapError::Unallocated(addr.clone()));
        }

        let val = unsafe {
            let marshal_ty = self.ffi_cache.get_ty(&addr.ty)?;
            assert_ne!(0, marshal_ty.size());

            let mem_slice = slice_from_raw_parts(addr.addr as *const u8, marshal_ty.size());
            unmarshal(&*mem_slice, &addr.ty)?
        };

        Ok(val)
    }

    pub fn store(&mut self, addr: &NativePointer, val: ValueCell) -> NativeHeapResult<()> {
        if addr.addr == 0 {
            return Err(NativeHeapError::Unallocated(addr.clone()));
        }

        unsafe {
            let marshal_ty = self.ffi_cache.get_ty(&addr.ty)?;
            assert_ne!(0, marshal_ty.size());

            let mem_slice = slice_from_raw_parts_mut(addr.addr as *mut u8, marshal_ty.size());
            marshal(&val, &mut *mem_slice)?;
        };

        Ok(())
    }
}

/// pointer to native memory that is marshalled to/from value cells when accessed
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct NativePointer {
    pub addr: usize,
    pub ty: Type,
}

impl fmt::Display for NativePointer {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "0x{:0width$x} ({})", self.addr, self.ty, width = (size_of::<usize>() * 2))
    }
}