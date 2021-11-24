use std::collections::BTreeMap;
use std::fmt;
use std::mem::size_of;
use std::rc::Rc;
use pas_ir::Type;
use crate::{
    ValueCell,
    marshal::{Marshaller, MarshalError}
};

#[derive(Clone, Debug)]
pub enum NativeHeapError {
    MarshallingError(MarshalError),
    NullPointerDeref,
    BadFree(NativePointer),
}

impl fmt::Display for NativeHeapError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            NativeHeapError::MarshallingError(err) => write!(f, "{}", err),
            NativeHeapError::NullPointerDeref => write!(f, "null pointer dereference"),
            NativeHeapError::BadFree(ptr) => write!(f, "freeing value at {} which wasn't allocated on this heap", ptr),
        }
    }
}

impl From<MarshalError> for NativeHeapError {
    fn from(err: MarshalError) -> Self {
        NativeHeapError::MarshallingError(err)
    }
}

pub type NativeHeapResult<T> = Result<T, NativeHeapError>;

#[derive(Debug)]
pub struct NativeHeap {
    marshaller: Rc<Marshaller>,

    allocs: BTreeMap<usize, Box<[u8]>>,

    trace_allocs: bool,
}

impl NativeHeap {
    pub fn new(ffi_cache: Rc<Marshaller>, trace_allocs: bool) -> Self {
        Self {
            marshaller: ffi_cache,
            trace_allocs,
            allocs: BTreeMap::new(),
        }
    }

    pub fn set_ffi_cache(&mut self, ffi_cache: Rc<Marshaller>) {
        self.marshaller = ffi_cache;
    }

    pub fn alloc(&mut self, ty: Type, count: usize) -> NativeHeapResult<NativePointer> {
        if count == 0 {
            return Ok(NativePointer {
                ty: Type::Nothing,
                addr: 0,
            });
        }

        let total_len = self.marshaller.get_ty(&ty)?.size() * count;

        let alloc_mem = vec![0; total_len];
        let addr = alloc_mem.as_ptr() as usize;

        self.allocs.insert(addr, alloc_mem.into_boxed_slice());

        if self.trace_allocs {
            eprintln!("NativeHeap: alloc {} bytes @ {}", total_len, addr);
        }

        Ok(NativePointer { addr, ty })
    }

    pub fn free(&mut self, ptr: &NativePointer) -> NativeHeapResult<()> {
        if self.trace_allocs {
            eprintln!("NativeHeap: free @ {}", ptr);
        }

        if self.allocs.remove(&ptr.addr).is_none() {
            return Err(NativeHeapError::BadFree(ptr.clone()));
        }

        Ok(())
    }

    pub fn load(&self, addr: &NativePointer) -> NativeHeapResult<ValueCell> {
        if addr.addr == 0 {
            return Err(NativeHeapError::NullPointerDeref);
        }

        let val = self.marshaller.unmarshal_from_ptr(addr)?;

        Ok(val)
    }

    pub fn store(&mut self, addr: &NativePointer, val: ValueCell) -> NativeHeapResult<()> {
        if addr.addr == 0 {
            return Err(NativeHeapError::NullPointerDeref);
        }

        self.marshaller.marshal_into(&val, addr)?;

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