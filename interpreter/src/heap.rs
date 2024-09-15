use std::collections::BTreeMap;
use std::fmt;
use std::rc::Rc;
use pas_ir::Type;
use crate::{DynValue, marshal::{Marshaller, MarshalError}, Pointer};

#[derive(Clone, Debug)]
pub enum NativeHeapError {
    MarshallingError(MarshalError),
    NullPointerDeref,
    BadFree(Pointer),
    ZeroSizedAllocation {
        ty: Type,
        count: usize,
    },
}

impl fmt::Display for NativeHeapError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            NativeHeapError::MarshallingError(err) => write!(f, "{}", err),
            NativeHeapError::NullPointerDeref => write!(f, "null pointer dereference"),
            NativeHeapError::BadFree(ptr) => write!(f, "freeing value at {} which wasn't allocated on this heap", ptr),
            NativeHeapError::ZeroSizedAllocation { ty, count } => write!(f, "zero-sized allocation of {} elements of type {}", count, ty),
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
    
    // usage stats for trace output
    alloc_count: usize,
    free_count: usize,
    peak_alloc: usize,
}

impl NativeHeap {
    pub fn new(marshaller: Rc<Marshaller>, trace_allocs: bool) -> Self {
        Self {
            marshaller,
            trace_allocs,
            allocs: BTreeMap::new(),
            
            alloc_count: 0,
            free_count: 0,
            peak_alloc: 0,
        }
    }

    pub fn set_marshaller(&mut self, marshaller: Rc<Marshaller>) {
        self.marshaller = marshaller;
    }

    pub fn alloc(&mut self, ty: Type, count: usize) -> NativeHeapResult<Pointer> {
        let ty_size = self.marshaller.get_ty(&ty)?.size();
        if ty_size == 0 || count == 0 {
            return Err(NativeHeapError::ZeroSizedAllocation { ty, count });
        }

        let total_len = ty_size * count;

        let alloc_mem = vec![0; total_len];
        let addr = alloc_mem.as_ptr() as usize;

        self.allocs.insert(addr, alloc_mem.into_boxed_slice());

        if self.trace_allocs {
            eprintln!("NativeHeap: alloc {} bytes @ {}", total_len, addr);
            self.alloc_count += 1;
            
            self.peak_alloc = usize::max(self.peak_alloc, self.allocs.iter()
                .map(|(_, mem)| mem.len())
                .sum())
        }
        

        Ok(Pointer { addr, ty })
    }

    pub fn free(&mut self, ptr: &Pointer) -> NativeHeapResult<()> {
        if self.trace_allocs {
            eprintln!("NativeHeap: free @ {}", ptr);
            self.free_count += 1;
        }

        if self.allocs.remove(&ptr.addr).is_none() {
            return Err(NativeHeapError::BadFree(ptr.clone()));
        }

        Ok(())
    }

    pub fn load(&self, addr: &Pointer) -> NativeHeapResult<DynValue> {
        if addr.addr == 0 {
            return Err(NativeHeapError::NullPointerDeref);
        }

        let val = self.marshaller.unmarshal_from_ptr(addr)?;

        Ok(val)
    }

    pub fn store(&mut self, addr: &Pointer, val: DynValue) -> NativeHeapResult<()> {
        if addr.addr == 0 {
            return Err(NativeHeapError::NullPointerDeref);
        }

        self.marshaller.marshal_into(&val, addr)?;

        Ok(())
    }
    
    pub fn print_trace_stats(&self) {
        eprintln!("NativeHeap: alloc count = {}", self.alloc_count);
        eprintln!("NativeHeap: free count = {}", self.free_count);
        eprintln!("NativeHeap: peak alloc size = {}", self.peak_alloc);
    }
}
