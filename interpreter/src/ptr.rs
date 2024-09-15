use crate::dyn_value::DynValue;
use crate::{ExecResult, Interpreter};
use std::borrow::Cow;
use std::cmp::Ordering;
use std::fmt;
use std::mem::size_of;
use intermediate::metadata::Metadata;
use intermediate::Type;

/// pointer to native memory that is marshalled to/from value cells when accessed
#[derive(Debug, Clone, Eq)]
pub struct Pointer {
    pub addr: usize,
    pub ty: Type,
}

impl fmt::Display for Pointer {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "0x{:0width$x} ({})", self.addr, self.ty, width = (size_of::<usize>() * 2))
    }
}

impl Pointer {
    pub fn null(ty: Type) -> Self {
        Self {
            addr: 0,
            ty
        }
    }

    pub fn is_null(&self) -> bool {
        self.addr == 0
    }

    pub fn deref_ptr<'a>(&self, state: &'a Interpreter) -> ExecResult<Cow<'a, DynValue>> {
        state.load_indirect(self)
    }

    pub fn reinterpret(&self, ty: Type) -> Self {
        Self {
            addr: self.addr,
            ty,
        }
    }

    pub fn addr_add(&self, rhs: usize) -> Self {
        Self { ty: self.ty.clone(), addr: self.addr + rhs }
    }

    pub fn addr_sub(&self, rhs: usize) -> Self {
        Self { ty: self.ty.clone(), addr: self.addr - rhs }
    }

    pub fn addr_mul(&self, rhs: usize) -> Self {
        Self { ty: self.ty.clone(), addr: self.addr * rhs }
    }

    pub fn addr_div(&self, rhs: usize) -> Self {
        Self { ty: self.ty.clone(), addr: self.addr / rhs }
    }

    pub fn addr_shl(&self, rhs: usize) -> Self {
        Self { ty: self.ty.clone(), addr: self.addr << rhs }
    }

    pub fn addr_shr(&self, rhs: usize) -> Self {
        Self { ty: self.ty.clone(), addr: self.addr >> rhs }
    }

    pub fn to_pretty_string(&self, metadata: &Metadata) -> String {
        let ty_pretty_name = metadata.pretty_ty_name(&self.ty);

        format!("0x{:0width$x} ({})", self.addr, ty_pretty_name, width = (size_of::<usize>() * 2))
    }
}

impl PartialOrd for Pointer {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Pointer {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self, other) {
            (a, b) => a.addr.cmp(&b.addr),
        }
    }
}

impl PartialEq for Pointer {
    fn eq(&self, other: &Self) -> bool {
        self.addr == other.addr 
    }
}
