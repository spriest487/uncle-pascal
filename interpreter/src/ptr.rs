use crate::value_cell::ValueCell;
use crate::{ExecResult, Interpreter};
use std::borrow::Cow;
use std::cmp::Ordering;
use std::fmt;
use std::mem::size_of;
use pas_ir::Type;

/// pointer to native memory that is marshalled to/from value cells when accessed
#[derive(Debug, Clone, Eq, PartialEq)]
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

    pub fn deref_ptr<'a>(&self, state: &'a Interpreter) -> ExecResult<Cow<'a, ValueCell>> {
        state.load_indirect(self)
    }

    pub fn reinterpret(&self, ty: Type) -> Self {
        Self {
            addr: self.addr,
            ty,
        }
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