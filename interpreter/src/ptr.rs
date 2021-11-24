use crate::value_cell::ValueCell;
use crate::{ExecResult, Interpreter};
use std::borrow::Cow;
use std::cmp::Ordering;
use std::fmt;
use std::ops::{Add, Sub};
use pas_ir::Type;
use crate::heap::NativePointer;

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Pointer {
    VariantData {
        variant: Box<Pointer>,
        tag: usize,
    },
    Native(NativePointer),
}

impl Pointer {
    pub fn null(ty: Type) -> Self {
        Pointer::Native(NativePointer {
            addr: 0,
            ty
        })
    }

    pub fn is_null(&self) -> bool {
        match self {
            Pointer::Native(ptr) => ptr.addr == 0,
            _ => false,
        }
    }

    pub fn deref_ptr<'a>(&self, state: &'a Interpreter) -> ExecResult<Cow<'a, ValueCell>> {
        state.load_indirect(self)
    }

    fn kind(&self) -> PointerKind {
        match self {
            Pointer::VariantData { .. } => PointerKind::VariantData,
            Pointer::Native { .. } => PointerKind::Native,
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
            (a, b) => a.kind().cmp(&b.kind()),
        }
    }
}

impl Add<isize> for Pointer {
    type Output = Self;

    fn add(self, rhs: isize) -> Self {
        match self {
            Pointer::VariantData { .. } => {
                panic!("pointer arithmetic on struct pointers is illegal")
            }
            Pointer::Native(NativePointer { addr, ty }) => Pointer::Native(NativePointer {
                addr: (addr as isize + rhs) as usize,
                ty,
            }),
        }
    }
}

impl Sub<isize> for Pointer {
    type Output = Self;

    fn sub(self, rhs: isize) -> Self {
        match self {
            Pointer::VariantData { .. } => {
                panic!("pointer arithmetic on struct pointers is illegal")
            }
            Pointer::Native(NativePointer { addr, ty }) => Pointer::Native(NativePointer {
                addr: (addr as isize - rhs) as usize,
                ty,
            }),
        }
    }
}

impl fmt::Display for Pointer {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Pointer::VariantData { variant, tag } => write!(f, "@({}.{})", variant, tag),
            Pointer::Native(native_ptr) => write!(f, "{}", native_ptr),
        }
    }
}

#[derive(Copy, Clone, Eq, Ord, PartialOrd, PartialEq, Debug, Hash)]
enum PointerKind {
    VariantData,
    Native,
}
