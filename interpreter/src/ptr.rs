use std::ops::{Add, Sub};
use pas_ir::LocalID;
use pas_ir::metadata::FieldID;
use crate::{HeapAddress, Interpreter};
use crate::memcell::MemCell;

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Pointer {
    Null,
    Uninit,
    Heap(HeapAddress),
    Local {
        frame: usize,
        id: LocalID,
    },
    IntoArray {
        array: Box<Pointer>,
        offset: usize,
    },
    IntoStruct {
        structure: Box<Pointer>,
        field: FieldID,
    },
    VariantTag {
        variant: Box<Pointer>,
    },
    VariantData {
        variant: Box<Pointer>,
        tag: usize,
    },
}

impl Pointer {
    pub fn as_heap_addr(&self) -> Option<HeapAddress> {
        match self {
            Pointer::Heap(addr) => Some(*addr),
            _ => None,
        }
    }

    pub fn deref_ptr<'a>(&self, state: &'a Interpreter) -> &'a MemCell {
        state.deref_ptr(self)
    }
}

impl Add<usize> for Pointer {
    type Output = Self;

    fn add(self, rhs: usize) -> Self {
        match self {
            Pointer::Null => Pointer::Null,
            Pointer::Uninit => Pointer::Uninit,
            Pointer::Local { frame, id } => Pointer::Local {
                frame,
                id: LocalID(id.0 + rhs),
            },
            Pointer::Heap(HeapAddress(addr)) => Pointer::Heap(HeapAddress(addr + rhs)),
            Pointer::IntoArray { array, offset } => Pointer::IntoArray {
                array,
                offset: offset + rhs,
            },
            Pointer::VariantData { .. }
            | Pointer::VariantTag { .. }
            | Pointer::IntoStruct { .. } => {
                panic!("pointer arithmetic on struct pointers is illegal")
            }
        }
    }
}

impl Sub<usize> for Pointer {
    type Output = Self;

    fn sub(self, rhs: usize) -> Self {
        match self {
            Pointer::Null => Pointer::Null,
            Pointer::Uninit => Pointer::Uninit,
            Pointer::Local { frame, id } => Pointer::Local {
                frame,
                id: LocalID(id.0 - rhs),
            },
            Pointer::Heap(HeapAddress(addr)) => Pointer::Heap(HeapAddress(addr - rhs)),
            Pointer::IntoArray { array, offset } => Pointer::IntoArray {
                array,
                offset: offset - rhs,
            },
            Pointer::VariantData { .. }
            | Pointer::VariantTag { .. }
            | Pointer::IntoStruct { .. } => {
                panic!("pointer arithmetic on struct pointers is illegal")
            }
        }
    }
}
