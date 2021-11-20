use crate::value_cell::ValueCell;
use crate::{ExecResult, HeapAddress, Interpreter};
use pas_ir::metadata::FieldID;
use pas_ir::{LocalID};
use std::borrow::Cow;
use std::cmp::Ordering;
use std::fmt;
use std::ops::{Add, Sub};
use crate::heap::native_heap::NativePointer;

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Pointer {
    Null,
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
    Native(NativePointer),
}

impl Pointer {
    pub fn as_heap_addr(&self) -> Option<HeapAddress> {
        match self {
            Pointer::Heap(addr) => Some(*addr),
            _ => None,
        }
    }

    pub fn deref_ptr<'a>(&self, state: &'a Interpreter) -> ExecResult<Cow<'a, ValueCell>> {
        state.load_indirect(self)
    }

    fn kind(&self) -> PointerKind {
        match self {
            Pointer::Null => PointerKind::Null,
            Pointer::Heap(_) => PointerKind::Heap,
            Pointer::Local { .. } => PointerKind::Local,
            Pointer::IntoArray { .. } => PointerKind::IntoArray,
            Pointer::IntoStruct { .. } => PointerKind::IntoStruct,
            Pointer::VariantTag { .. } => PointerKind::VariantTag,
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
            (Pointer::Null, Pointer::Null) => Ordering::Equal,

            (Pointer::Heap(a), Pointer::Heap(b)) => a.0.cmp(&b.0),
            (
                Pointer::Local {
                    frame: a_frame,
                    id: a_id,
                },
                Pointer::Local {
                    frame: b_frame,
                    id: b_id,
                },
            ) => a_frame.cmp(b_frame).then_with(|| a_id.cmp(b_id)),

            (
                Pointer::IntoArray {
                    array: arr_a,
                    offset: off_a,
                },
                Pointer::IntoArray {
                    array: arr_b,
                    offset: off_b,
                },
            ) => arr_a.cmp(&arr_b).then_with(|| off_a.cmp(off_b)),

            (a, b) => a.kind().cmp(&b.kind()),
        }
    }
}

impl Add<isize> for Pointer {
    type Output = Self;

    fn add(self, rhs: isize) -> Self {
        match self {
            Pointer::Null => Pointer::Null,
            Pointer::Local { frame, id } => Pointer::Local {
                frame,
                id: LocalID((id.0 as isize + rhs) as usize),
            },
            Pointer::Heap(HeapAddress(addr)) => {
                Pointer::Heap(HeapAddress((addr as isize + rhs) as usize))
            }
            Pointer::IntoArray { array, offset } => Pointer::IntoArray {
                array,
                offset: (offset as isize + rhs) as usize,
            },
            Pointer::VariantData { .. }
            | Pointer::VariantTag { .. }
            | Pointer::IntoStruct { .. } => {
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
            Pointer::Null => Pointer::Null,
            Pointer::Local { frame, id } => Pointer::Local {
                frame,
                id: LocalID((id.0 as isize - rhs) as usize),
            },
            Pointer::Heap(HeapAddress(addr)) => {
                Pointer::Heap(HeapAddress((addr as isize - rhs) as usize))
            }
            Pointer::IntoArray { array, offset } => Pointer::IntoArray {
                array,
                offset: (offset as isize - rhs) as usize,
            },
            Pointer::VariantData { .. }
            | Pointer::VariantTag { .. }
            | Pointer::IntoStruct { .. } => {
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
            Pointer::Null => write!(f, "NULL"),
            Pointer::Heap(heap_addr) => write!(f, "{}", heap_addr),
            Pointer::Local { frame, id } => write!(f, "{}/{}", frame, id ),
            Pointer::IntoArray { array, offset } => write!(f, "@({}^ [{}])", array, offset),
            Pointer::IntoStruct { structure, field } => write!(f, "@({}^.{})", structure, field),
            Pointer::VariantTag { variant } => write!(f, "@({}^.tag)", variant),
            Pointer::VariantData { variant, tag } => write!(f, "@({}.{})", variant, tag),
            Pointer::Native(native_ptr) => write!(f, "{}", native_ptr),
        }
    }
}

#[derive(Copy, Clone, Eq, Ord, PartialOrd, PartialEq, Debug, Hash)]
enum PointerKind {
    Null,
    Local,
    Heap,
    IntoArray,
    VariantData,
    VariantTag,
    IntoStruct,
    Native,
}
