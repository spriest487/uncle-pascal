use crate::value_cell::ValueCell;
use crate::{ExecResult, Interpreter};
use pas_ir::metadata::FieldID;
use std::borrow::Cow;
use std::cmp::Ordering;
use std::fmt;
use std::ops::{Add, Sub};
use crate::heap::NativePointer;

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Pointer {
    Null,
    IntoArray {
        array: Box<Pointer>,
        offset: usize,
    },
    IntoStruct {
        structure: Box<Pointer>,
        field: FieldID,
    },
    VariantData {
        variant: Box<Pointer>,
        tag: usize,
    },
    Native(NativePointer),
}

impl Pointer {
    pub fn deref_ptr<'a>(&self, state: &'a Interpreter) -> ExecResult<Cow<'a, ValueCell>> {
        state.load_indirect(self)
    }

    fn kind(&self) -> PointerKind {
        match self {
            Pointer::Null => PointerKind::Null,
            Pointer::IntoArray { .. } => PointerKind::IntoArray,
            Pointer::IntoStruct { .. } => PointerKind::IntoStruct,
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
            Pointer::IntoArray { array, offset } => Pointer::IntoArray {
                array,
                offset: (offset as isize + rhs) as usize,
            },
            Pointer::VariantData { .. }
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
            Pointer::IntoArray { array, offset } => Pointer::IntoArray {
                array,
                offset: (offset as isize - rhs) as usize,
            },
            Pointer::VariantData { .. }
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
            Pointer::IntoArray { array, offset } => write!(f, "@({}^ [{}])", array, offset),
            Pointer::IntoStruct { structure, field } => write!(f, "@({}^.{})", structure, field),
            Pointer::VariantData { variant, tag } => write!(f, "@({}.{})", variant, tag),
            Pointer::Native(native_ptr) => write!(f, "{}", native_ptr),
        }
    }
}

#[derive(Copy, Clone, Eq, Ord, PartialOrd, PartialEq, Debug, Hash)]
enum PointerKind {
    Null,
    IntoArray,
    VariantData,
    IntoStruct,
    Native,
}
