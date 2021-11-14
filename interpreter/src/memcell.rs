use std::fmt;
use std::ops::{Index, IndexMut};
use std::rc::Rc;
use pas_ir::metadata::{FieldID, StructID};
use pas_ir::Type;
use crate::func::Function;
use crate::HeapAddress;
use crate::ptr::Pointer;

#[derive(Debug, Clone)]
pub struct StructCell {
    pub id: StructID,
    pub fields: Vec<MemCell>,
}

impl Index<FieldID> for StructCell {
    type Output = MemCell;

    fn index(&self, index: FieldID) -> &MemCell {
        &self.fields[index.0]
    }
}

impl IndexMut<FieldID> for StructCell {
    fn index_mut(&mut self, index: FieldID) -> &mut MemCell {
        &mut self.fields[index.0]
    }
}

impl PartialEq<Self> for StructCell {
    fn eq(&self, other: &Self) -> bool {
        if self.id != other.id || self.fields.len() != other.fields.len() {
            return false;
        }

        self.fields
            .iter()
            .zip(other.fields.iter())
            .all(|(a, b)| match a.try_eq(b) {
                Some(eq) => eq,
                None => panic!("structs can only contain comparable fields"),
            })
    }
}

#[derive(Debug, Clone)]
pub struct VariantCell {
    pub id: StructID,
    pub tag: Box<MemCell>,
    pub data: Box<MemCell>,
}

#[derive(Debug, Clone)]
pub struct RcCell {
    pub resource_addr: HeapAddress,
    pub ref_count: usize,
    pub struct_id: StructID,
}

#[derive(Debug, Clone)]
pub struct ArrayCell {
    pub el_ty: Type,
    pub elements: Vec<MemCell>,
}

impl ArrayCell {
    pub fn try_eq(&self, other: &Self) -> Option<bool> {
        if self.elements.len() == other.elements.len() {
            let mut all_same = true;
            for (mine, theirs) in self.elements.iter().zip(other.elements.iter()) {
                all_same &= mine.try_eq(theirs)?;
            }
            Some(all_same)
        } else {
            None
        }
    }
}

#[derive(Debug, Clone)]
pub enum MemCell {
    Bool(bool),
    U8(u8),
    I32(i32),
    F32(f32),
    RcCell(Box<RcCell>),
    Function(Rc<Function>),
    Structure(Box<StructCell>),
    Variant(Box<VariantCell>),
    Pointer(Pointer),
    Array(Box<ArrayCell>),
}

impl MemCell {
    pub fn try_eq(&self, other: &Self) -> Option<bool> {
        match (self, other) {
            (MemCell::Bool(a), MemCell::Bool(b)) => Some(a == b),
            (MemCell::U8(a), MemCell::U8(b)) => Some(a == b),
            (MemCell::I32(a), MemCell::I32(b)) => Some(a == b),
            (MemCell::F32(a), MemCell::F32(b)) => Some(a == b),
            (MemCell::Pointer(a), MemCell::Pointer(b)) => Some(a == b),
            (MemCell::Structure(a), MemCell::Structure(b)) => Some(a == b),
            (MemCell::Array(a), MemCell::Array(b)) => a.try_eq(b),
            _ => None,
        }
    }

    pub fn try_not(&self) -> Option<bool> {
        match self {
            MemCell::Bool(b) => Some(!*b),
            _ => None,
        }
    }

    pub fn try_add(&self, other: &Self) -> Option<Self> {
        match (self, other) {
            (MemCell::I32(a), MemCell::I32(b)) => Some(MemCell::I32(a + b)),
            (MemCell::U8(a), MemCell::U8(b)) => Some(MemCell::U8(a + b)),
            (MemCell::F32(a), MemCell::F32(b)) => Some(MemCell::F32(a + b)),

            (MemCell::Pointer(ptr), MemCell::I32(offset)) => {
                Some(MemCell::Pointer(ptr.clone() + *offset as isize))
            }

            (MemCell::Pointer(Pointer::Heap(ref a)), MemCell::Pointer(Pointer::Heap(ref b))) => {
                Some(MemCell::Pointer(Pointer::Heap(HeapAddress(a.0 + b.0))))
            }

            _ => None,
        }
    }

    pub fn try_sub(&self, other: &Self) -> Option<Self> {
        match (self, other) {
            (MemCell::I32(a), MemCell::I32(b)) => Some(MemCell::I32(a - b)),
            (MemCell::U8(a), MemCell::U8(b)) => Some(MemCell::U8(a - b)),
            (MemCell::F32(a), MemCell::F32(b)) => Some(MemCell::F32(a - b)),

            (MemCell::Pointer(ptr), MemCell::I32(offset)) => {
                Some(MemCell::Pointer(ptr.clone() - *offset as isize))
            }

            (MemCell::Pointer(Pointer::Heap(ref a)), MemCell::Pointer(Pointer::Heap(ref b))) => {
                Some(MemCell::Pointer(Pointer::Heap(HeapAddress(a.0 - b.0))))
            }

            _ => None,
        }
    }

    pub fn try_mul(&self, other: &Self) -> Option<Self> {
        match (self, other) {
            (MemCell::I32(a), MemCell::I32(b)) => Some(MemCell::I32(a * b)),
            (MemCell::U8(a), MemCell::U8(b)) => Some(MemCell::U8(a * b)),
            (MemCell::F32(a), MemCell::F32(b)) => Some(MemCell::F32(a * b)),

            _ => None,
        }
    }

    pub fn try_idiv(&self, other: &Self) -> Option<Self> {
        match (self, other) {
            (MemCell::I32(a), MemCell::I32(b)) => Some(MemCell::I32(a / b)),
            (MemCell::U8(a), MemCell::U8(b)) => Some(MemCell::U8(a / b)),
            (MemCell::F32(a), MemCell::F32(b)) => Some(MemCell::F32(a / b)),

            _ => None,
        }
    }

    pub fn try_shl(&self, other: &Self) -> Option<Self> {
        match (self, other) {
            (MemCell::I32(a), MemCell::I32(b)) => Some(MemCell::I32(a << b)),
            (MemCell::U8(a), MemCell::U8(b)) => Some(MemCell::U8(a << b)),

            _ => None,
        }
    }

    pub fn try_shr(&self, other: &Self) -> Option<Self> {
        match (self, other) {
            (MemCell::I32(a), MemCell::I32(b)) => Some(MemCell::I32(a >> b)),
            (MemCell::U8(a), MemCell::U8(b)) => Some(MemCell::U8(a >> b)),

            _ => None,
        }
    }

    pub fn try_gt(&self, other: &Self) -> Option<bool> {
        match (self, other) {
            (MemCell::I32(a), MemCell::I32(b)) => Some(a > b),
            (MemCell::U8(a), MemCell::U8(b)) => Some(a > b),
            (MemCell::F32(a), MemCell::F32(b)) => Some(a > b),
            (MemCell::Bool(a), MemCell::Bool(b)) => Some(a > b),
            (MemCell::Pointer(a), MemCell::Pointer(b)) => Some(a > b),

            _ => None,
        }
    }

    pub fn as_function(&self) -> Option<&Rc<Function>> {
        match self {
            MemCell::Function(f) => Some(f),
            _ => None,
        }
    }

    pub fn as_struct_mut(&mut self, struct_id: StructID) -> Option<&mut StructCell> {
        match self {
            MemCell::Structure(struct_cell) if struct_id == struct_cell.id => Some(struct_cell),
            _ => None,
        }
    }

    pub fn as_struct(&self, struct_id: StructID) -> Option<&StructCell> {
        match self {
            MemCell::Structure(struct_cell) if struct_id == struct_cell.id => Some(struct_cell),
            _ => None,
        }
    }

    pub fn as_array(&self, el_ty: &Type) -> Option<&[MemCell]> {
        match self {
            MemCell::Array(arr) if arr.el_ty == *el_ty => Some(&arr.elements),
            _ => None,
        }
    }

    pub fn as_variant(&self, struct_id: StructID) -> Option<&VariantCell> {
        match self {
            MemCell::Variant(var_cell) if struct_id == var_cell.id => Some(var_cell),
            _ => None,
        }
    }

    pub fn as_rc_mut(&mut self) -> Option<&mut RcCell> {
        match self {
            MemCell::RcCell(rc) => Some(rc),
            _ => None,
        }
    }

    pub fn as_rc(&self) -> Option<&RcCell> {
        match self {
            MemCell::RcCell(rc) => Some(rc),
            _ => None,
        }
    }

    pub fn as_bool(&self) -> Option<bool> {
        match self {
            MemCell::Bool(b) => Some(*b),
            _ => None,
        }
    }

    pub fn as_u8(&self) -> Option<u8> {
        match self {
            MemCell::U8(x) => Some(*x),
            _ => None,
        }
    }

    pub fn as_i32(&self) -> Option<i32> {
        match self {
            MemCell::I32(i) => Some(*i),
            _ => None,
        }
    }

    pub fn as_pointer(&self) -> Option<&Pointer> {
        match self {
            MemCell::Pointer(ptr) => Some(ptr),
            _ => None,
        }
    }

    pub fn kind(&self) -> MemCellKind {
        match self {
            MemCell::Bool(..) => MemCellKind::Bool,
            MemCell::U8(..) => MemCellKind::U8,
            MemCell::I32(..) => MemCellKind::I32,
            MemCell::F32(..) => MemCellKind::F32,
            MemCell::RcCell(..) => MemCellKind::Rc,
            MemCell::Function(..) => MemCellKind::Function,
            MemCell::Structure(..) => MemCellKind::Structure,
            MemCell::Variant(..) => MemCellKind::Variant,
            MemCell::Pointer(..) => MemCellKind::Pointer,
            MemCell::Array(..) => MemCellKind::Array,
        }
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub enum MemCellKind {
    Bool,
    U8,
    I32,
    F32,
    Rc,
    Function,
    Structure,
    Variant,
    Pointer,
    Array,
}

impl fmt::Display for MemCellKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", match self {
            MemCellKind::Bool => "Bool",
            MemCellKind::U8 => "U8",
            MemCellKind::I32 => "I32",
            MemCellKind::F32 => "F32",
            MemCellKind::Rc => "Rc",
            MemCellKind::Function => "Function",
            MemCellKind::Structure => "Structure",
            MemCellKind::Variant => "Variant",
            MemCellKind::Pointer => "Pointer",
            MemCellKind::Array => "Array",
        })
    }
}