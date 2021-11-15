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
    pub fields: Vec<ValueCell>,
}

impl Index<FieldID> for StructCell {
    type Output = ValueCell;

    fn index(&self, index: FieldID) -> &ValueCell {
        &self.fields[index.0]
    }
}

impl IndexMut<FieldID> for StructCell {
    fn index_mut(&mut self, index: FieldID) -> &mut ValueCell {
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
    pub tag: Box<ValueCell>,
    pub data: Box<ValueCell>,
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
    pub elements: Vec<ValueCell>,
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
pub enum ValueCell {
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

impl ValueCell {
    pub fn try_eq(&self, other: &Self) -> Option<bool> {
        match (self, other) {
            (ValueCell::Bool(a), ValueCell::Bool(b)) => Some(a == b),
            (ValueCell::U8(a), ValueCell::U8(b)) => Some(a == b),
            (ValueCell::I32(a), ValueCell::I32(b)) => Some(a == b),
            (ValueCell::F32(a), ValueCell::F32(b)) => Some(a == b),
            (ValueCell::Pointer(a), ValueCell::Pointer(b)) => Some(a == b),
            (ValueCell::Structure(a), ValueCell::Structure(b)) => Some(a == b),
            (ValueCell::Array(a), ValueCell::Array(b)) => a.try_eq(b),
            _ => None,
        }
    }

    pub fn try_not(&self) -> Option<bool> {
        match self {
            ValueCell::Bool(b) => Some(!*b),
            _ => None,
        }
    }

    pub fn try_add(&self, other: &Self) -> Option<Self> {
        match (self, other) {
            (ValueCell::I32(a), ValueCell::I32(b)) => Some(ValueCell::I32(a + b)),
            (ValueCell::U8(a), ValueCell::U8(b)) => Some(ValueCell::U8(a + b)),
            (ValueCell::F32(a), ValueCell::F32(b)) => Some(ValueCell::F32(a + b)),

            (ValueCell::Pointer(ptr), ValueCell::I32(offset)) => {
                Some(ValueCell::Pointer(ptr.clone() + *offset as isize))
            }

            (ValueCell::Pointer(Pointer::Heap(ref a)), ValueCell::Pointer(Pointer::Heap(ref b))) => {
                Some(ValueCell::Pointer(Pointer::Heap(HeapAddress(a.0 + b.0))))
            }

            _ => None,
        }
    }

    pub fn try_sub(&self, other: &Self) -> Option<Self> {
        match (self, other) {
            (ValueCell::I32(a), ValueCell::I32(b)) => Some(ValueCell::I32(a - b)),
            (ValueCell::U8(a), ValueCell::U8(b)) => Some(ValueCell::U8(a - b)),
            (ValueCell::F32(a), ValueCell::F32(b)) => Some(ValueCell::F32(a - b)),

            (ValueCell::Pointer(ptr), ValueCell::I32(offset)) => {
                Some(ValueCell::Pointer(ptr.clone() - *offset as isize))
            }

            (ValueCell::Pointer(Pointer::Heap(ref a)), ValueCell::Pointer(Pointer::Heap(ref b))) => {
                Some(ValueCell::Pointer(Pointer::Heap(HeapAddress(a.0 - b.0))))
            }

            _ => None,
        }
    }

    pub fn try_mul(&self, other: &Self) -> Option<Self> {
        match (self, other) {
            (ValueCell::I32(a), ValueCell::I32(b)) => Some(ValueCell::I32(a * b)),
            (ValueCell::U8(a), ValueCell::U8(b)) => Some(ValueCell::U8(a * b)),
            (ValueCell::F32(a), ValueCell::F32(b)) => Some(ValueCell::F32(a * b)),

            _ => None,
        }
    }

    pub fn try_idiv(&self, other: &Self) -> Option<Self> {
        match (self, other) {
            (ValueCell::I32(a), ValueCell::I32(b)) => Some(ValueCell::I32(a / b)),
            (ValueCell::U8(a), ValueCell::U8(b)) => Some(ValueCell::U8(a / b)),
            (ValueCell::F32(a), ValueCell::F32(b)) => Some(ValueCell::F32(a / b)),

            _ => None,
        }
    }

    pub fn try_shl(&self, other: &Self) -> Option<Self> {
        match (self, other) {
            (ValueCell::I32(a), ValueCell::I32(b)) => Some(ValueCell::I32(a << b)),
            (ValueCell::U8(a), ValueCell::U8(b)) => Some(ValueCell::U8(a << b)),

            _ => None,
        }
    }

    pub fn try_shr(&self, other: &Self) -> Option<Self> {
        match (self, other) {
            (ValueCell::I32(a), ValueCell::I32(b)) => Some(ValueCell::I32(a >> b)),
            (ValueCell::U8(a), ValueCell::U8(b)) => Some(ValueCell::U8(a >> b)),

            _ => None,
        }
    }

    pub fn try_gt(&self, other: &Self) -> Option<bool> {
        match (self, other) {
            (ValueCell::I32(a), ValueCell::I32(b)) => Some(a > b),
            (ValueCell::U8(a), ValueCell::U8(b)) => Some(a > b),
            (ValueCell::F32(a), ValueCell::F32(b)) => Some(a > b),
            (ValueCell::Bool(a), ValueCell::Bool(b)) => Some(a > b),
            (ValueCell::Pointer(a), ValueCell::Pointer(b)) => Some(a > b),

            _ => None,
        }
    }

    pub fn as_function(&self) -> Option<&Rc<Function>> {
        match self {
            ValueCell::Function(f) => Some(f),
            _ => None,
        }
    }

    pub fn as_struct_mut(&mut self, struct_id: StructID) -> Option<&mut StructCell> {
        match self {
            ValueCell::Structure(struct_cell) if struct_id == struct_cell.id => Some(struct_cell),
            _ => None,
        }
    }

    pub fn as_struct(&self, struct_id: StructID) -> Option<&StructCell> {
        match self {
            ValueCell::Structure(struct_cell) if struct_id == struct_cell.id => Some(struct_cell),
            _ => None,
        }
    }

    pub fn as_array(&self, el_ty: &Type) -> Option<&[ValueCell]> {
        match self {
            ValueCell::Array(arr) if arr.el_ty == *el_ty => Some(&arr.elements),
            _ => None,
        }
    }

    pub fn as_variant(&self, struct_id: StructID) -> Option<&VariantCell> {
        match self {
            ValueCell::Variant(var_cell) if struct_id == var_cell.id => Some(var_cell),
            _ => None,
        }
    }

    pub fn as_rc_mut(&mut self) -> Option<&mut RcCell> {
        match self {
            ValueCell::RcCell(rc) => Some(rc),
            _ => None,
        }
    }

    pub fn as_rc(&self) -> Option<&RcCell> {
        match self {
            ValueCell::RcCell(rc) => Some(rc),
            _ => None,
        }
    }

    pub fn as_bool(&self) -> Option<bool> {
        match self {
            ValueCell::Bool(b) => Some(*b),
            _ => None,
        }
    }

    pub fn as_u8(&self) -> Option<u8> {
        match self {
            ValueCell::U8(x) => Some(*x),
            _ => None,
        }
    }

    pub fn as_i32(&self) -> Option<i32> {
        match self {
            ValueCell::I32(i) => Some(*i),
            _ => None,
        }
    }

    pub fn as_pointer(&self) -> Option<&Pointer> {
        match self {
            ValueCell::Pointer(ptr) => Some(ptr),
            _ => None,
        }
    }

    pub fn kind(&self) -> MemCellKind {
        match self {
            ValueCell::Bool(..) => MemCellKind::Bool,
            ValueCell::U8(..) => MemCellKind::U8,
            ValueCell::I32(..) => MemCellKind::I32,
            ValueCell::F32(..) => MemCellKind::F32,
            ValueCell::RcCell(..) => MemCellKind::Rc,
            ValueCell::Function(..) => MemCellKind::Function,
            ValueCell::Structure(..) => MemCellKind::Structure,
            ValueCell::Variant(..) => MemCellKind::Variant,
            ValueCell::Pointer(..) => MemCellKind::Pointer,
            ValueCell::Array(..) => MemCellKind::Array,
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