use crate::func::Function;
use crate::ptr::Pointer;
use pas_ir::metadata::{ClassID, FieldID, StructID};
use pas_ir::Type;
use std::ops::{Index, IndexMut};
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct StructValue {
    pub id: StructID,
    pub fields: Vec<DynValue>,
}

impl StructValue {
    pub fn struct_ty(&self) -> Type {
        Type::Struct(self.id)
    }
}

impl Index<FieldID> for StructValue {
    type Output = DynValue;

    fn index(&self, index: FieldID) -> &DynValue {
        &self.fields[index.0]
    }
}

impl IndexMut<FieldID> for StructValue {
    fn index_mut(&mut self, index: FieldID) -> &mut DynValue {
        &mut self.fields[index.0]
    }
}

impl PartialEq<Self> for StructValue {
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
pub struct VariantValue {
    pub id: StructID,
    pub tag: Box<DynValue>,
    pub data: Box<DynValue>,
}

impl VariantValue {
    pub fn variant_ty(&self) -> Type {
        Type::Variant(self.id)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RcValue {
    pub resource_ptr: Pointer,
    pub ref_count: usize,
    pub struct_id: StructID,
}

#[derive(Debug, Clone)]
pub struct ArrayValue {
    pub el_ty: Type,
    pub elements: Vec<DynValue>,
}

impl ArrayValue {
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

    pub fn array_ty(&self) -> Type {
        self.el_ty.clone().array(self.elements.len())
    }
}

#[derive(Debug, Clone)]
pub enum DynValue {
    Bool(bool),
    I8(i8),
    U8(u8),
    I16(i16),
    U16(u16),
    I32(i32),
    U32(u32),
    I64(i64),
    U64(u64),
    ISize(isize),
    USize(usize),
    F32(f32),
    Rc(Box<RcValue>),
    Function(Rc<Function>),
    Structure(Box<StructValue>),
    Variant(Box<VariantValue>),
    Pointer(Pointer),
    Array(Box<ArrayValue>),
}

impl DynValue {
    pub fn try_cast(&self, ty: &Type) -> Option<Self> {
        match ty {
            | Type::I8 => self.to_bigint().map(|x| x as i8).map(DynValue::I8),
            | Type::U8 => self.to_bigint().map(|x| x as u8).map(DynValue::U8),
            | Type::I16 => self.to_bigint().map(|x| x as i16).map(DynValue::I16),
            | Type::U16 => self.to_bigint().map(|x| x as u16).map(DynValue::U16),
            | Type::I32 => self.to_bigint().map(|x| x as i32).map(DynValue::I32),
            | Type::U32 => self.to_bigint().map(|x| x as u32).map(DynValue::U32),
            | Type::I64 => self.to_bigint().map(|x| x as i64).map(DynValue::I64),
            | Type::U64 => self.to_bigint().map(|x| x as u64).map(DynValue::U64),
            | Type::ISize => self.to_bigint().map(|x| x as isize).map(DynValue::ISize),
            | Type::USize => self.to_bigint().map(|x| x as usize).map(DynValue::USize),
            | Type::Bool => self.to_bigint().map(|i| DynValue::Bool(i != 0)),

            | Type::F32 => {
                if let DynValue::F32(..) = self {
                    return Some(self.clone());
                }

                self.to_bigint().map(|x| x as f32).map(DynValue::F32)
            }

            | Type::Pointer(deref_ty) => {
                let addr = cast::usize(self.to_bigint()?).ok()?;
                let ptr = Pointer {
                    ty: (**deref_ty).clone(),
                    addr
                };
                Some(DynValue::Pointer(ptr))
            }

            | Type::RcObject(..)
            | Type::Nothing => None,

            | Type::RcPointer(class_id) => {
                let addr = cast::usize(self.to_bigint()?).ok()?;
                let ptr = Pointer {
                    addr,
                    ty: match class_id {
                        Some(ClassID::Class(struct_id)) => Type::RcObject(Some(*struct_id)),
                        None | Some(ClassID::Interface(..)) => Type::RcObject(None),
                    }
                };
                Some(DynValue::Pointer(ptr))
            }

            | Type::Struct(id) => match self {
                DynValue::Structure(s) if s.id == *id => Some(self.clone()),
                _ => None,
            }

            | Type::Variant(id) => match self {
                DynValue::Variant(v) if v.id == *id => Some(self.clone()),
                _ => None,
            }

            | Type::Array { element, dim } => match self {
                DynValue::Array(arr) if arr.el_ty == **element && arr.elements.len() == *dim => {
                    Some(self.clone())
                }
                _ => None,
            }
        }
    }

    pub fn try_eq(&self, other: &Self) -> Option<bool> {
        match (self, other) {
            (DynValue::Bool(a), DynValue::Bool(b)) => Some(a == b),
            (DynValue::I8(a), DynValue::I8(b)) => Some(a == b),
            (DynValue::U8(a), DynValue::U8(b)) => Some(a == b),
            (DynValue::I16(a), DynValue::I16(b)) => Some(a == b),
            (DynValue::U16(a), DynValue::U16(b)) => Some(a == b),
            (DynValue::I32(a), DynValue::I32(b)) => Some(a == b),
            (DynValue::U32(a), DynValue::U32(b)) => Some(a == b),
            (DynValue::I64(a), DynValue::I64(b)) => Some(a == b),
            (DynValue::U64(a), DynValue::U64(b)) => Some(a == b),
            (DynValue::ISize(a), DynValue::ISize(b)) => Some(a == b),
            (DynValue::USize(a), DynValue::USize(b)) => Some(a == b),

            (DynValue::F32(a), DynValue::F32(b)) => Some(a == b),

            (DynValue::Pointer(a), DynValue::Pointer(b)) => Some(a == b),
            (DynValue::Structure(a), DynValue::Structure(b)) => Some(a == b),
            (DynValue::Array(a), DynValue::Array(b)) => a.try_eq(b),
            _ => None,
        }
    }

    pub fn try_not(&self) -> Option<bool> {
        match self {
            DynValue::Bool(b) => Some(!*b),
            _ => None,
        }
    }

    pub fn try_add(&self, other: &Self) -> Option<Self> {
        match (self, other) {
            (DynValue::I8(a), DynValue::I8(b)) => Some(DynValue::I8(a + b)),
            (DynValue::U8(a), DynValue::U8(b)) => Some(DynValue::U8(a + b)),
            (DynValue::I16(a), DynValue::I16(b)) => Some(DynValue::I16(a + b)),
            (DynValue::U16(a), DynValue::U16(b)) => Some(DynValue::U16(a + b)),
            (DynValue::I32(a), DynValue::I32(b)) => Some(DynValue::I32(a + b)),
            (DynValue::U32(a), DynValue::U32(b)) => Some(DynValue::U32(a + b)),
            (DynValue::I64(a), DynValue::I64(b)) => Some(DynValue::I64(a + b)),
            (DynValue::U64(a), DynValue::U64(b)) => Some(DynValue::U64(a + b)),
            (DynValue::ISize(a), DynValue::ISize(b)) => Some(DynValue::ISize(a + b)),
            (DynValue::USize(a), DynValue::USize(b)) => Some(DynValue::USize(a + b)),
            (DynValue::Pointer(a), DynValue::Pointer(b)) => Some(DynValue::Pointer(a.addr_add(b.addr))),

            (DynValue::F32(a), DynValue::F32(b)) => Some(DynValue::F32(a + b)),

            _ => None,
        }
    }

    pub fn try_sub(&self, other: &Self) -> Option<Self> {
        match (self, other) {
            (DynValue::I8(a), DynValue::I8(b)) => Some(DynValue::I8(a - b)),
            (DynValue::U8(a), DynValue::U8(b)) => Some(DynValue::U8(a - b)),
            (DynValue::I16(a), DynValue::I16(b)) => Some(DynValue::I16(a - b)),
            (DynValue::U16(a), DynValue::U16(b)) => Some(DynValue::U16(a - b)),
            (DynValue::I32(a), DynValue::I32(b)) => Some(DynValue::I32(a - b)),
            (DynValue::U32(a), DynValue::U32(b)) => Some(DynValue::U32(a - b)),
            (DynValue::I64(a), DynValue::I64(b)) => Some(DynValue::I64(a - b)),
            (DynValue::U64(a), DynValue::U64(b)) => Some(DynValue::U64(a - b)),
            (DynValue::ISize(a), DynValue::ISize(b)) => Some(DynValue::ISize(a - b)),
            (DynValue::USize(a), DynValue::USize(b)) => Some(DynValue::USize(a - b)),
            (DynValue::Pointer(a), DynValue::Pointer(b)) => Some(DynValue::Pointer(a.addr_sub(b.addr))),

            (DynValue::F32(a), DynValue::F32(b)) => Some(DynValue::F32(a - b)),

            _ => None,
        }
    }

    pub fn try_mul(&self, other: &Self) -> Option<Self> {
        match (self, other) {
            (DynValue::I8(a), DynValue::I8(b)) => Some(DynValue::I8(a * b)),
            (DynValue::U8(a), DynValue::U8(b)) => Some(DynValue::U8(a * b)),
            (DynValue::I16(a), DynValue::I16(b)) => Some(DynValue::I16(a * b)),
            (DynValue::U16(a), DynValue::U16(b)) => Some(DynValue::U16(a * b)),
            (DynValue::I32(a), DynValue::I32(b)) => Some(DynValue::I32(a * b)),
            (DynValue::U32(a), DynValue::U32(b)) => Some(DynValue::U32(a * b)),
            (DynValue::I64(a), DynValue::I64(b)) => Some(DynValue::I64(a * b)),
            (DynValue::U64(a), DynValue::U64(b)) => Some(DynValue::U64(a * b)),
            (DynValue::ISize(a), DynValue::ISize(b)) => Some(DynValue::ISize(a * b)),
            (DynValue::USize(a), DynValue::USize(b)) => Some(DynValue::USize(a * b)),
            (DynValue::Pointer(a), DynValue::Pointer(b)) => Some(DynValue::Pointer(a.addr_mul(b.addr))),

            (DynValue::F32(a), DynValue::F32(b)) => Some(DynValue::F32(a * b)),

            _ => None,
        }
    }

    pub fn try_idiv(&self, other: &Self) -> Option<Self> {
        match (self, other) {
            (DynValue::I8(a), DynValue::I8(b)) => Some(DynValue::I8(a / b)),
            (DynValue::U8(a), DynValue::U8(b)) => Some(DynValue::U8(a / b)),
            (DynValue::I16(a), DynValue::I16(b)) => Some(DynValue::I16(a / b)),
            (DynValue::U16(a), DynValue::U16(b)) => Some(DynValue::U16(a / b)),
            (DynValue::I32(a), DynValue::I32(b)) => Some(DynValue::I32(a / b)),
            (DynValue::U32(a), DynValue::U32(b)) => Some(DynValue::U32(a / b)),
            (DynValue::I64(a), DynValue::I64(b)) => Some(DynValue::I64(a / b)),
            (DynValue::U64(a), DynValue::U64(b)) => Some(DynValue::U64(a / b)),
            (DynValue::ISize(a), DynValue::ISize(b)) => Some(DynValue::ISize(a / b)),
            (DynValue::USize(a), DynValue::USize(b)) => Some(DynValue::USize(a / b)),
            (DynValue::Pointer(a), DynValue::Pointer(b)) => Some(DynValue::Pointer(a.addr_div(b.addr))),

            (DynValue::F32(a), DynValue::F32(b)) => Some(DynValue::F32(a / b)),

            _ => None,
        }
    }

    pub fn try_shl(&self, other: &Self) -> Option<Self> {
        match (self, other) {
            (DynValue::I8(a), DynValue::I8(b)) => Some(DynValue::I8(a << b)),
            (DynValue::U8(a), DynValue::U8(b)) => Some(DynValue::U8(a << b)),
            (DynValue::I16(a), DynValue::I16(b)) => Some(DynValue::I16(a << b)),
            (DynValue::U16(a), DynValue::U16(b)) => Some(DynValue::U16(a << b)),
            (DynValue::I32(a), DynValue::I32(b)) => Some(DynValue::I32(a << b)),
            (DynValue::U32(a), DynValue::U32(b)) => Some(DynValue::U32(a << b)),
            (DynValue::I64(a), DynValue::I64(b)) => Some(DynValue::I64(a << b)),
            (DynValue::U64(a), DynValue::U64(b)) => Some(DynValue::U64(a << b)),
            (DynValue::ISize(a), DynValue::ISize(b)) => Some(DynValue::ISize(a << b)),
            (DynValue::USize(a), DynValue::USize(b)) => Some(DynValue::USize(a << b)),
            (DynValue::Pointer(a), DynValue::Pointer(b)) => Some(DynValue::Pointer(a.addr_shl(b.addr))),

            _ => None,
        }
    }

    pub fn try_shr(&self, other: &Self) -> Option<Self> {
        match (self, other) {
            (DynValue::I8(a), DynValue::I8(b)) => Some(DynValue::I8(a >> b)),
            (DynValue::U8(a), DynValue::U8(b)) => Some(DynValue::U8(a >> b)),
            (DynValue::I16(a), DynValue::I16(b)) => Some(DynValue::I16(a >> b)),
            (DynValue::U16(a), DynValue::U16(b)) => Some(DynValue::U16(a >> b)),
            (DynValue::I32(a), DynValue::I32(b)) => Some(DynValue::I32(a >> b)),
            (DynValue::U32(a), DynValue::U32(b)) => Some(DynValue::U32(a >> b)),
            (DynValue::I64(a), DynValue::I64(b)) => Some(DynValue::I64(a >> b)),
            (DynValue::U64(a), DynValue::U64(b)) => Some(DynValue::U64(a >> b)),
            (DynValue::ISize(a), DynValue::ISize(b)) => Some(DynValue::ISize(a >> b)),
            (DynValue::USize(a), DynValue::USize(b)) => Some(DynValue::USize(a >> b)),
            (DynValue::Pointer(a), DynValue::Pointer(b)) => Some(DynValue::Pointer(a.addr_shr(b.addr))),

            _ => None,
        }
    }

    pub fn try_gt(&self, other: &Self) -> Option<bool> {
        match (self, other) {
            (DynValue::I32(a), DynValue::I32(b)) => Some(a > b),
            (DynValue::U8(a), DynValue::U8(b)) => Some(a > b),
            (DynValue::F32(a), DynValue::F32(b)) => Some(a > b),
            (DynValue::Bool(a), DynValue::Bool(b)) => Some(a > b),
            (DynValue::Pointer(a), DynValue::Pointer(b)) => Some(a > b),

            _ => None,
        }
    }

    pub fn as_function(&self) -> Option<&Rc<Function>> {
        match self {
            DynValue::Function(f) => Some(f),
            _ => None,
        }
    }

    pub fn as_struct_mut(&mut self, struct_id: StructID) -> Option<&mut StructValue> {
        match self {
            DynValue::Structure(struct_val) if struct_id == struct_val.id => Some(struct_val),
            _ => None,
        }
    }

    pub fn as_struct(&self, struct_id: StructID) -> Option<&StructValue> {
        match self {
            DynValue::Structure(struct_val) if struct_id == struct_val.id => Some(struct_val),
            _ => None,
        }
    }

    pub fn as_array(&self, el_ty: &Type) -> Option<&[DynValue]> {
        match self {
            DynValue::Array(arr) if arr.el_ty == *el_ty => Some(&arr.elements),
            _ => None,
        }
    }

    pub fn as_variant(&self, struct_id: StructID) -> Option<&VariantValue> {
        match self {
            DynValue::Variant(var_val) if struct_id == var_val.id => Some(var_val),
            _ => None,
        }
    }

    pub fn as_rc_mut(&mut self) -> Option<&mut RcValue> {
        match self {
            DynValue::Rc(rc) => Some(rc),
            _ => None,
        }
    }

    pub fn as_rc(&self) -> Option<&RcValue> {
        match self {
            DynValue::Rc(rc) => Some(rc),
            _ => None,
        }
    }

    pub fn as_bool(&self) -> Option<bool> {
        match self {
            DynValue::Bool(b) => Some(*b),
            _ => None,
        }
    }

    pub fn as_i8(&self) -> Option<i8> {
        match self {
            DynValue::I8(x) => Some(*x),
            _ => None,
        }
    }

    pub fn as_u8(&self) -> Option<u8> {
        match self {
            DynValue::U8(x) => Some(*x),
            _ => None,
        }
    }

    pub fn as_i16(&self) -> Option<i16> {
        match self {
            DynValue::I16(i) => Some(*i),
            _ => None,
        }
    }

    pub fn as_u16(&self) -> Option<u16> {
        match self {
            DynValue::U16(i) => Some(*i),
            _ => None,
        }
    }

    pub fn as_i32(&self) -> Option<i32> {
        match self {
            DynValue::I32(i) => Some(*i),
            _ => None,
        }
    }

    pub fn as_u32(&self) -> Option<u32> {
        match self {
            DynValue::U32(i) => Some(*i),
            _ => None,
        }
    }

    pub fn as_i64(&self) -> Option<i64> {
        match self {
            DynValue::I64(i) => Some(*i),
            _ => None,
        }
    }

    pub fn as_u64(&self) -> Option<u64> {
        match self {
            DynValue::U64(i) => Some(*i),
            _ => None,
        }
    }

    pub fn as_isize(&self) -> Option<isize> {
        match self {
            DynValue::ISize(i) => Some(*i),
            _ => None,
        }
    }

    pub fn as_usize(&self) -> Option<usize> {
        match self {
            DynValue::USize(i) => Some(*i),
            _ => None,
        }
    }

    // might need to replace i128 with an actual bigint type at one point, this is valid as long
    // as i128 can hold any representable integer type in the language
    pub fn to_bigint(&self) -> Option<i128> {
        match self {
            DynValue::I8(x) => Some(*x as i128),
            DynValue::U8(x) => Some(*x as i128),
            DynValue::I16(x) => Some(*x as i128),
            DynValue::U16(x) => Some(*x as i128),
            DynValue::I32(x) => Some(*x as i128),
            DynValue::U32(x) => Some(*x as i128),
            DynValue::I64(x) => Some(*x as i128),
            DynValue::U64(x) => Some(*x as i128),
            DynValue::ISize(x) => Some(*x as i128),
            DynValue::USize(x) => Some(*x as i128),
            DynValue::Pointer(ptr) => Some(ptr.addr as i128),
            DynValue::Bool(true) => Some(1),
            DynValue::Bool(false) => Some(0),
            _ => None,
        }
    }

    pub fn as_pointer(&self) -> Option<&Pointer> {
        match self {
            DynValue::Pointer(ptr) => Some(ptr),
            _ => None,
        }
    }
}

impl From<RcValue> for DynValue {
    fn from(rc_val: RcValue) -> Self {
        DynValue::Rc(Box::new(rc_val))
    }
}