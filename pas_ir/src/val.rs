use std::fmt;
use crate::{FunctionID, StringID};

#[derive(Debug, Clone, PartialEq)]
pub enum Ref {
    Discard,
    // write-only ref that doesn't result in mov instructions when written to
    Local(LocalID),
    Global(GlobalRef),
    Deref(Box<Value>),
}

impl Ref {
    pub fn to_deref(self) -> Self {
        Ref::Deref(Box::new(Value::Ref(self)))
    }
}

impl fmt::Display for Ref {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Ref::Discard => write!(f, "_"),
            Ref::Local(id) => write!(f, "{}", id),
            Ref::Global(name) => write!(f, "{}", name),
            Ref::Deref(at) => write!(f, "{}^", at),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Ref(Ref),
    LiteralNull,
    LiteralBool(bool),
    LiteralByte(u8),
    LiteralI8(i8),
    LiteralI16(i16),
    LiteralU16(u16),
    LiteralI32(i32),
    LiteralU32(u32),
    LiteralI64(i64),
    LiteralU64(u64),
    LiteralF32(f32),
    LiteralISize(isize),
    LiteralUSize(usize),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Ref(r) => write!(f, "{}", r),
            Value::LiteralByte(i) => write!(f, "{}u8", i),
            Value::LiteralI8(i) => write!(f, "{}i8", i),
            Value::LiteralI16(i) => write!(f, "{}i16", i),
            Value::LiteralU16(i) => write!(f, "{}u16", i),
            Value::LiteralI32(i) => write!(f, "{}i32", i),
            Value::LiteralU32(i) => write!(f, "{}u32", i),
            Value::LiteralI64(i) => write!(f, "{}i64", i),
            Value::LiteralU64(i) => write!(f, "{}u64", i),
            Value::LiteralISize(i) => write!(f, "{}isize", i),
            Value::LiteralUSize(i) => write!(f, "{}usize", i),
            Value::LiteralBool(b) => write!(f, "{}", b),
            Value::LiteralF32(x) => write!(f, "{:.6}", x),
            Value::LiteralNull => write!(f, "NULL"),
        }
    }
}

impl From<Ref> for Value {
    fn from(r: Ref) -> Self {
        Value::Ref(r)
    }
}

impl Value {
    pub fn deref(self) -> Ref {
        Ref::Deref(Box::new(self))
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum GlobalRef {
    Function(FunctionID),
    StringLiteral(StringID),
    StaticClosure(FunctionID),
}

impl fmt::Display for GlobalRef {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            GlobalRef::Function(func_id) => write!(f, "{}", func_id),
            GlobalRef::StringLiteral(id) => write!(f, "string literal `{}`", id),
            GlobalRef::StaticClosure(id) => write!(f, "static closure of {}", id),
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub struct LocalID(pub usize);

impl fmt::Display for LocalID {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "%{}", self.0)
    }
}