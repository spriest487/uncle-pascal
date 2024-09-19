use crate::typ::ast::Literal;
use crate::IntConstant;
use crate::RealConstant;
use std::mem::size_of;

#[derive(Eq, PartialEq, Hash, Copy, Clone, Debug)]
pub enum Primitive {
    Boolean,
    UInt8,
    Int8,
    Int16,
    UInt16,
    Int32,
    UInt32,
    Int64,
    UInt64,
    NativeInt,
    NativeUInt,
    Real32,
    Pointer,
}

impl Primitive {
    pub const ALL: [Primitive; 13] = [
        Primitive::Boolean,
        Primitive::UInt8,
        Primitive::Int8,
        Primitive::Int16,
        Primitive::UInt16,
        Primitive::Int32,
        Primitive::UInt32,
        Primitive::Int64,
        Primitive::UInt64,
        Primitive::NativeInt,
        Primitive::NativeUInt,
        Primitive::Real32,
        Primitive::Pointer,
    ];

    pub const fn is_numeric(&self) -> bool {
        match self {
            Primitive::Boolean => false,
            _ => true,
        }
    }

    pub const fn is_signed(&self) -> bool {
        match self {
            Primitive::Int8
            | Primitive::Int16
            | Primitive::Int32
            | Primitive::Int64
            | Primitive::NativeInt
            | Primitive::Real32 => true,

            _ => false,
        }
    }

    pub const fn is_integer(&self) -> bool {
        match self {
            | Primitive::UInt8
            | Primitive::Int8
            | Primitive::Int16
            | Primitive::UInt16
            | Primitive::Int32
            | Primitive::UInt32
            | Primitive::Int64
            | Primitive::UInt64
            | Primitive::NativeInt
            | Primitive::NativeUInt
            | Primitive::Pointer => true,

            | _ => false,
        }
    }

    pub const fn is_pointer(&self) -> bool {
        match self {
            Primitive::Pointer => true,
            _ => false,
        }
    }

    pub const fn is_real(&self) -> bool {
        match self {
            | Primitive::Real32 => true,
            | _ => false,
        }
    }

    pub const fn native_size(&self) -> usize {
        match self {
            | Primitive::UInt8 => size_of::<u8>(),
            | Primitive::Int8 => size_of::<i8>(),
            | Primitive::Int16 => size_of::<i16>(),
            | Primitive::UInt16 => size_of::<u16>(),
            | Primitive::Int32 => size_of::<u32>(),
            | Primitive::UInt32 => size_of::<u32>(),
            | Primitive::Int64 => size_of::<u64>(),
            | Primitive::UInt64 => size_of::<u64>(),
            | Primitive::NativeInt => size_of::<isize>(),
            | Primitive::NativeUInt => size_of::<usize>(),
            | Primitive::Pointer => size_of::<*const u8>(),
            | Primitive::Boolean => size_of::<bool>(),
            | Primitive::Real32 => size_of::<f32>(),
        }
    }

    pub const fn name(&self) -> &str {
        match self {
            Primitive::Boolean => "Boolean",
            Primitive::UInt8 => "UInt8",
            Primitive::Int8 => "Int8",
            Primitive::Int16 => "Int16",
            Primitive::UInt16 => "UInt16",
            Primitive::Int32 => "Int32",
            Primitive::UInt32 => "UInt32",
            Primitive::Int64 => "Int64",
            Primitive::UInt64 => "UInt64",
            Primitive::NativeInt => "NativeInt",
            Primitive::NativeUInt => "NativeUInt",
            Primitive::Real32 => "Real32",
            Primitive::Pointer => "Pointer",
        }
    }

    pub fn default_val(&self) -> Literal {
        match self {
            | Primitive::Boolean => Literal::Boolean(false),
            | Primitive::Real32 => Literal::Real(RealConstant::from(0.0)),
            | Primitive::Pointer => Literal::Nil,

            | Primitive::UInt8
            | Primitive::Int8
            | Primitive::Int16
            | Primitive::UInt16
            | Primitive::Int32
            | Primitive::UInt32
            | Primitive::Int64
            | Primitive::UInt64
            | Primitive::NativeInt
            | Primitive::NativeUInt => Literal::Integer(IntConstant::from(0)),
        }
    }
}