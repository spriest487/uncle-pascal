#[derive(Eq, PartialEq, Hash, Copy, Clone, Debug)]
pub enum Primitive {
    Boolean,
    Byte,
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
        Primitive::Byte,
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

    pub fn name(&self) -> &str {
        match self {
            Primitive::Boolean => "Boolean",
            Primitive::Byte => "Byte",
            Primitive::Int8 => "Int8",
            Primitive::Int16 => "Int16",
            Primitive::UInt16 => "UInt16",
            Primitive::Int32 => "Integer",
            Primitive::UInt32 => "UInt32",
            Primitive::Int64 => "Int64",
            Primitive::UInt64 => "UInt64",
            Primitive::NativeInt => "NativeInt",
            Primitive::NativeUInt => "NativeUInt",
            Primitive::Real32 => "Real32",
            Primitive::Pointer => "Pointer",
        }
    }
}
