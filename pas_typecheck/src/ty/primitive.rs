#[derive(Eq, PartialEq, Hash, Copy, Clone, Debug)]
pub enum Primitive {
    Boolean,
    Byte,
    Int32,
    Real32,
    Pointer,
}

impl Primitive {
    pub const ALL: [Primitive; 5] = [
        Primitive::Boolean,
        Primitive::Byte,
        Primitive::Int32,
        Primitive::Real32,
        Primitive::Pointer,
    ];

    pub fn name(&self) -> &str {
        match self {
            Primitive::Boolean => "Boolean",
            Primitive::Byte => "Byte",
            Primitive::Int32 => "Integer",
            Primitive::Real32 => "Real32",
            Primitive::Pointer => "Pointer",
        }
    }
}
