use std::{
    fmt,
};

use node::{self, Identifier, ToSource, IndexRange};

#[derive(Eq, PartialEq, Clone, Debug, Hash)]
pub struct Symbol {
    pub name: Identifier,
    pub decl_type: Type,
}

impl ToSource for Symbol {
    fn to_source(&self) -> String {
        self.name.to_source()
    }
}

impl fmt::Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} : {}", self.name, self.decl_type)
    }
}

impl Symbol {
    pub fn new<T, TId>(name: TId, decl_type: T) -> Self
        where T: Into<Type>,
              TId: Into<Identifier>,
    {
        Self {
            name: name.into(),
            decl_type: decl_type.into(),
        }
    }
}

impl node::Symbol for Symbol {
    type Type = Type;
}

#[derive(Eq, PartialEq, Clone, Debug, Hash)]
pub struct ArrayType {
    pub element: Box<Type>,
    pub first_dim: IndexRange,
    pub rest_dims: Vec<IndexRange>,
}

impl ArrayType {
    pub fn total_elements(&self) -> u32 {
        self.rest_dims.iter().fold(self.first_dim.elements(), |total, dim| {
            total * dim.elements()
        })
    }
}

#[derive(Eq, PartialEq, Clone, Debug, Hash)]
pub struct FunctionSignature {
    pub return_type: Option<Type>,
    pub arg_types: Vec<Type>,
}

impl ToSource for FunctionSignature {
    fn to_source(&self) -> String {
        let mut source = String::new();
        source.push_str(if self.return_type.is_some() {
            "function"
        } else {
            "procedure"
        });

        if self.arg_types.len() > 0 {
            source.push('(');
            for arg in self.arg_types.iter() {
//                source.push_str(&arg.to_source());
            }
            source.push(')');
        }

        source
    }
}

#[derive(Eq, PartialEq, Clone, Debug, Hash)]
pub enum Type {
    Nil,
    Byte,
    Boolean,
    Int32,
    UInt32,
    Int64,
    UInt64,
    NativeInt,
    NativeUInt,
    RawPointer,
    Pointer(Box<Type>),
    Function(Identifier),
    FunctionPointer(FunctionSignature),
    Record(Identifier),
    Class(Identifier),
    Array(ArrayType),
}

impl ToSource for Type {
    fn to_source(&self) -> String {
        Type::name(Some(self))
    }
}

impl Type {
    pub fn name(decl_type: Option<&Self>) -> String {
        match decl_type {
            None => "(none)".to_string(),
            Some(t) => match t {
                Type::Nil => "nil".to_string(),
                Type::Byte => "System.Byte".to_string(),
                Type::Boolean => "System.Boolean".to_string(),
                Type::Int32 => "System.Int32".to_string(),
                Type::UInt32 => "System.UInt32".to_string(),
                Type::Int64 => "System.Int64".to_string(),
                Type::UInt64 => "System.UInt64".to_string(),
                Type::NativeInt => "System.NativeInt".to_string(),
                Type::NativeUInt => "System.NativeUInt".to_string(),
                Type::RawPointer => "System.Pointer".to_string(),
                Type::Pointer(target) => format!("^{}", target.to_source()),
                Type::Function(name) => format!("{}", name),
                Type::Function(sig) => format!("{}", sig.to_source()),
                Type::Record(record) => format!("{}", record),
                Type::Class(class) => format!("{}", class),
                Type::Array(array) => {
                    let mut name = "array ".to_string();
                    name.push_str(&format!("[{}..{}", array.first_dim.from, array.first_dim.to));
                    for dim in array.rest_dims.iter() {
                        name.push_str(&format!(",{}..{}", dim.from, dim.to));
                    }
                    name.push_str(&format!("] of {}", array.element.to_source()));
                    name
                }
            }
        }
    }

    pub fn is_record(&self) -> bool {
        match self {
            Type::Record(_) => true,
            _ => false,
        }
    }

    pub fn is_class(&self) -> bool {
        match self {
            Type::Class(_) => true,
            _ => false,
        }
    }

    pub fn pointer(self) -> Type {
        Type::Pointer(Box::from(self))
    }

    pub fn is_pointer(&self) -> bool {
        match self {
            Type::Pointer(_) |
            Type::RawPointer => true,

            _ => false,
        }
    }

    pub fn remove_indirection(&self) -> &Type {
        let mut next = self;
        loop {
            match next {
                Type::Pointer(target) => next = target.as_ref(),
                _ => break next
            }
        }
    }

    pub fn with_indirection(&self, level: usize) -> Self {
        let mut result = self.clone();
        for _ in 0..level {
            result = result.pointer();
        }
        result
    }

    pub fn indirection_level(&self) -> usize {
        let mut level = 0;
        let mut next = self;

        loop {
            match next {
                Type::Pointer(target) => {
                    level += 1;
                    next = target.as_ref();
                }

                _ => break level
            }
        }
    }

    /* if this is a typed pointer, what does it dereference to? None if this is a raw pointer
     or not a pointer */
    pub fn deref_type(&self) -> Option<&Type> {
        match self {
            Type::Pointer(ptr_type) => Some(ptr_type.as_ref()),
            _ => None,
        }
    }

    pub fn valid_lhs_type(&self) -> bool {
        match self {
            Type::Pointer(_) |
            Type::Byte |
            Type::Record(_) |
            Type::Class(_) |
            Type::RawPointer |
            Type::Int32 |
            Type::UInt32 |
            Type::Int64 |
            Type::UInt64 |
            Type::NativeInt |
            Type::NativeUInt |
            Type::Boolean => true,

            Type::Array { .. } |
            Type::Nil |
            Type::Function(_) => false,
        }
    }

    pub fn assignable_from(&self, other: &Type) -> bool {
        if !self.valid_lhs_type() {
            return false;
        }

        match (self, other) {
            // nil can be assigned to any pointer, and nothing else
            (Type::RawPointer, Type::Nil) |
            (Type::Pointer(_), Type::Nil) => true,
            (_, Type::Nil) => false,

            (ref a, ref b) if b.promotes_to(a) => true,

            (ref x, ref y) => x == y,
        }
    }

    // can we use the + and - arithmetic operations between these two types?
    pub fn can_offset_by(&self, other: &Type) -> bool {
        match (self, other) {
            // numbers can be offset, as long as the rhs is promotable to the lhs type
            (a, b) if a.is_numeric() && b.promotes_to(a) =>
                true,

            //pointers can be offset, as long as the rhs is promotable to NativeInt
            (ptr, off) if ptr.is_pointer() && off.promotes_to(&Type::NativeInt) =>
                true,

            _ =>
                false,
        }
    }

    // can we use the >, >=, <, and <= operations between these two types?
    pub fn has_ord_comparisons(&self, other: &Type) -> bool {
        self.is_numeric() && other.promotes_to(self)
    }

    pub fn promotes_to(&self, other: &Type) -> bool {
        match (self, other) {
            // numeric types always "promote" to themselves
            (a, b) if a == b && a.is_numeric()
                => true,

            // byte promotes to any larger type
            (Type::Byte, Type::Int32) |
            (Type::Byte, Type::UInt32) |
            (Type::Byte, Type::Int64) |
            (Type::Byte, Type::UInt64) |
            (Type::Byte, Type::NativeInt) |
            (Type::Byte, Type::NativeUInt) |

            // 32-bit integers promote to any 64-bit integer
            (Type::UInt32, Type::Int64) |
            (Type::UInt32, Type::UInt64) |

            // 32-bit integers promote to native ints with the same signedness
            (Type::UInt32, Type::NativeUInt) |
            (Type::Int32, Type::NativeInt)
                => true,

            _ => false,
        }
    }

    pub fn is_numeric(&self) -> bool {
        match self {
            Type::Int32 |
            Type::UInt32 |
            Type::Int64 |
            Type::UInt64 |
            Type::NativeInt |
            Type::NativeUInt
                => true,
            Type::Byte => true,
            _ => false,
        }
    }

    pub fn comparable_to(&self, other: &Type) -> bool {
        let can_compare = |a: &Type, b: &Type| match (a, b) {
            (Type::Int64, Type::Int64) |
            (Type::Byte, Type::Byte) |
            (Type::Boolean, Type::Boolean) |
            (Type::RawPointer, Type::RawPointer) =>
                true,

            (Type::Pointer(_), Type::Nil) |
            (Type::RawPointer, Type::Nil) =>
                true,

            (Type::Pointer(a_target), Type::Pointer(b_target)) => {
                a_target == b_target
            }

            (a, b) => a.promotes_to(b),
        };

        can_compare(self, other) || can_compare(other, self)
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", Type::name(Some(self)))
    }
}
