use std::{
    collections::HashSet,
    fmt::{
        self,
        Write,
    }
};

use node::{
    Identifier,
    ConstExpression,
};
use consts::{
    IntConstant,
    FloatConstant,
    SetConstant,
    EnumConstant,
};
use semantic::{
    IndexRange,
    FunctionSignature,
};

#[derive(PartialEq, Eq, Clone, Debug, Hash)]
pub struct DynamicArrayType {
    pub element: Box<Type>,
}

#[derive(PartialEq, Eq, Clone, Debug, Hash)]
pub struct ArrayType {
    pub element: Box<Type>,
    pub first_dim: IndexRange,
    pub rest_dims: Vec<IndexRange>,
}

impl ArrayType {
    pub fn total_elements(&self) -> usize {
        self.rest_dims.iter().fold(self.first_dim.elements(), |total, dim| {
            total * dim.elements()
        })
    }

    pub fn next_rank(&self) -> Option<ArrayType> {
        let mut next_dims = self.rest_dims.iter().cloned();

        match next_dims.next() {
            Some(next_dim) => {
                Some(ArrayType {
                    element: self.element.clone(),
                    first_dim: next_dim,
                    rest_dims: next_dims.collect(),

                })
            }

            None => None
        }
    }
}

#[derive(PartialEq, Eq, Clone, Debug, Hash)]
pub enum ReferenceType {
    Class(ParameterizedName),
    DynamicArray(DynamicArrayType),
    AnyImplementation(Identifier),
}

impl fmt::Display for ReferenceType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ReferenceType::Class(name) => write!(f, "{}", name),
            ReferenceType::DynamicArray(arr) => write!(f, "array of {}", arr.element),
            ReferenceType::AnyImplementation(interface) => write!(f, "{}", interface),
        }
    }
}

#[derive(PartialEq, Eq, Clone, Debug, Hash)]
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
    Float64,
    UntypedRef,
    Function(Box<FunctionSignature>),
    Record(ParameterizedName),
    Reference(ReferenceType),
    WeakReference(ReferenceType),
    Array(ArrayType),
    Enumeration(Identifier),
    Set(Identifier),
    Generic(String),
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Type::Nil => write!(f, "nil"),
            Type::Generic(name) => write!(f, "{}", name),
            Type::Byte => write!(f, "System.Byte"),
            Type::Boolean => write!(f, "System.Boolean"),
            Type::Int32 => write!(f, "System.Int32"),
            Type::UInt32 => write!(f, "System.UInt32"),
            Type::Int64 => write!(f, "System.Int64"),
            Type::UInt64 => write!(f, "System.UInt64"),
            Type::NativeInt => write!(f, "System.NativeInt"),
            Type::NativeUInt => write!(f, "System.NativeUInt"),
            Type::Float64 => write!(f, "System.Float64"),
            Type::UntypedRef => write!(f, "(untyped reference)"),
            Type::RawPointer => write!(f, "System.Pointer"),
            Type::Pointer(target) => write!(f, "^{}", target),
            Type::Function(sig) => write!(f, "{}", sig),
            Type::Enumeration(enum_id) => write!(f, "{}", enum_id),
            Type::Set(set_id) => write!(f, "{}", set_id),
            Type::Record(name) => write!(f, "{}", name),
            Type::Reference(ref_type) => write!(f, "{}", ref_type),
            Type::WeakReference(ref_type) => write!(f, "weak {}", ref_type),
            Type::Array(array) => {
                write!(f, "array ")?;
                write!(f, "[{}..{}", array.first_dim.from, array.first_dim.to)?;
                for dim in &array.rest_dims {
                    write!(f, ",{}..{}", dim.from, dim.to)?;
                }
                write!(f, "] of {}", array.element)
            }
        }
    }
}

impl Type {
    pub fn name(decl_type: Option<&Self>) -> String {
        match decl_type {
            None => "(none)".to_string(),
            Some(t) => t.to_string()
        }
    }

    pub fn is_record(&self) -> bool {
        match self {
            Type::Record { .. } => true,
            _ => false,
        }
    }

    pub fn dynamic_array_ref(element: impl Into<Type>) -> Self {
        Type::Reference(ReferenceType::DynamicArray(DynamicArrayType {
            element: Box::new(element.into())
        }))
    }

    pub fn class_ref(name: impl Into<ParameterizedName>) -> Self {
        Type::Reference(ReferenceType::Class(name.into()))
    }

    pub fn class_ref_weak(name: impl Into<ParameterizedName>) -> Self {
        Type::WeakReference(ReferenceType::Class(name.into()))
    }

    pub fn interface_ref(name: impl Into<Identifier>) -> Self {
        Type::Reference(ReferenceType::AnyImplementation(name.into()))
    }

    pub fn is_class_ref(&self) -> bool {
        match self {
            Type::Reference(ReferenceType::Class(_)) => true,
            Type::WeakReference(ReferenceType::Class(_)) => true,
            _ => false,
        }
    }

    pub fn is_ref_counted(&self) -> bool {
        match self {
            | Type::Reference(_) | Type::WeakReference(_) => true,
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

    pub fn is_interface_ref(&self) -> bool {
        match self {
            | Type::Reference(ReferenceType::AnyImplementation(_))
            | Type::WeakReference(ReferenceType::AnyImplementation(_))
            => true,

            _ => false
        }
    }

    pub fn as_class_ref(&self) -> Option<&ParameterizedName> {
        match self {
            | Type::Reference(ReferenceType::Class(name))
            | Type::WeakReference(ReferenceType::Class(name))
            => Some(name),

            _ => None,
        }
    }

    pub fn as_record(&self) -> Option<&ParameterizedName> {
        match self {
            | Type::Record(name) => Some(name),
            | _ => None,
        }
    }

    pub fn as_interface_ref(&self) -> Option<&Identifier> {
        match self {
            | Type::Reference(ReferenceType::AnyImplementation(iface))
            | Type::WeakReference(ReferenceType::AnyImplementation(iface))
            => Some(iface),
            | _ => None,
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
            | Type::Pointer(_)
            | Type::Byte
            | Type::Record { .. }
            | Type::RawPointer
            | Type::UntypedRef
            | Type::Boolean
            | Type::Int32
            | Type::UInt32
            | Type::Int64
            | Type::UInt64
            | Type::NativeInt
            | Type::NativeUInt
            | Type::Float64
            | Type::Function(_)
            | Type::Enumeration(_)
            | Type::Set(_)
            | Type::Reference(_)
            | Type::WeakReference(_)
            | Type::Generic(_)
            | Type::Array { .. }
            => true,

            | Type::Nil
            => false
        }
    }

    // can we use the + and - arithmetic operations between these two types?
    pub fn can_offset_by(&self, other: &Type) -> bool {
        match (self, other) {
            // numbers can be offset, as long as the rhs is promotable to the lhs type
            | (a, b) if a.is_numeric() && b.promotes_to(a)
            => true,

            //pointers can be offset, as long as the rhs is promotable to NativeInt
            | (ptr, off) if ptr.is_pointer() && off.promotes_to(&Type::NativeInt)
            => true,

            | _ => false,
        }
    }

    // can we use the >, >=, <, and <= operations between these two types?
    pub fn has_ord_comparisons(&self, other: &Type) -> bool {
        self.is_numeric() && other.promotes_to(self)
    }

    pub fn default_value(&self) -> Option<ConstExpression> {
        match self {
            | Type::Byte => Some(ConstExpression::Integer(IntConstant::from(0u8))),
            | Type::Int32 => Some(ConstExpression::Integer(IntConstant::from(0i32))),
            | Type::UInt32 => Some(ConstExpression::Integer(IntConstant::from(0u32))),
            | Type::Int64 => Some(ConstExpression::Integer(IntConstant::from(0u64))),
            | Type::UInt64 => Some(ConstExpression::Integer(IntConstant::from(0u64))),
            | Type::NativeInt => Some(ConstExpression::Integer(IntConstant::from(0isize))),
            | Type::NativeUInt => Some(ConstExpression::Integer(IntConstant::from(0usize))),
            | Type::Float64 => Some(ConstExpression::Float(FloatConstant::from(0f64))),
            | Type::Boolean => Some(ConstExpression::Boolean(false)),
            | Type::Set(set_id) => Some(ConstExpression::Set(SetConstant {
                included_values: HashSet::new(),
                set: set_id.clone(),
            })),
            | Type::Nil => Some(ConstExpression::Nil),
            | Type::Enumeration(enum_id) => Some(ConstExpression::Enum(EnumConstant {
                enumeration: enum_id.clone(),
                ordinal: 0,
            })),
            | Type::RawPointer
            | Type::WeakReference(_)
            | Type::Pointer(_)
            => Some(ConstExpression::Nil),

            | Type::Record(_) /* todo: can default if all members are defaultable */
            | Type::Array(_) /* todo: can default if element is defaultable */
            | Type::Reference(_)
            | Type::UntypedRef
            | Type::Function(_)
            | Type::Generic(_)
            => None,
        }
    }

    pub fn promotes_to(&self, other: &Type) -> bool {
        match (self, other) {
            // types always "promote" to themselves
            | (a, b) if a == b => true,

            // any number promotes to a float64
            | (a, Type::Float64) if a.is_numeric() => true,

            // byte promotes to any larger type
            | (Type::Byte, Type::Int32)
            | (Type::Byte, Type::UInt32)
            | (Type::Byte, Type::Int64)
            | (Type::Byte, Type::UInt64)
            | (Type::Byte, Type::NativeInt)
            | (Type::Byte, Type::NativeUInt)

            // 32-bit integers promote to any 64-bit integer
            | (Type::UInt32, Type::Int64)
            | (Type::UInt32, Type::UInt64)

            // 32-bit integers promote to native ints with the same signedness
            | (Type::UInt32, Type::NativeUInt)
            | (Type::Int32, Type::NativeInt)
            => true,

            /* weak refs promote to strong refs of the same type (todo: they shouldn't) */
            | (Type::WeakReference(a), Type::Reference(b))
            => *a == *b,

            /* arrays promote to arrays with the same element and number of elements per rank
                (the indexing scheme can vary) */
            | (Type::Array(array_a), Type::Array(array_b)) => {
                array_a.element == array_b.element
                    && array_a.first_dim.elements() == array_b.first_dim.elements()
                    && array_a.rest_dims.iter().enumerate()
                    .map(|(rank_index, rank_a)| {
                        (rank_a, &array_b.rest_dims[rank_index])
                    })
                    .all(|(rank_a, rank_b)| {
                        rank_a.elements() == rank_b.elements()
                    })
            }

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
            Type::NativeUInt |
            Type::Float64
            => true,
            Type::Byte
            => true,
            _ => false,
        }
    }

    pub fn comparable_to(&self, other: &Type) -> bool {
        let can_compare = |a: &Type, b: &Type| match (a, b) {
            (Type::Int64, Type::Int64) |
            (Type::Byte, Type::Byte) |
            (Type::Boolean, Type::Boolean) |
            (Type::Float64, Type::Float64) |
            (Type::RawPointer, Type::RawPointer) =>
                true,
            (Type::Pointer(_), Type::Nil) |
            (Type::RawPointer, Type::Nil) =>
                true,
            (Type::Pointer(a_target), Type::Pointer(b_target)) => {
                a_target == b_target
            }

            (Type::WeakReference(_), Type::Nil) => true,

            (a, b) => a.promotes_to(b),
        };

        can_compare(self, other) || can_compare(other, self)
    }

    pub fn is_weak(&self) -> bool {
        match self {
            Type::WeakReference(_) => true,
            _ => false,
        }
    }
}

#[derive(Clone, Eq, PartialEq, Debug, Hash)]
pub struct ParameterizedName {
    pub name: Identifier,
    pub type_args: Vec<Type>,
}

impl ParameterizedName {
    pub fn new_simple(name: impl Into<Identifier>) -> Self {
        ParameterizedName {
            name: name.into(),
            type_args: Vec::new(),
        }
    }

    pub fn new_with_args(name: impl Into<Identifier>,
                         type_args: impl IntoIterator<Item=Type>)
                         -> Self {
        ParameterizedName {
            name: name.into(),
            type_args: type_args.into_iter().collect(),
        }
    }
}

impl fmt::Display for ParameterizedName {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.name)?;

        if !self.type_args.is_empty() {
            f.write_char('<')?;

            for (i, arg) in self.type_args.iter().enumerate() {
                if i > 0 {
                    f.write_str(", ")?;
                }
                write!(f, "{}", arg)?;
            }

            f.write_char('>')?;
        }
        Ok(())
    }
}