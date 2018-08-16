use std::{
    fmt,
    mem::size_of,
};

use node::{self, Identifier, ToSource, IndexRange};

#[derive(Eq, PartialEq, Clone, Debug, Hash)]
pub struct Symbol {
    pub name: Identifier,
    pub decl_type: DeclaredType,
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
        where T: Into<DeclaredType>,
              TId: Into<Identifier>,
    {
        Self {
            name: name.into(),
            decl_type: decl_type.into(),
        }
    }
}

impl node::Symbol for Symbol {
    type Type = DeclaredType;
}

#[derive(Eq, PartialEq, Clone, Debug, Hash)]
pub struct FunctionSignature {
    pub name: Identifier,
    pub return_type: Option<DeclaredType>,
    pub arg_types: Vec<DeclaredType>,
}

impl fmt::Display for FunctionSignature {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let args_str = self.arg_types
            .iter()
            .map(|arg_type| format!("{}", arg_type))
            .collect::<Vec<_>>()
            .join(", ");

        if let Some(ref return_type) = self.return_type {
            write!(f, "function {}({}): {}", self.name, args_str, return_type)
        } else {
            write!(f, "procedure {}({})", self.name, args_str)
        }
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub enum RecordKind {
    Record,
    Class,
}

#[derive(Eq, PartialEq, Clone, Debug, Hash)]
pub struct DeclaredRecord {
    pub name: Identifier,
    pub kind: RecordKind,
    pub members: Vec<Symbol>,
}

impl DeclaredRecord {
    pub fn get_member(&self, name: &str) -> Option<&Symbol> {
        self.members.iter().find(|m| m.name.to_string() == name)
    }

    pub fn size_of(&self) -> usize {
        self.members.iter()
            .map(|member| member.decl_type.align_of())
            .sum()
    }
}

impl fmt::Display for DeclaredRecord {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "record")?;

        for member in self.members.iter() {
            writeln!(f, "\t{}", member)?;
        }

        writeln!(f, "end")
    }
}

#[derive(Eq, PartialEq, Clone, Debug, Hash)]
pub struct ArrayType {
    pub element: Box<DeclaredType>,
    pub first_dim: IndexRange,
    pub rest_dims: Vec<IndexRange>,
}

#[derive(Eq, PartialEq, Clone, Debug, Hash)]
pub enum DeclaredType {
    Nil,
    Byte,
    Boolean,
    Integer,
    RawPointer,
    Pointer(Box<DeclaredType>),
    Function(Box<FunctionSignature>),
    Record(DeclaredRecord),
    Array(ArrayType),
}

impl ToSource for DeclaredType {
    fn to_source(&self) -> String {
        DeclaredType::name(Some(self))
    }
}

impl DeclaredType {
    pub fn name(decl_type: Option<&Self>) -> String {
        match decl_type {
            None => "(none)".to_string(),
            Some(t) => match t {
                DeclaredType::Nil => "nil".to_string(),
                DeclaredType::Byte => "System.Byte".to_string(),
                DeclaredType::Boolean => "System.Boolean".to_string(),
                DeclaredType::Integer => "System.Integer".to_string(),
                DeclaredType::RawPointer => "System.Pointer".to_string(),
                DeclaredType::Pointer(target) => format!("^{}", target.to_source()),
                DeclaredType::Function(sig) => format!("{}", sig),
                DeclaredType::Record(record) => format!("{}", record.name),
                DeclaredType::Array(array) => {
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
//
//    pub fn annotate(parsed_type: &ParsedType, scope: &Scope) -> SemanticResult<Self> {
//
//    }
//
    pub fn align_of(&self) -> usize {
        const WORD_SIZE: usize = size_of::<u32>();

        let size = self.size_of();
        let mut align = 0;

        while align < size {
            align += WORD_SIZE
        }
        align
    }

    pub fn size_of(&self) -> usize {
        match self {
            DeclaredType::Nil |
            DeclaredType::RawPointer |
            DeclaredType::Pointer(_) |
            DeclaredType::Function(_) |
            DeclaredType::Integer => size_of::<usize>(),

            DeclaredType::Byte => size_of::<u8>(),
            DeclaredType::Boolean => size_of::<u8>(),

            DeclaredType::Record(rec) => rec.size_of(),
            DeclaredType::Array(array) => {
                let total_len = array.rest_dims.iter()
                    .fold(array.first_dim.len(), |total, dim| {
                        total * dim.len()
                    });

                total_len * array.element.size_of()
            }
        }
    }

    pub fn is_record(&self) -> bool {
        match self {
            &DeclaredType::Record(_) => true,
            _ => false,
        }
    }

    pub fn unwrap_record(self) -> DeclaredRecord {
        match self {
            DeclaredType::Record(record) => record,
            _ => panic!("called unwrap_record on {}", self)
        }
    }

    pub fn pointer(self) -> DeclaredType {
        DeclaredType::Pointer(Box::from(self))
    }

    pub fn is_pointer(&self) -> bool {
        match self {
            DeclaredType::Pointer(_) |
            DeclaredType::RawPointer => true,

            _ => false,
        }
    }

    pub fn remove_indirection(&self) -> &DeclaredType {
        let mut next = self;
        loop {
            match next {
                &DeclaredType::Pointer(ref target) => next = target.as_ref(),
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
                &DeclaredType::Pointer(ref target) => {
                    level += 1;
                    next = target.as_ref();
                }

                _ => break level
            }
        }
    }

    /* if this is a typed pointer, what does it dereference to? None if this is a raw pointer
     or not a pointer */
    pub fn deref_type(&self) -> Option<&DeclaredType> {
        match self {
            DeclaredType::Pointer(ptr_type) => Some(ptr_type.as_ref()),
            _ => None,
        }
    }

    pub fn valid_lhs_type(&self) -> bool {
        match self {
            DeclaredType::Pointer(_) |
            DeclaredType::Byte |
            DeclaredType::Record(_) |
            DeclaredType::RawPointer |
            DeclaredType::Integer |
            DeclaredType::Boolean => true,

            DeclaredType::Array { .. } |
            DeclaredType::Nil |
            DeclaredType::Function(_) => false,
        }
    }

    pub fn assignable_from(&self, other: &DeclaredType) -> bool {
        if !self.valid_lhs_type() {
            return false;
        }

        match (self, other) {
            // nil can be assigned to any pointer, and nothing else
            (&DeclaredType::RawPointer, &DeclaredType::Nil) |
            (&DeclaredType::Pointer(_), &DeclaredType::Nil) => true,
            (_, &DeclaredType::Nil) => false,

            //TODO: we should only allow byte<-int assignment if the int is in 0..255
            (&DeclaredType::Byte, &DeclaredType::Integer) => true,

            (ref x, ref y) => x == y,
        }
    }

    // can we use the + and - arithmetic operations between these two types?
    pub fn can_offset_by(&self, other: &DeclaredType) -> bool {
        match (self, other) {
            // numbers can be offset, as long as the rhs is promotable to the lhs type
            (a, b) if a.is_numeric() && b.promotes_to(a) =>
                true,

            //pointers can be offset, as long as the rhs is promotable to Integer
            (ptr, off) if ptr.is_pointer() && off.promotes_to(&DeclaredType::Integer) =>
                true,

            _ =>
                false,
        }
    }

    // can we use the >, >=, <, and <= operations between these two types?
    pub fn has_ord_comparisons(&self, other: &DeclaredType) -> bool {
        self.is_numeric() && other.promotes_to(self)
    }

    pub fn promotes_to(&self, other: &DeclaredType) -> bool {
        match (self, other) {
            (a, b) if a == b && a.is_numeric() => true,
            (DeclaredType::Byte, DeclaredType::Integer) => true,

            _ => false,
        }
    }

    pub fn is_numeric(&self) -> bool {
        match self {
            DeclaredType::Integer => true,
            DeclaredType::Byte => true,
            _ => false,
        }
    }

    pub fn comparable_to(&self, other: &DeclaredType) -> bool {
        let can_compare = |a: &DeclaredType, b: &DeclaredType| match (a, b) {
            (DeclaredType::Integer, DeclaredType::Integer) |
            (DeclaredType::Byte, DeclaredType::Byte) |
            (DeclaredType::Boolean, DeclaredType::Boolean) |
            (DeclaredType::RawPointer, DeclaredType::RawPointer) =>
                true,

            (DeclaredType::Pointer(_), DeclaredType::Nil) |
            (DeclaredType::RawPointer, DeclaredType::Nil) =>
                true,

            (DeclaredType::Pointer(a_target), DeclaredType::Pointer(b_target)) => {
                a_target == b_target
            }

            (a, b) => a.promotes_to(b),
        };

        can_compare(self, other) || can_compare(other, self)
    }

    pub fn function(sig: FunctionSignature) -> Self {
        DeclaredType::Function(Box::new(sig))
    }
}

impl fmt::Display for DeclaredType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", DeclaredType::name(Some(self)))
    }
}

impl From<FunctionSignature> for DeclaredType {
    fn from(sig: FunctionSignature) -> Self {
        DeclaredType::Function(Box::new(sig))
    }
}
