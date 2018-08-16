use std::fmt;

use node;

#[derive(Eq, PartialEq, Clone, Debug)]
pub struct Symbol {
    pub name: node::Identifier,
    pub decl_type: DeclaredType,
}

impl fmt::Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} : {}", self.name, self.decl_type)
    }
}

impl Symbol {
    pub fn new<T, TId>(name: TId, decl_type: T) -> Self
        where T: Into<DeclaredType>,
              TId: Into<node::Identifier>,
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

#[derive(Eq, PartialEq, Clone, Debug)]
pub struct FunctionSignature {
    pub name: String,
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

#[derive(Eq, PartialEq, Clone, Debug)]
pub struct DeclaredRecord {
    pub name: String,
    pub members: Vec<Symbol>,
}

impl DeclaredRecord {
    pub fn get_member(&self, name: &str) -> Option<&Symbol> {
        self.members.iter().find(|m| m.name.to_string() == name)
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

#[derive(Eq, PartialEq, Clone, Debug)]
pub enum DeclaredType {
    Byte,
    Boolean,
    Integer,
    String,
    Pointer,
    Function(Box<FunctionSignature>),
    Record(DeclaredRecord),
}

impl fmt::Display for DeclaredType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", match self {
            &DeclaredType::Byte => "System.Byte".to_owned(),
            &DeclaredType::Boolean => "System.Boolean".to_owned(),
            &DeclaredType::Integer => "System.Integer".to_owned(),
            &DeclaredType::String => "System.String".to_owned(),
            &DeclaredType::Pointer => "System.Pointer".to_owned(),
            &DeclaredType::Function(ref sig) => format!("{}", sig),
            &DeclaredType::Record(ref record) => format!("{}", record),
        })
    }
}

impl DeclaredType {
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
}

impl From<FunctionSignature> for DeclaredType {
    fn from(sig: FunctionSignature) -> Self {
        DeclaredType::Function(Box::new(sig))
    }
}

pub mod builtin_names {
    use node::*;

    pub fn system_byte() -> Identifier {
        Identifier::parse("System.Byte")
    }

    pub fn system_boolean() -> Identifier {
        Identifier::parse("System.Boolean")
    }

    pub fn system_integer() -> Identifier {
        Identifier::parse("System.Integer")
    }

    pub fn system_string() -> Identifier {
        Identifier::parse("System.String")
    }

    pub fn system_pointer() -> Identifier {
        Identifier::parse("System.Pointer")
    }
}