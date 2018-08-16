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
    pub fn new(name: node::Identifier, decl_type: DeclaredType) -> Self {
        Self {
            name,
            decl_type,
        }
    }
}

impl node::Symbol for Symbol {
    type Type = DeclaredType;
}

#[derive(Eq, PartialEq, Clone, Debug)]
pub struct FunctionSignature {
    pub name: String,
    pub decl_type: DeclaredType,
    pub args_types: Vec<DeclaredType>,
}

impl fmt::Display for FunctionSignature {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let args_str = self.args_types
            .iter()
            .map(|arg_type| format!("{}", arg_type))
            .collect::<Vec<_>>()
            .join(", ");

        write!(f, "function {}({}): {}", self.name, args_str, self.decl_type)
    }
}

#[derive(Eq, PartialEq, Clone, Debug)]
pub struct DeclaredRecord {
    pub name: String,
    pub members: Vec<Symbol>,
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
    None,
    Integer,
    String,
    Pointer,
    Function(Box<FunctionSignature>),
    Record(DeclaredRecord),
}

impl fmt::Display for DeclaredType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", match self {
            &DeclaredType::None => "<untyped>".to_owned(),
            &DeclaredType::Integer => "System.Integer".to_owned(),
            &DeclaredType::String => "System.String".to_owned(),
            &DeclaredType::Pointer => "System.Pointer".to_owned(),
            &DeclaredType::Function(ref sig) => format!("{}", sig),
            &DeclaredType::Record(ref record) => format!("{}", record),
        })
    }
}

impl From<FunctionSignature> for DeclaredType {
    fn from(sig: FunctionSignature) -> Self {
        DeclaredType::Function(Box::new(sig))
    }
}

pub mod builtin_names {
    use node::*;

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