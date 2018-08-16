use std::fmt;

use node::Identifier;
use types::{
    ParameterizedName,
    Type,
};

#[derive(Clone, Debug, PartialEq)]
enum NameKind {
    UserType,
    UserSymbol,
    ClassInterfaces,
    ClassVTable { interface: Box<Name> },
    Method { interface: Box<Name>, for_type: Box<Name> },
    InterfaceCall { interface: Box<Name>, for_type: Box<Name> },
    AbstractCall { interface: Box<Name> },
    InterfaceVTable,
    InternalType,
    InternalSymbol,
    Constructor,
    Local,
    LocalInternal,
    Member,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Name {
    name: String,
    kind: NameKind,
}

impl Name {
    pub fn user_type(qualified_name: &ParameterizedName) -> Self {
        Name {
            kind: NameKind::UserType,
            name: parameterized_id_to_c(qualified_name),
        }
    }

    pub fn method(interface: &Identifier,
                  for_type: &ParameterizedName,
                  name: impl Into<String>)
                  -> Self {
        Name {
            kind: NameKind::Method {
                interface: Box::new(Name::user_type(&ParameterizedName::new_simple(interface.clone()))),
                for_type: Box::new(Name::user_type(for_type)),
            },
            name: name.into(),
        }
    }

    pub fn interface_call(interface: &Identifier,
                          for_type: &ParameterizedName,
                          name: impl Into<String>)
                          -> Self {
        Name {
            kind: NameKind::InterfaceCall {
                interface: Box::new(Name::user_type(&ParameterizedName::new_simple(interface.clone()))),
                for_type: Box::new(Name::user_type(for_type)),
            },
            name: name.into(),
        }
    }

    pub fn abstract_call(interface: &Identifier, name: impl Into<String>) -> Self {
        Name {
            kind: NameKind::AbstractCall {
                interface: Box::new(Name::user_type(&ParameterizedName::new_simple(interface.clone())))
            },
            name: name.into(),
        }
    }

    pub fn interface_vtable(interface: &Identifier) -> Self {
        Name {
            kind: NameKind::InterfaceVTable,
            name: identifier_to_c(interface),
        }
    }

    pub fn class_interfaces(class_name: &ParameterizedName) -> Self {
        Name {
            kind: NameKind::ClassInterfaces,
            name: parameterized_id_to_c(class_name),
        }
    }

    pub fn class_vtable(class_name: &ParameterizedName, interface_name: &Identifier) -> Self {
        Name {
            kind: NameKind::ClassVTable {
                interface: Box::new(Name::user_type(&ParameterizedName::new_simple(interface_name.clone())))
            },
            name: parameterized_id_to_c(class_name),
        }
    }

    pub fn user_symbol(qualified_name: &Identifier) -> Self {
        Name {
            kind: NameKind::UserSymbol,
            name: identifier_to_c(qualified_name),
        }
    }

    pub fn internal_type(name: impl Into<String>) -> Self {
        Name {
            kind: NameKind::InternalType,
            name: name.into(),
        }
    }

    pub fn internal_symbol(name: impl Into<String>) -> Self {
        Name {
            kind: NameKind::InternalSymbol,
            name: name.into(),
        }
    }

    pub fn local(name: impl Into<String>) -> Self {
        Name {
            kind: NameKind::Local,
            name: name.into(),
        }
    }

    pub fn local_internal(name: impl Into<String>) -> Self {
        Name {
            kind: NameKind::LocalInternal,
            name: name.into(),
        }
    }

    pub fn constructor(type_id: &ParameterizedName) -> Self {
        Name {
            kind: NameKind::Constructor,
            name: parameterized_id_to_c(type_id),
        }
    }

    pub fn member(name: impl Into<String>) -> Self {
        Name {
            kind: NameKind::Member,
            name: name.into(),
        }
    }
}

impl fmt::Display for Name {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.kind {
            | NameKind::UserType
            => write!(f, "PascalType_{}", self.name),

            | NameKind::UserSymbol
            => write!(f, "Pascal_{}", self.name),

            | NameKind::Method { interface, for_type }
            => write!(f, "{}_Method_{}_{}", interface, for_type, self.name),

            | NameKind::AbstractCall { interface }
            => write!(f, "{}_VirtualCall_{}", interface, self.name),

            | NameKind::ClassInterfaces
            => write!(f, "PascalType_Interfaces_{}", self.name),

            | NameKind::InterfaceVTable
            => write!(f, "PascalType_{}_VTable", self.name),

            | NameKind::ClassVTable { interface }
            => write!(f, "{}_VTable_{}", interface, self.name),

            | NameKind::InterfaceCall { interface, for_type }
            => write!(f, "{}_VirtualCall_{}_{}", interface, for_type, self.name),

            | NameKind::InternalSymbol
            | NameKind::InternalType
            => write!(f, "System_Internal_{}", self.name),

            | NameKind::Local
            => write!(f, "local_{}", self.name),

            | NameKind::LocalInternal
            => write!(f, "internal_{}", self.name),

            | NameKind::Constructor
            => write!(f, "Constructor_{}", self.name),

            | NameKind::Member
            => write!(f, "member_{}", self.name),
        }
    }
}

fn identifier_to_c(id: &Identifier) -> String {
    let mut parts = id.namespace.clone();
    parts.push(id.name.replace('_', "_OF_"));

    parts.join("_")
}

fn name_for_type_param(ty: &Type) -> String {
    match ty {
        | Type::Nil
        | Type::UntypedRef
        | Type::Generic(_)
        => unreachable!("illegal generic type arg"),

        | Type::Function(_)
        | Type::Array(_)
        | Type::DynamicArray(_)
        => unimplemented!("type param name for {} (c++ backend)", ty),

        | Type::Record(type_id)
        => format!("Record_{}", parameterized_id_to_c(type_id)),
        | Type::Class(type_id)
        => format!("Class_{}", parameterized_id_to_c(type_id)),

        | Type::AnyImplementation(iface) => format!("AnyImpl_{}", identifier_to_c(iface)),


        | Type::Pointer(pointed_to)
        => format!("PointerTo_{}", name_for_type_param(pointed_to.as_ref())),

        | Type::RawPointer => "Pointer".to_string(),
        | Type::Boolean => "Boolean".to_string(),
        | Type::Byte => "Byte".to_string(),
        | Type::UInt32 => "UInt32".to_string(),
        | Type::Int32 => "Int32".to_string(),
        | Type::UInt64 => "UInt64".to_string(),
        | Type::Int64 => "Int64".to_string(),
        | Type::NativeUInt => "NativeUInt".to_string(),
        | Type::NativeInt => "NativeInt".to_string(),
        | Type::Float64 => "Float64".to_string(),

        | Type::Set(type_id)
        | Type::Enumeration(type_id)
        => identifier_to_c(type_id)
    }
}

fn parameterized_id_to_c(id: &ParameterizedName) -> String {
    let mut name = identifier_to_c(&id.name);
    for (i, arg) in id.type_args.iter().enumerate() {
        name.push_str(&format!("_TP{}_{}", i, name_for_type_param(arg)));
    }

    name
}