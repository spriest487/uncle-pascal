use std::fmt;

use node::Identifier;

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
    pub fn user_type(qualified_name: &Identifier) -> Self {
        Name {
            kind: NameKind::UserType,
            name: identifier_to_c(qualified_name),
        }
    }

    pub fn method(interface: &Identifier,
                          for_type: &Identifier,
                          name: impl ToString)
                          -> Self {
        Name {
            kind: NameKind::Method {
                interface: Box::new(Name::user_type(interface)),
                for_type: Box::new(Name::user_type(for_type)),
            },
            name: name.to_string(),
        }
    }

    pub fn interface_call(interface: &Identifier,
                          for_type: &Identifier,
                          name: impl ToString)
                          -> Self {
        Name {
            kind: NameKind::InterfaceCall {
                interface: Box::new(Name::user_type(interface)),
                for_type: Box::new(Name::user_type(for_type)),
            },
            name: name.to_string(),
        }
    }

    pub fn abstract_call(interface: &Identifier, name: impl ToString) -> Self {
        Name {
            kind: NameKind::AbstractCall {
                interface: Box::new(Name::user_type(interface))
            },
            name: name.to_string(),
        }
    }

    pub fn interface_vtable(interface: &Identifier) -> Self {
        Name {
            kind: NameKind::InterfaceVTable,
            name: identifier_to_c(interface),
        }
    }

    pub fn class_interfaces(class_name: &Identifier) -> Self {
        Name {
            kind: NameKind::ClassInterfaces,
            name: identifier_to_c(class_name),
        }
    }

    pub fn class_vtable(class_name: &Identifier, interface_name: &Identifier) -> Self {
        Name {
            kind: NameKind::ClassVTable {
                interface: Box::new(Name::user_type(interface_name))
            },
            name: identifier_to_c(class_name),
        }
    }

    pub fn user_symbol(qualified_name: &Identifier) -> Self {
        Name {
            kind: NameKind::UserSymbol,
            name: identifier_to_c(qualified_name),
        }
    }

    pub fn internal_type(name: impl ToString) -> Self {
        Name {
            kind: NameKind::InternalType,
            name: name.to_string(),
        }
    }

    pub fn internal_symbol(name: impl ToString) -> Self {
        Name {
            kind: NameKind::InternalSymbol,
            name: name.to_string(),
        }
    }

    pub fn local(name: impl ToString) -> Self {
        Name {
            kind: NameKind::Local,
            name: name.to_string(),
        }
    }

    pub fn local_internal(name: impl ToString) -> Self {
        Name {
            kind: NameKind::LocalInternal,
            name: name.to_string(),
        }
    }

    pub fn constructor(type_id: &Identifier) -> Self {
        Name {
            kind: NameKind::Constructor,
            name: identifier_to_c(type_id),
        }
    }

    pub fn member(name: impl ToString) -> Self {
        Name {
            kind: NameKind::Member,
            name: name.to_string(),
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