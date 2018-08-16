use std::fmt;

use node::{
    Identifier
};
use types::Type;
use super::BindingKind;

#[derive(Clone, PartialEq)]
pub enum ScopedSymbol {
    /* symbol refers to a name that is in the current scope */
    Local {
        name: Identifier,
        decl_type: Type,
        binding_kind: BindingKind,
    },

    RecordMember {
        record_id: Identifier,
        record_type: Identifier,
        binding_kind: BindingKind,
        name: String,
        member_type: Type,
    },
}


impl ScopedSymbol {
    pub fn decl_type(&self) -> &Type {
        match self {
            ScopedSymbol::Local { decl_type, .. } =>
                decl_type,

            ScopedSymbol::RecordMember { member_type, .. } =>
                member_type,
        }
    }

    pub fn name(&self) -> Identifier {
        match self {
            ScopedSymbol::Local { name, .. } =>
                name.clone(),
            ScopedSymbol::RecordMember { record_id, name, .. } =>
                record_id.child(name),
        }
    }

    pub fn initialized(&self) -> bool {
        match self {
            | ScopedSymbol::RecordMember { binding_kind, .. }
            | ScopedSymbol::Local { binding_kind, .. } =>
                binding_kind.initialized(),
        }
    }

    pub fn mutable(&self, from_ns: Option<&Identifier>) -> bool {
        match self {
            ScopedSymbol::Local { binding_kind, .. } => binding_kind.mutable(),
            ScopedSymbol::RecordMember { record_id, binding_kind, .. } => {
                let same_ns = record_id.parent().as_ref() == from_ns;
                same_ns && binding_kind.mutable()
            }
        }
    }

    pub fn binding_kind(&self) -> BindingKind {
        match self {
            ScopedSymbol::Local { binding_kind, .. } => *binding_kind,
            ScopedSymbol::RecordMember { binding_kind, .. } => *binding_kind,
        }
    }
}

impl fmt::Debug for ScopedSymbol {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ScopedSymbol::Local { name, decl_type, binding_kind } =>
                write!(f, "{} ({} {})", name, binding_kind, decl_type),

            ScopedSymbol::RecordMember {
                record_id,
                name,
                member_type,
                record_type,
                binding_kind
            } => {
                write!(f, "{} ({} member of {} record {})",
                       record_id.child(name),
                       member_type,
                       binding_kind,
                       record_type.name)
            }
        }
    }
}

impl fmt::Display for ScopedSymbol {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ScopedSymbol::Local { name, .. } =>
                write!(f, "{}", name),
            ScopedSymbol::RecordMember { record_id, name, .. } =>
                write!(f, "{}.{}", record_id, name),
        }
    }
}
