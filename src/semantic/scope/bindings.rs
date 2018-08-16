use std::fmt;

use types::Type;

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum BindingKind {
    Mutable,
    Uninitialized,
    Immutable,
    Global,
    Function,
    Internal,
}

impl fmt::Display for BindingKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            BindingKind::Mutable => write!(f, "mutable"),
            BindingKind::Uninitialized => write!(f, "uninitialized"),
            BindingKind::Immutable => write!(f, "immutable"),
            BindingKind::Global => write!(f, "global"),
            BindingKind::Function => write!(f, "function"),
            BindingKind::Internal => write!(f, "internal"),
        }
    }
}

impl BindingKind {
    pub fn mutable(&self) -> bool {
        match self {
            | BindingKind::Uninitialized
            | BindingKind::Global
            | BindingKind::Mutable
            => true,

            | BindingKind::Immutable
            | BindingKind::Function
            | BindingKind::Internal
            => false
        }
    }

    pub fn initialized(&self) -> bool {
        match self {
            | BindingKind::Immutable
            | BindingKind::Mutable
            | BindingKind::Global
            | BindingKind::Function
            | BindingKind::Internal
            => true,

            | BindingKind::Uninitialized
            => false
        }
    }

    pub fn initialize(self) -> Self {
        match self {
            | BindingKind::Global
            | BindingKind::Mutable
            | BindingKind::Function
            | BindingKind::Internal
            | BindingKind::Immutable =>
                self,
            | BindingKind::Uninitialized =>
                BindingKind::Mutable,
        }
    }
}

#[derive(Clone, Debug)]
pub struct SymbolBinding {
    pub decl_type: Type,
    pub kind: BindingKind,
}
