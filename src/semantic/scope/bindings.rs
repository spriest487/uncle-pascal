use std::fmt;

use types::Type;

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum BindingKind {
    Mutable,
    Uninitialized,
    Immutable,
}

impl fmt::Display for BindingKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            BindingKind::Mutable => write!(f, "mutable"),
            BindingKind::Uninitialized => write!(f, "uninitialized"),
            BindingKind::Immutable => write!(f, "immutable"),
        }
    }
}

impl BindingKind {
    pub fn mutable(&self) -> bool {
        match self {
            | BindingKind::Uninitialized
            | BindingKind::Mutable =>
                true,

            | BindingKind::Immutable =>
                false
        }
    }

    pub fn initialized(&self) -> bool {
        match self {
            | BindingKind::Immutable
            | BindingKind::Mutable =>
                true,

            | BindingKind::Uninitialized =>
                false
        }
    }

    pub fn initialize(self) -> Self {
        match self {
            | BindingKind::Mutable
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
