use std::fmt;

#[derive(Clone, Debug, PartialEq, Copy, Eq, Hash)]
pub enum ValueKind {
    /// local value in an immutable location
    Immutable,

    /// uninitialized mutable location
    Uninitialized,

    /// local value in mutable location
    Mutable,

    /// rvalue, e.g. value returned from function, result of operation,
    /// with no binding
    Temporary,
}

impl fmt::Display for ValueKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ValueKind::Uninitialized => write!(f, "Uninitialized binding"),
            ValueKind::Immutable => write!(f, "Immutable binding"),
            ValueKind::Mutable => write!(f, "Mutable binding"),
            ValueKind::Temporary => write!(f, "Temporary value"),
        }
    }
}

impl ValueKind {
    pub fn mutable(self) -> bool {
        match self {
            ValueKind::Mutable | ValueKind::Uninitialized => true,
            _ => false,
        }
    }
}
