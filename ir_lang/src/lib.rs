mod formatter;
mod function;
mod instruction;
mod metadata;
mod ty;
mod ty_decl;
mod val;

pub use formatter::*;
pub use function::*;
pub use instruction::*;
pub use metadata::*;
use std::fmt;
pub use ty::*;
pub use ty_decl::*;
pub use val::*;

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct NamePath {
    pub path: Vec<String>,
    pub type_args: Option<Vec<Type>>,
}

impl NamePath {
    pub fn new(ns: impl IntoIterator<Item = String>, name: impl Into<String>) -> Self {
        let mut path: Vec<_> = ns.into_iter().collect();
        path.push(name.into());

        NamePath {
            path,
            type_args: None,
        }
    }

    pub fn with_ty_args(self, args: impl IntoIterator<Item = Type>) -> Self {
        assert!(
            self.type_args.is_none(),
            "with_type_args: name must not already have a type argument list"
        );

        Self {
            path: self.path,
            type_args: Some(args.into_iter().collect()),
        }
    }
}

impl fmt::Display for NamePath {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        RawInstructionFormatter.format_name(self, f)
    }
}
