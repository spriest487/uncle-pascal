mod instruction;
mod val;
mod ty;
mod ty_decl;
mod metadata;
mod formatter;
mod function;

use std::fmt;
pub use instruction::*;
pub use val::*;
pub use ty::*;
pub use metadata::*;
pub use formatter::*;
pub use function::*;
pub use ty_decl::*;

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct NamePath {
    pub path: Vec<String>,
    pub type_args: Option<Vec<Type>>,
}

impl fmt::Display for NamePath {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        RawInstructionFormatter.format_name(self, f)
    }
}
