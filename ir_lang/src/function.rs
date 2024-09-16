use std::fmt;
use crate::{Label, TypeDefID};
use crate::LocalID;
use crate::Ref;

pub const BUILTIN_SRC: &str = "rt";

pub const RETURN_REF: Ref = Ref::Local(LocalID(0));
pub const EXIT_LABEL: Label = Label(0);

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub struct FunctionID(pub usize);

impl fmt::Display for FunctionID {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "function {}", self.0)
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct StaticClosureID(pub usize);

impl fmt::Display for StaticClosureID {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "static closure #{}", self.0)
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct StaticClosure {
    pub id: StaticClosureID,
    pub init_func: FunctionID,

    pub closure_id: TypeDefID,
    pub func_ty_id: TypeDefID,
}


#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct FunctionSig {
    pub return_ty: crate::Type,
    pub param_tys: Vec<crate::Type>,
}

impl fmt::Display for FunctionSig {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "function(")?;
        for (i, param_ty) in self.param_tys.iter().enumerate() {
            if i > 0 {
                write!(f, "; ")?;
            }
            write!(f, "{}", param_ty)?;
        }
        write!(f, "): {}", self.return_ty)?;

        Ok(())
    }
}
