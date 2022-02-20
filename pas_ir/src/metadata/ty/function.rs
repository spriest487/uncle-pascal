use std::fmt;
use crate::Type;

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct FunctionSig {
    pub return_ty: Type,
    pub param_tys: Vec<Type>,
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