use std::fmt;
use crate::{VirtualTypeID, FunctionInstance, Type, TypeDefID};

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

#[derive(Debug, Clone)]
pub struct ClosureInstance {
    pub func_instance: FunctionInstance,
    pub func_ty_id: TypeDefID,
    pub closure_id: TypeDefID,
}

impl ClosureInstance {
    pub fn closure_ptr_ty(&self) -> Type {
        Type::RcPointer(VirtualTypeID::Closure(self.func_ty_id))
    }
}

impl fmt::Display for ClosureInstance {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "closure {} of {} ({})", self.closure_id, self.func_ty_id, self.func_instance.sig)
    }
}