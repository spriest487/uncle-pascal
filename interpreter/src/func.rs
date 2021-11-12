use std::fmt;
use std::rc::Rc;
use pas_ir::{FunctionDef, Type};
use crate::Interpreter;

pub type BuiltinFn = fn(state: &mut Interpreter);

#[derive(Clone)]
pub struct BuiltinFunctionDef {
    pub func: BuiltinFn,
    pub ret: Type,
    pub debug_name: String,
}

#[derive(Clone)]
pub enum Function {
    Builtin(BuiltinFunctionDef),
    IR(Rc<FunctionDef>),
}

impl Function {
    pub fn return_ty(&self) -> &Type {
        match self {
            Function::Builtin(def) => &def.ret,
            Function::IR(def) => &def.return_ty,
        }
    }

    pub fn debug_name(&self) -> &str {
        match self {
            Function::Builtin(def) => &def.debug_name,
            Function::IR(def) => &def.debug_name,
        }
    }
}

impl fmt::Debug for Function {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Function::Builtin { .. } => write!(f, "<native code>"),
            Function::IR(func) => write!(f, "<function with {} instructions>", func.body.len()),
        }
    }
}
