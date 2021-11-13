pub mod ffi;

use crate::{ExecResult, Interpreter};
use pas_ir::{ExternalFunctionRef, FunctionDef, Type};
use std::fmt;

use crate::func::ffi::{FfiCache, FfiInvoker};
use pas_ir::prelude::Metadata;

pub type BuiltinFn = fn(state: &mut Interpreter) -> ExecResult<()>;

pub struct BuiltinFunction {
    pub func: BuiltinFn,
    pub return_ty: Type,

    pub debug_name: String,
}

pub struct FfiFunction {
    debug_name: String,

    return_ty: Type,

    invoker: FfiInvoker,
}

pub enum Function {
    Builtin(BuiltinFunction),
    External(FfiFunction),
    IR(FunctionDef),
}

impl Function {
    pub fn new_ffi(
        func_ref: &ExternalFunctionRef,
        ffi_cache: &mut FfiCache,
        metadata: &Metadata,
    ) -> ExecResult<Self> {
        let invoker = ffi_cache.build_invoker(&func_ref, metadata)?;

        let func = Function::External(FfiFunction {
            debug_name: format!("{}::{}", func_ref.src, func_ref.symbol),
            return_ty: func_ref.return_ty.clone(),

            invoker,
        });

        Ok(func)
    }

    pub fn return_ty(&self) -> &Type {
        match self {
            Function::Builtin(def) => &def.return_ty,
            Function::External(def) => &def.return_ty,
            Function::IR(def) => &def.return_ty,
        }
    }

    pub fn debug_name(&self) -> &str {
        match self {
            Function::Builtin(def) => &def.debug_name,
            Function::External(def) => &def.debug_name,
            Function::IR(def) => &def.debug_name,
        }
    }

    pub fn invoke(&self, state: &mut Interpreter) -> ExecResult<()> {
        match self {
            Function::Builtin(def) => {
                if state.trace_ir {
                    println!("calling {} (interpreter builtin)", def.debug_name);
                }
                (def.func)(state)?
            }

            Function::External(def) => def.invoker.invoke(state)?,

            Function::IR(def) => {
                if state.trace_ir {
                    println!("entering {}", def.debug_name);
                }
                state.execute(&def.body)?;
                if state.trace_ir {
                    println!("exiting {}", def.debug_name);
                }
            }
        };

        Ok(())
    }
}

impl fmt::Debug for Function {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Function::Builtin { .. } => write!(f, "<native code>"),
            Function::IR(func) => write!(f, "<function with {} instructions>", func.body.len()),
            Function::External(func) => write!(f, "<{}>", func.debug_name),
        }
    }
}
