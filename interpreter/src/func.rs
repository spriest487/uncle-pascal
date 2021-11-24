use crate::{
    marshal::{MarshalResult, Marshaller},
    func::ffi::FfiInvoker,
    ExecResult, Interpreter,
};
use pas_ir::prelude::Metadata;
use pas_ir::{ExternalFunctionRef, FunctionDef, Type};
use std::fmt;

pub mod ffi;

pub type BuiltinFn = fn(state: &mut Interpreter) -> ExecResult<()>;

pub struct BuiltinFunction {
    pub func: BuiltinFn,
    pub return_ty: Type,
    pub param_tys: Vec<Type>,

    pub debug_name: String,
}

pub struct FfiFunction {
    debug_name: String,

    return_ty: Type,
    param_tys: Vec<Type>,

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
        marshaller: &mut Marshaller,
        metadata: &Metadata,
    ) -> MarshalResult<Self> {
        let invoker = marshaller.build_invoker(&func_ref, metadata)?;

        let func = Function::External(FfiFunction {
            debug_name: format!("{}::{}", func_ref.src, func_ref.symbol),
            return_ty: func_ref.return_ty.clone(),
            param_tys: func_ref.params.clone(),

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

    pub fn param_tys(&self) -> &[Type] {
        match self {
            Function::Builtin(builtin_fn) => &builtin_fn.param_tys,
            Function::External(external_fn) => &external_fn.param_tys,
            Function::IR(func_def) => &func_def.params,
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

    pub fn stack_alloc_size(&self, marshaller: &Marshaller) -> MarshalResult<usize> {
        let mut args_size = 0;
        for arg_ty in self.param_tys() {
            let arg_size = marshaller.get_ty(arg_ty)?.size();
            args_size += arg_size;
        }

        let return_size = marshaller.get_ty(self.return_ty())?.size();

        match self {
            Function::IR(def) => {
                let body_size = marshaller.stack_alloc_size(&def.body)?;
                Ok(body_size + args_size + return_size)
            }

            Function::Builtin(..) | Function::External(..) => Ok(args_size + return_size),
        }
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
