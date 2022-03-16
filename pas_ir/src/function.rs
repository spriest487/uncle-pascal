use std::fmt;
use std::rc::Rc;
use pas_common::span::Span;
use pas_syn::IdentPath;
use crate::{FunctionID, Instruction, pas_ty, Type, TypeDefID};

#[derive(Clone, Debug)]
pub struct ExternalFunctionRef {
    pub symbol: String,
    pub src: String,

    pub return_ty: Type,
    pub params: Vec<Type>,

    pub src_span: Span,
}

pub const BUILTIN_SRC: &str = "rt";

#[derive(Clone, Debug)]
pub struct FunctionDef {
    pub debug_name: String,

    pub body: Vec<Instruction>,
    pub return_ty: Type,
    pub params: Vec<Type>,

    pub src_span: Span,
}

#[derive(Clone, Debug)]
pub enum Function {
    External(ExternalFunctionRef),
    Local(FunctionDef),
}

impl Function {
    pub fn debug_name(&self) -> &str {
        match self {
            Function::External(ExternalFunctionRef { symbol, .. }) => symbol.as_str(),
            Function::Local(FunctionDef { debug_name, .. }) => debug_name.as_str(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct FunctionInstance {
    pub id: FunctionID,
    pub sig: Rc<pas_ty::FunctionSig>,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum FunctionDeclKey {
    Function {
        name: IdentPath,
    },
    Method {
        iface: IdentPath,
        self_ty: pas_ty::Type,
        method: pas_syn::Ident,
    },
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub(crate) struct FunctionDefKey {
    pub decl_key: FunctionDeclKey,
    pub type_args: Option<pas_ty::TypeList>,
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