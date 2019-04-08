use std::fmt;
use pas_ir::{
    metadata::{FunctionID},
    self as ir,
};
use crate::ast::{
    Type,
    Module,
    Statement,
    Builder,
};

#[derive(Eq, PartialEq, Hash, Copy, Clone)]
pub enum FunctionName {
    Main,
    ID(FunctionID),

    // runtime functions
    RcAlloc,
//    RcFree,
//    RcRetain,
//    RcRelease,

    // builtins
    IntToStr,
    StrToInt,
    WriteLn,
    GetMem,
    FreeMem,
}

impl fmt::Display for FunctionName {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            FunctionName::Main => write!(f, "main"),
            FunctionName::ID(id) => write!(f, "Function_{}", id.0),

            FunctionName::RcAlloc => write!(f, "RcAlloc"),

            FunctionName::WriteLn => write!(f, "System_WriteLn"),
            FunctionName::IntToStr => write!(f, "System_IntToStr"),
            FunctionName::StrToInt => write!(f, "System_StrToInt"),
            FunctionName::GetMem => write!(f, "System_GetMem"),
            FunctionName::FreeMem => write!(f, "System_FreeMem"),
        }
    }
}

pub struct FunctionDecl {
    pub name: FunctionName,
    pub return_ty: Type,
    pub params: Vec<Type>,
}

impl FunctionDecl {
    pub fn translate(id: FunctionID, func: &ir::Function, module: &mut Module) -> Self {
        let name = FunctionName::ID(id);
        let return_ty = Type::from_metadata(&func.return_ty, module);
        let params = func
            .params
            .iter()
            .map(|param| Type::from_metadata(param, module))
            .collect();

        Self {
            name,
            return_ty,
            params,
        }
    }
}

impl fmt::Display for FunctionDecl {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let name = self.name.to_string();

        write!(f, "{}(", self.return_ty.to_decl_string(&name))?;
        for (i, param) in self.params.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }

            let arg_id = if self.return_ty != Type::Void {
                i + 1
            } else {
                i
            };

            let name = format!("L{}", arg_id);
            write!(f, "{}", param.to_decl_string(&name))?;
        }
        write!(f, ")")
    }
}

pub struct FunctionDef {
    pub decl: FunctionDecl,
    pub body: Vec<Statement>,
}

impl FunctionDef {
    pub fn translate(id: FunctionID, func: &ir::Function, module: &mut Module) -> Self {
        let mut builder = Builder::new(module);
        builder.translate_instructions(&func.body);

        Self {
            body: builder.stmts,
            decl: FunctionDecl::translate(id, func, module),
        }
    }
}

impl fmt::Display for FunctionDef {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "{} {{", self.decl)?;

        if self.decl.return_ty != Type::Void {
            writeln!(f, "{};", self.decl.return_ty.to_decl_string("L0"))?;
        }

        for stmt in &self.body {
            writeln!(f, "{}", stmt)?;
        }

        if self.decl.return_ty != Type::Void {
            write!(f, "return L0;")?;
        }

        write!(f, "}}")
    }
}