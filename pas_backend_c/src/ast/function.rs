use crate::ast::{Builder, Module, Statement, Type};
use pas_ir::{
    self as ir,
    metadata::{FunctionID, InterfaceID, MethodID},
};
use std::fmt;

#[derive(Debug, Eq, PartialEq, Hash, Copy, Clone)]
pub enum FunctionName {
    // c main function
    Main,

    // init function that all loaded modules append their init code into
    Init,

    ID(FunctionID),
    Method(InterfaceID, MethodID),

    // runtime functions
    RcAlloc,
    RcRetain,
    RcRelease,
    IsImpl,

    // builtins
    IntToStr,
    StrToInt,
    WriteLn,
    ReadLn,
    GetMem,
    FreeMem,
    ArrayLengthInternal,
}

impl fmt::Display for FunctionName {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            FunctionName::Main => write!(f, "main"),
            FunctionName::Init => write!(f, "ModuleInit"),
            FunctionName::ID(id) => write!(f, "Function_{}", id.0),
            FunctionName::Method(iface, method) => write!(f, "Method_{}_{}", iface, method.0),

            FunctionName::RcAlloc => write!(f, "RcAlloc"),
            FunctionName::RcRetain => write!(f, "RcRetain"),
            FunctionName::RcRelease => write!(f, "RcRelease"),
            FunctionName::IsImpl => write!(f, "IsImpl"),

            FunctionName::WriteLn => write!(f, "System_WriteLn"),
            FunctionName::ReadLn => write!(f, "System_ReadLn"),
            FunctionName::IntToStr => write!(f, "System_IntToStr"),
            FunctionName::StrToInt => write!(f, "System_StrToInt"),
            FunctionName::GetMem => write!(f, "System_GetMem"),
            FunctionName::FreeMem => write!(f, "System_FreeMem"),
            FunctionName::ArrayLengthInternal => write!(f, "System_ArrayLengthInternal"),
        }
    }
}

pub struct FunctionDecl {
    pub name: FunctionName,
    pub return_ty: Type,
    pub params: Vec<Type>,

    pub comment: Option<String>,
}

impl FunctionDecl {
    pub fn translate(id: FunctionID, func: &ir::FunctionDef, module: &mut Module) -> Self {
        let name = FunctionName::ID(id);
        let return_ty = Type::from_metadata(&func.return_ty, module);
        let params = func
            .params
            .iter()
            .map(|param| Type::from_metadata(param, module))
            .collect();

        let mut comment = func.debug_name.to_string();
        comment.push_str(": (");
        for (i, arg) in func.params.iter().enumerate() {
            if i > 0 {
                comment.push_str(", ");
            }
            comment.push_str(&arg.to_string());
        }
        comment.push_str(") -> ");
        comment.push_str(&func.return_ty.to_string());

        Self {
            name,
            return_ty,
            params,
            comment: Some(comment),
        }
    }

    pub fn ptr_type(&self) -> Type {
        Type::FunctionPointer {
            return_ty: Box::new(self.return_ty.clone()),
            params: self.params.clone(),
        }
    }
}

impl fmt::Display for FunctionDecl {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(comment) = &self.comment {
            writeln!(f, "/** {} **/", comment)?;
        }

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
    pub fn translate(id: FunctionID, func: &ir::FunctionDef, module: &mut Module) -> Self {
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
