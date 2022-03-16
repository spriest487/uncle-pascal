use crate::ast::{Builder, Expr, InfixOp, Module, Statement, TypeDecl, Type};
use pas_ir::{self as ir, LocalID, metadata::{FunctionID, InterfaceID, MethodID}};
use std::fmt;
use pas_ir::metadata::TypeDefID;

#[derive(Debug, Eq, PartialEq, Hash, Copy, Clone)]
pub enum FunctionName {
    // c main function
    Main,

    // init function that all loaded modules append their init code into
    Init,

    // function to initialize an external symbol at runtime
    LoadSymbol,

    ID(FunctionID),
    Method(InterfaceID, MethodID),
    MethodWrapper(InterfaceID, MethodID, TypeDefID),

    // runtime functions
    RcAlloc,
    RcRetain,
    RcRelease,
    IsImpl,
    Raise,

    // builtins
    Int8ToStr,
    ByteToStr,
    Int16ToStr,
    UInt16ToStr,
    IntToStr,
    UInt32ToStr,
    Int64ToStr,
    UInt64ToStr,
    NativeIntToStr,
    NativeUIntToStr,

    StrToInt,
    WriteLn,
    ReadLn,
    GetMem,
    FreeMem,

    ArrayLengthInternal,
    ArraySetLengthInternal,
}

impl fmt::Display for FunctionName {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            FunctionName::Main => write!(f, "main"),
            FunctionName::Init => write!(f, "ModuleInit"),
            FunctionName::LoadSymbol => write!(f, "LoadSymbol"),

            FunctionName::ID(id) => write!(f, "Function_{}", id.0),
            FunctionName::Method(iface, method) => write!(f, "Method_{}_{}", iface, method.0),
            FunctionName::MethodWrapper(iface, method, self_ty) => write!(f, "Method_{}_{}_Wrapper_{}", iface, method.0, self_ty),

            FunctionName::RcAlloc => write!(f, "RcAlloc"),
            FunctionName::RcRetain => write!(f, "RcRetain"),
            FunctionName::RcRelease => write!(f, "RcRelease"),
            FunctionName::IsImpl => write!(f, "IsImpl"),
            FunctionName::Raise => write!(f, "Raise"),

            FunctionName::WriteLn => write!(f, "System_WriteLn"),
            FunctionName::ReadLn => write!(f, "System_ReadLn"),
            FunctionName::StrToInt => write!(f, "System_StrToInt"),
            FunctionName::GetMem => write!(f, "System_GetMem"),
            FunctionName::FreeMem => write!(f, "System_FreeMem"),

            FunctionName::Int8ToStr => write!(f, "System_Int8ToStr"),
            FunctionName::ByteToStr => write!(f, "System_ByteToStr"),
            FunctionName::Int16ToStr => write!(f, "System_Int16ToStr"),
            FunctionName::UInt16ToStr => write!(f, "System_UInt16ToStr"),
            FunctionName::IntToStr => write!(f, "System_IntToStr"),
            FunctionName::UInt32ToStr => write!(f, "System_UInt32ToStr"),
            FunctionName::Int64ToStr => write!(f, "System_Int64ToStr"),
            FunctionName::UInt64ToStr => write!(f, "System_UInt64ToStr"),
            FunctionName::NativeIntToStr => write!(f, "System_NativeIntToStr"),
            FunctionName::NativeUIntToStr => write!(f, "System_NativeUIntToStr"),

            FunctionName::ArrayLengthInternal => write!(f, "System_ArrayLengthInternal"),
            FunctionName::ArraySetLengthInternal => write!(f, "System_ArraySetLengthInternal"),
        }
    }
}

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
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
            write!(f, "{}", Statement::Return(Expr::Local(LocalID(0))))?;
        }

        write!(f, "}}")
    }
}

pub struct FfiFunction {
    pub decl: FunctionDecl,
    pub symbol: String,
    pub src: String,
}

impl FfiFunction {
    pub fn translate(id: FunctionID, func_ref: &ir::ExternalFunctionRef, module: &mut Module) -> Self {
        let return_ty = Type::from_metadata(&func_ref.return_ty, module);
        let mut params = Vec::new();
        for param in &func_ref.params {
            params.push(Type::from_metadata(param, module));
        }

        let decl = FunctionDecl {
            name: FunctionName::ID(id),
            return_ty,
            params,
            comment: Some(format!("external func {}::{}", func_ref.src, func_ref.symbol)),
        };

        FfiFunction {
            decl,
            src: func_ref.src.clone(),
            symbol: func_ref.symbol.clone(),
        }
    }

    pub fn init_statement(&self) -> Statement {
        Statement::Expr(Expr::InfixOp {
            lhs: Expr::Function(self.decl.name).into(),
            op: InfixOp::Assign,
            rhs: Expr::Call {
                func: Expr::Function(FunctionName::LoadSymbol).into(),
                args: vec![
                    Expr::LitCString(self.src.clone()),
                    Expr::LitCString(self.symbol.clone()),
                ],
            }.into(),
        })
    }

    pub fn func_ptr_decl(&self) -> String {
        self.decl.ptr_type().to_decl_string(&self.decl.name)
    }
}

#[derive(Clone, Eq, PartialEq, Hash)]
pub struct FuncAliasDef {
    pub decl: TypeDecl,

    pub param_tys: Vec<Type>,
    pub return_ty: Type,

    pub comment: Option<String>,
}

impl fmt::Display for FuncAliasDef {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(comment) = &self.comment {
            write!(f, "/** {} */", comment)?;
        }

        let ptr_type = self.to_pointer_type();
        let decl_string = ptr_type.to_decl_string(&self.decl.name.to_string());

        write!(f, "typedef {}", decl_string)?;

        Ok(())
    }
}

impl FuncAliasDef {
    pub fn to_pointer_type(&self) -> Type {
        Type::FunctionPointer {
            return_ty: Box::new(self.return_ty.clone()),
            params: self.param_tys.clone(),
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct FuncAliasID(pub TypeDefID);

impl fmt::Display for FuncAliasID {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "FunctionAlias_{}", self.0.0)
    }
}