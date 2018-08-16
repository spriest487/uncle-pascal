use std::{
    fmt::{self, Write}
};
use node::{
    FunctionArgModifier,
    FunctionArg,
    Context,
};

#[derive(PartialEq, Clone, Debug, Hash)]
pub struct ExternalName {
    pub shared_lib: Option<String>,
    pub symbol_name: Option<String>,
}

#[derive(PartialEq, Clone, Debug, Hash)]
pub enum FunctionModifier {
    Cdecl,
    Stdcall,
    External(ExternalName),
}

impl fmt::Display for FunctionModifier {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            FunctionModifier::Cdecl => write!(f, "cdecl"),
            FunctionModifier::Stdcall => write!(f, "stdcall"),
            FunctionModifier::External(ExternalName { shared_lib, symbol_name }) => {
                write!(f, "external")?;

                if let Some(shared_lib) = shared_lib {
                    write!(f, " '{}'", shared_lib)?;
                }
                if let Some(name) = symbol_name {
                    write!(f, " name '{}'", name)?;
                }

                Ok(())
            }
        }
    }
}

#[derive(PartialEq, Clone, Debug, Hash)]
pub struct FunctionArgSignature<TType> {
    pub decl_type: TType,
    pub modifier: Option<FunctionArgModifier>,
}

impl<TType> fmt::Display for FunctionArgSignature<TType>
    where TType: fmt::Display
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.modifier {
            Some(modifier) =>
                write!(f, "{} {}", modifier, self.decl_type),
            None =>
                write!(f, "{}", self.decl_type),
        }
    }
}

impl<TContext> From<FunctionArg<TContext>> for FunctionArgSignature<TContext::Type>
    where TContext: Context
{
    fn from(arg: FunctionArg<TContext>) -> Self {
        FunctionArgSignature {
            decl_type: arg.decl_type,
            modifier: arg.modifier,
        }
    }
}

#[derive(PartialEq, Clone, Debug, Hash)]
pub struct FunctionSignature<TType> {
    pub return_type: Option<TType>,
    pub args: Vec<FunctionArgSignature<TType>>,
    pub modifiers: Vec<FunctionModifier>,
}

impl<TType> fmt::Display for FunctionSignature<TType>
    where TType: fmt::Display
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str("function")?;

        if self.args.len() > 0 {
            f.write_char('(')?;
            f.write_str(&self.args.iter()
                .map(|arg_type| {
                    format!("{}", arg_type)
                })
                .collect::<Vec<_>>()
                .join(";"))?;
            f.write_char(')')?;
        }

        if let Some(return_type) = self.return_type.as_ref() {
            write!(f, ": {}", return_type)?;
        }

        for modifier in self.modifiers.iter() {
            write!(f, "; {}", modifier)?;
        }

        Ok(())
    }
}
