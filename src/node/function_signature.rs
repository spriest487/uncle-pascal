use std::{
    fmt,
};
use node::{
    ToSource,
    FunctionArgModifier,
};

#[derive(Eq, PartialEq, Clone, Debug, Hash)]
pub enum FunctionModifier {
    Cdecl,
    Stdcall,
}

impl ToSource for FunctionModifier {
    fn to_source(&self) -> String {
        match self {
            FunctionModifier::Cdecl => "cdecl".to_string(),
            FunctionModifier::Stdcall => "stdcall".to_string(),
        }
    }
}

impl fmt::Display for FunctionModifier {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(&self.to_source())
    }
}

#[derive(PartialEq, Clone, Debug, Hash)]
pub struct FunctionArgSignature<TType> {
    pub decl_type: TType,
    pub modifier: Option<FunctionArgModifier>,
}

impl<TType> ToSource for FunctionArgSignature<TType>
    where TType: ToSource
{
    fn to_source(&self) -> String {
        match self.modifier {
            Some(modifier) =>
                format!("{} {}", modifier.to_source(), self.decl_type.to_source()),
            None =>
                self.decl_type.to_source(),
        }
    }
}

#[derive(PartialEq, Clone, Debug, Hash)]
pub struct FunctionSignature<TType> {
    pub return_type: Option<TType>,
    pub args: Vec<FunctionArgSignature<TType>>,
    pub modifiers: Vec<FunctionModifier>,
}

impl<TType> ToSource for FunctionSignature<TType>
    where TType: ToSource
{
    fn to_source(&self) -> String {
        let mut source = String::new();
        source.push_str(if self.return_type.is_some() {
            "function"
        } else {
            "procedure"
        });

        if self.args.len() > 0 {
            source.push('(');
            source.push_str(&self.args.iter()
                .enumerate()
                .map(|(arg_index, arg_type)| {
                    format!("arg{}: {}", arg_index, arg_type.to_source())
                })
                .collect::<Vec<_>>()
                .join(";"));
            source.push(')');
        }

        for modifier in self.modifiers.iter() {
            source.push_str("; ");
            source.push_str(&modifier.to_source());
        }

        source
    }
}

impl<TType> fmt::Display for FunctionSignature<TType>
    where TType: ToSource
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(&self.to_source())
    }
}