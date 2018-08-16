use std::{
    fmt,
};
use node::{
    ToSource,
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

#[derive(Eq, PartialEq, Clone, Debug, Hash)]
pub struct FunctionSignature<TType> {
    pub return_type: Option<TType>,
    pub arg_types: Vec<TType>,
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

        if self.arg_types.len() > 0 {
            source.push('(');
            source.push_str(&self.arg_types.iter()
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