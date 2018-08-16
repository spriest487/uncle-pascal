use std::fmt;
use node::{
    ToSource,
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

impl ToSource for FunctionModifier {
    fn to_source(&self) -> String {
        match self {
            FunctionModifier::Cdecl => "cdecl".to_string(),
            FunctionModifier::Stdcall => "stdcall".to_string(),
            FunctionModifier::External(ExternalName { shared_lib, symbol_name }) => {
                let mut parts = vec!["external".to_string()];

                if let Some(shared_lib) = shared_lib {
                    parts.push(format!("'{}'", shared_lib));
                }
                if let Some(name) = symbol_name {
                    parts.push(format!("name '{}'", name))
                }

                parts.join(" ")
            }
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

impl<TType> ToSource for FunctionSignature<TType>
    where TType: ToSource
{
    fn to_source(&self) -> String {
        let mut source = "function".to_string();

        if self.args.len() > 0 {
            source.push('(');
            source.push_str(&self.args.iter()
                .map(|arg_type| {
                    format!("{}", arg_type.to_source())
                })
                .collect::<Vec<_>>()
                .join(";"));
            source.push(')');
        }

        if let Some(return_type) = self.return_type.as_ref() {
            source.push_str(": ");
            source.push_str(&return_type.to_source());
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