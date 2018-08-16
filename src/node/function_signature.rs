use std::fmt;
use node::ToSource;

#[derive(Eq, PartialEq, Clone, Debug, Hash)]
pub struct FunctionSignature<TType> {
    pub return_type: Option<TType>,
    pub arg_types: Vec<TType>,
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