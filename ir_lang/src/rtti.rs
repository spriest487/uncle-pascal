use crate::FunctionID;
use crate::StringID;
use crate::Type;
use serde::Deserialize;
use serde::Serialize;

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct RuntimeType {
    pub name: Option<StringID>,
    
    pub methods: Vec<RuntimeMethod>,

    pub release: Option<FunctionID>,
    pub retain: Option<FunctionID>,
}

impl RuntimeType {
    pub fn new(name: Option<StringID>) -> Self {
        Self {
            name,
            
            methods: Vec::new(),
            
            release: None,
            retain: None,
        }
    }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct DynArrayRuntimeType {
    pub alloc: FunctionID,
    pub length: FunctionID,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct RuntimeMethod {
    pub name: StringID,
    
    pub function: FunctionID,
    
    pub result_ty: Type,
    pub params: Vec<Type>,
}
