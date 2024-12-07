use std::borrow::Cow;
use serde::Deserialize;
use serde::Serialize;
use crate::FunctionID;
use crate::StringID;
use crate::Type;

pub trait RttiProvider {
    fn type_name(&self, ty: &Type) -> Option<Cow<String>>;
    fn dyn_array_type_name(&self, element_ty: &Type) -> Option<Cow<String>>;
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct RuntimeType {
    pub name: Option<StringID>,

    pub release: Option<FunctionID>,
    pub retain: Option<FunctionID>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct DynArrayRuntimeType {
    pub alloc: FunctionID,
    pub length: FunctionID,
}
