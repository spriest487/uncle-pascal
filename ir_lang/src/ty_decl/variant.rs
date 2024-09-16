use serde::{Deserialize, Serialize};
use common::span::Span;
use crate::{NamePath, Type};

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct VariantCase {
    pub name: String,
    pub ty: Option<Type>,
    pub rc: bool,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct VariantDef {
    pub name: NamePath,
    pub cases: Vec<VariantCase>,

    pub src_span: Option<Span>,
}
