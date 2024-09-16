use common::span::Span;
use crate::{NamePath, Type};

#[derive(Clone, Debug)]
pub struct VariantCase {
    pub name: String,
    pub ty: Option<Type>,
    pub rc: bool,
}

#[derive(Clone, Debug)]
pub struct VariantDef {
    pub name: NamePath,
    pub cases: Vec<VariantCase>,

    pub src_span: Option<Span>,
}
