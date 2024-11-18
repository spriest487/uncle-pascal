use crate::ast::IdentPath;
use crate::typ::Type;
use crate::IntConstant;
use derivative::Derivative;
use std::fmt;

pub const MAX_FLAGS_BITS: usize = 256;

#[derive(Derivative, Clone, Eq)]
#[derivative(Debug, PartialEq, Hash)]
pub struct SetType {
    pub name: Option<IdentPath>,
    pub item_type: Type,

    pub min: IntConstant,
    pub max: IntConstant,
}

impl SetType {
    pub fn flags_type_bits(&self) -> usize {
        // only support one size for now
        MAX_FLAGS_BITS
    }
}

impl fmt::Display for SetType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(name) = &self.name {
            write!(f, "{}", name)
        } else {
            write!(f, "set of [{}..{}]", self.min, self.max)
        }
    }
}

// #[derive(Derivative, Clone, Eq)]
// #[derivative(Debug, PartialEq, Hash)]
// pub struct SetValue {
//     pub indices: Vec<usize>,
//     pub set_type: Rc<SetType>,
//     
//     #[derivative(Debug = "ignore")]
//     #[derivative(PartialEq = "ignore")]
//     #[derivative(Hash = "ignore")]
//     pub span: Span,
// }
// 
// impl From<SetValue> for Typed {
//     fn from(value: SetValue) -> Self {
//         Typed::Set(Rc::new(value))
//     }
// }
