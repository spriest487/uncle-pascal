use std::rc::Rc;
use pas_syn::{Ident, IdentPath};
use crate::{FunctionSig, Type};
//
// #[derive(Debug, Clone, Eq, PartialEq, Hash)]
// pub struct ClosureType {
//     pub sig: Rc<FunctionSig>,
//     pub outer_decl: IdentPath,
//     pub id: usize,
//
//     pub captures: Vec<ClosureCapture>,
// }
//
// #[derive(Debug, Clone, Eq, PartialEq, Hash)]
// pub struct ClosureCapture {
//     pub name: Ident,
//     pub ty: Type,
// }