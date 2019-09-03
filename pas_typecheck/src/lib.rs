pub use self::{annotation::*, context::*, result::*, ty::*};

mod annotation;
mod context;
mod result;

pub mod ast {
    pub use self::{
        block::*, cond::*, ctor::*, expression::*, function::*, iter::*, op::*, statement::*,
        typedecl::*, unit::*,
    };

    mod block;
    mod cond;
    mod ctor;
    mod expression;
    mod function;
    mod iter;
    mod op;
    mod statement;
    mod typedecl;
    mod unit;

    mod prelude {
        pub use pas_common::span::*;
        pub use pas_syn::{
            ast::{self, FunctionParamMod, Visibility},
            ident::*,
            parse::InvalidStatement,
        };

        pub use crate::{annotation::*, ast::*, context::*, result::*, ty::*};
        pub use std::rc::Rc;
    }
}

pub mod ty;

#[cfg(test)]
mod test;

use ast::{typecheck_unit, Interface};
use pas_common::span::*;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct Module {
    pub units: Vec<ast::Unit>,
    pub root_ctx: Context,

    // language builtins
    pub string_class: QualifiedDeclName,
    pub disposable_iface: Rc<Interface>,
}

impl Module {
    pub fn typecheck(units: &[pas_syn::ast::Unit<Span>], no_stdlib: bool) -> TypecheckResult<Self> {
        let mut root_ctx = Context::root(no_stdlib);
        let mut typed_units = Vec::new();

        for unit in units {
            typed_units.push(typecheck_unit(&unit, &mut root_ctx)?);
        }

        let string_name = context::builtin_string_name();
        let (_, string_ty) = root_ctx
            .find_type(&string_name.qualified)
            .expect("string class must exist");
        let string_class = string_ty
            .clone()
            .as_class()
            .expect("string class must be a class");

        let disposable_name = context::builtin_disposable_name();
        let (_, disposable_iface) = root_ctx
            .find_iface(&disposable_name.qualified)
            .expect("disposable interface must exist");

        Ok(Module {
            units: typed_units,
            root_ctx,
            string_class,
            disposable_iface,
        })
    }
}
