pub use self::{annotation::*, context::*, result::*, ty::*};

mod annotation;
mod context;
mod result;

pub mod ast {
    pub use self::{
        block::*, call::*, cond::*, ctor::*, expression::*, function::*, iter::*, op::*,
        raise::*, statement::*, typedecl::*, unit::*,
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
    mod call;
    mod raise;
    pub mod const_eval;

    mod prelude {
        pub use pas_common::span::*;
        pub use pas_syn::{
            ast::{self, FunctionParamMod, Visibility},
            ident::*,
            parse::InvalidStatement,
        };

        pub use crate::{annotation::*, ast::*, context::*, result::*, ty::*};
        pub use std::rc::Rc;

        pub use const_eval::ConstEval;
    }
}


pub mod ty;

#[cfg(test)]
pub mod test;

use ast::typecheck_unit;
use pas_common::span::*;
use pas_syn::IdentPath;

#[derive(Debug, Clone)]
pub struct ModuleUnit {
    pub unit: ast::Unit,
    pub context: Context,
}

#[derive(Debug, Clone)]
pub struct Module {
    pub units: Vec<ModuleUnit>,
    pub root_ctx: Context,

    // language builtins
    pub string_class: Symbol,
    pub disposable_iface: IdentPath,
}

impl Module {
    pub fn typecheck(units: &[pas_syn::ast::Unit<Span>], no_stdlib: bool) -> TypecheckResult<Self> {
        let module_span = Span::zero(units[0].ident.span().file.as_ref().clone());

        let mut root_ctx = Context::root(no_stdlib, module_span);
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
            .expect("string class must be a class")
            .clone();

        let disposable_name = context::builtin_disposable_name();
        let disposable_iface = root_ctx
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
