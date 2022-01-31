mod annotation;
mod context;
mod result;

pub mod ast;
pub mod ty;

#[cfg(test)]
pub mod test;

pub use self::{annotation::*, context::*, result::*, ty::*};
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
    pub root_ctx: Box<Context>,

    // language builtins
    pub string_class: Box<Symbol>,
    pub disposable_iface: IdentPath,
}

impl Module {
    pub fn typecheck(units: &[pas_syn::ast::Unit<Span>], no_stdlib: bool) -> TypecheckResult<Self> {
        eprintln!("function sig size: {}", std::mem::size_of::<sig::FunctionSig>());
        eprintln!("type size: {}", std::mem::size_of::<Type>());
        eprintln!("type annotation size: {}", std::mem::size_of::<TypeAnnotation>());
        eprintln!("expr size: {}", std::mem::size_of::<ast::Expression>());
        eprintln!("stmt size: {}", std::mem::size_of::<ast::Statement>());
        eprintln!("type list size: {}", std::mem::size_of::<TypeList>());
        eprintln!("ident size: {}", std::mem::size_of::<pas_syn::Ident>());
        eprintln!("ident path size: {}", std::mem::size_of::<IdentPath>());
        eprintln!("span size: {}", std::mem::size_of::<Span>());

        let module_span = Span::zero(units[0].ident.span().file.as_ref().clone());

        let mut root_ctx = Context::root(no_stdlib, module_span);
        let mut typed_units = Vec::new();

        for unit in units {
            typed_units.push(typecheck_unit(&unit, &mut root_ctx)?);
        }

        let string_name = Box::new(context::builtin_string_name());
        let (_, string_ty) = root_ctx
            .find_type(&string_name.qualified)
            .expect("string class must exist");
        let string_class = string_ty
            .clone()
            .as_class()
            .map(|c| Box::new(c.clone()))
            .expect("string class must be a class");

        let disposable_name = Box::new(context::builtin_disposable_name());
        let disposable_iface = root_ctx
            .find_iface(&disposable_name.qualified)
            .expect("disposable interface must exist");

        Ok(Module {
            units: typed_units,
            root_ctx: Box::new(root_ctx),
            string_class,
            disposable_iface,
        })
    }
}
