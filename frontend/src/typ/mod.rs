mod annotation;
mod context;
mod result;

pub mod ast;
pub mod ty;

#[cfg(test)]
pub mod test;

pub use self::annotation::*;
pub use self::context::*;
pub use self::result::*;
pub use self::ty::*;
use ast::typecheck_unit;
use common::span::*;
use crate::ast as syn;
use crate::ast::{IdentPath, UnitKind};

#[derive(Debug, Clone)]
pub struct ModuleUnit {
    pub unit: ast::Unit,
    pub context: Context,
}

#[derive(Debug, Clone)]
pub struct Module {
    pub units: Vec<ModuleUnit>,
    pub root_ctx: Box<Context>,
}

impl Module {
    pub fn typecheck(units: &[syn::Unit<Span>]) -> TypeResult<Self> {
        // eprintln!("function sig size: {}", std::mem::size_of::<sig::FunctionSig>());
        // eprintln!("type size: {}", std::mem::size_of::<Type>());
        // eprintln!("type annotation size: {}", std::mem::size_of::<TypeAnnotation>());
        // eprintln!("expr size: {}", std::mem::size_of::<ast::Expression>());
        // eprintln!("stmt size: {}", std::mem::size_of::<ast::Statement>());
        // eprintln!("type list size: {}", std::mem::size_of::<TypeList>());
        // eprintln!("ident size: {}", std::mem::size_of::<frontend::Ident>());
        // eprintln!("ident path size: {}", std::mem::size_of::<IdentPath>());
        // eprintln!("span size: {}", std::mem::size_of::<Span>());

        let module_span = Span::zero(units[0].ident.span().file.as_ref().clone());

        let mut root_ctx = Context::root(module_span);
        let mut typed_units = Vec::new();
        
        // if the System unit wasn't included in the compilation, add an empty version of it,
        // mostly to ensure the usual checks for defined symbols run and fail as expected
        let system_ident = builtin_ident(SYSTEM_UNIT_NAME);
        let has_sys_unit = units
            .iter()
            .any(|unit| unit.ident.len() == 1 && *unit.ident.last() == system_ident);

        if !has_sys_unit {
            let system = syn::Unit {
                kind: UnitKind::Unit,
                ident: IdentPath::from(system_ident),
                init: Vec::new(),
                iface_decls: Vec::new(),
                impl_decls: Vec::new(),
            };
            typed_units.push(typecheck_unit(&system, &mut root_ctx)?);
        }

        for unit in units {
            typed_units.push(typecheck_unit(&unit, &mut root_ctx)?);
        }

        Ok(Module {
            units: typed_units,
            root_ctx: Box::new(root_ctx),
        })
    }
}
