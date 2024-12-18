mod builder;
mod expr;
pub mod metadata;
mod stmt;
mod pattern;
mod function;
pub mod library_builder;
mod set_flags;

pub use self::function::*;
pub use self::set_flags::*;
use crate::codegen::builder::Builder;
use crate::codegen::expr::*;
use crate::codegen::metadata::*;
use crate::codegen::library_builder::LibraryBuilder;
use crate::codegen::stmt::*;
use crate::ast as syn;
use crate::typ as typ;
pub use ir_lang as ir;
use crate::ast::StructKind;

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct IROptions {
    // insert IR comments indicating RC release/retain operations
    pub annotate_rc: bool,

    // insert source spans for statements and expressions for improved error messaging in the
    // translation/interpreter stage
    pub debug: bool,
}

impl Default for IROptions {
    fn default() -> Self {
        Self {
            annotate_rc: false,

            debug: true,
        }
    }
}

pub fn translate(module: &typ::Module, opts: IROptions) -> ir::Library {
    let metadata = ir::Metadata::new();
    let mut ir_lib = LibraryBuilder::new((*module.root_ctx).clone(), metadata, opts);

    let builtin_disposable = typ::builtin_disposable_iface();

    // make sure frontend builtin types are defined e.g. dynamic array types add implementations
    // to Disposable so need that interface to be defined
    let disposable_iface = {
        let mut builder = Builder::new(&mut ir_lib);
        let disposable_iface = builder.translate_iface(&builtin_disposable);
        builder.finish();

        disposable_iface
    };

    ir_lib.metadata_mut().define_iface(disposable_iface);

    // if String is defined it needs to be defined in the metadata even if it isn't used,
    // for the benefit of the stdlib (it's not defined in the type context with --no-stdlib)
    let string_name = typ::builtin_string_name();
    if let Ok(string_class) = module.root_ctx
        .find_struct_def(&string_name.full_path, StructKind::Class) 
    {
        let name = {
            let mut builder = Builder::new(&mut ir_lib);
            let name = builder.translate_name(&string_name);
            builder.finish();
            name
        };

        ir_lib.metadata_mut().reserve_struct(ir::STRING_ID);
        ir_lib.metadata_mut().declare_struct(ir::STRING_ID, &name);

        let string_def = {
            let mut builder = Builder::new(&mut ir_lib);
            let string_def = builder.translate_class(&string_class);
            builder.finish();
            string_def
        };

        ir_lib.metadata_mut().define_struct(ir::STRING_ID, string_def);
        ir_lib.runtime_type(&ir::Type::Struct(ir::STRING_ID));
    }

    for unit in &module.units {
        ir_lib.translate_unit(&unit.unit);
    }

    ir_lib.finish()
}
