use std::fmt;

pub use self::function::*;
pub use self::module::*;
use crate::builder::Builder;
use crate::expr::*;
use crate::metadata::*;
use crate::module_builder::ModuleBuilder;
use crate::stmt::*;
use frontend::ast as syn;
use frontend::typecheck as typ;
pub use ir_lang as ir;
use ir_lang::InstructionFormatter;

mod builder;
mod expr;
pub mod metadata;
mod module;
mod stmt;
mod pattern;
mod function;
pub mod module_builder;

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct IROptions {
    // insert IR comments indicating scoped lifetimes
    pub annotate_scopes: bool,

    // insert IR comments indicating RC release/retain operations
    pub annotate_rc: bool,

    // insert source spans for statements and expressions for improved error messaging in the
    // translation/interpreter stage
    pub debug_info: bool,
}

impl Default for IROptions {
    fn default() -> Self {
        Self {
            annotate_scopes: false,
            annotate_rc: false,

            debug_info: true,
        }
    }
}

fn write_instruction_list(
    f: &mut fmt::Formatter,
    metadata: &ir::Metadata,
    instructions: &[ir::Instruction],
) -> fmt::Result {
    let num_len = instructions.len().to_string().len();

    let formatter = ir::StatefulIndentedFormatter::new(metadata, 4);

    for (i, instruction) in instructions.iter().enumerate() {
        write!(f, "{:>width$}|", i, width = num_len)?;
        formatter.format_instruction(instruction, f)?;
        writeln!(f)?;
    }

    Ok(())
}

pub fn translate(module: &typ::Module, opts: IROptions) -> Module {
    let metadata = ir::Metadata::new();
    let mut ir_module = ModuleBuilder::new((*module.root_ctx).clone(), metadata, opts);

    let builtin_disposable = typ::builtin_disposable_iface();

    // make sure frontend builtin types are defined e.g. dynamic array types add implementations
    // to Disposable so need that interface to be defined
    let disposable_iface = {
        let mut builder = Builder::new(&mut ir_module);
        let disposable_iface = builder.translate_iface(&builtin_disposable);
        builder.finish();

        disposable_iface
    };

    ir_module.metadata_mut().define_iface(disposable_iface);

    // if String is defined it needs to be defined in the metadata even if it isn't used,
    // for the benefit of the stdlib (it's not defined in the type context with --no-stdlib)
    let string_name = typ::builtin_string_name();
    if let Ok(string_class) = module.root_ctx.find_struct_def(&string_name.qualified) {
        let name = {
            let mut builder = Builder::new(&mut ir_module);
            let name = builder.translate_name(&string_name);
            builder.finish();
            name
        };

        ir_module.metadata_mut().reserve_struct(ir::STRING_ID);
        ir_module.metadata_mut().declare_struct(ir::STRING_ID, &name);

        let string_def = {
            let mut builder = Builder::new(&mut ir_module);
            let string_def = builder.translate_class(&string_class);
            builder.finish();
            string_def
        };

        ir_module.metadata_mut().define_struct(ir::STRING_ID, string_def);
        ir_module.runtime_type(&ir::Type::Struct(ir::STRING_ID));
    }

    for unit in &module.units {
        ir_module.translate_unit(&unit.unit);
    }

    ir_module.build()
}
