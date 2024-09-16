pub mod ast;
pub use ir_lang as ir;
pub use intermediate::Module as IRModule;

pub use ast as c;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Options {
    pub trace_heap: bool,
    pub trace_rc: bool,
    pub trace_ir: bool,
}

pub fn translate(ir_module: &IRModule, opts: Options) -> c::Module {
    let mut module = c::Module::new(&ir_module.metadata(), opts);
    module.add_ir(ir_module);

    module
}
