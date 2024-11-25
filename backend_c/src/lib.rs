pub mod ast;
pub use ir_lang as ir;

pub use ast as c;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Options {
    pub trace_heap: bool,
    pub trace_rc: bool,
    pub trace_ir: bool,
}

pub fn translate(ir_module: &ir::Library, opts: Options) -> c::CompilationUnit {
    let mut module = c::CompilationUnit::new(&ir_module.metadata(), opts);
    module.add_lib(ir_module);

    module
}
