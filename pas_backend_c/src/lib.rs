use pas_ir::{self as ir};

mod ast;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Options {
    pub trace_heap: bool,
}

pub fn translate(ir_module: &ir::Module, opts: Options) -> ast::Module {
    let mut module = ast::Module::new(&ir_module.metadata, opts);
    module.add_ir(ir_module);

    module
}
