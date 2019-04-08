use pas_ir::{self as ir};

mod ast;

pub fn translate(ir_module: &ir::Module) -> ast::Module {
    let mut module = ast::Module::new(&ir_module.metadata);
    module.add_ir(ir_module);

    module
}
