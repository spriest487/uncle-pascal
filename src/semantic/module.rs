use std::rc::Rc;
use linked_hash_map::LinkedHashMap;

use semantic::{
    Unit,
    Program,
    Scope,
};
use opts::CompileOptions;

pub struct ModuleUnit {
    pub unit: Unit,
    pub global_scope: Rc<Scope>,
}

pub struct ProgramModule {
    pub program: Program,
    pub global_scope: Rc<Scope>,

    pub units: LinkedHashMap<String, ModuleUnit>,
    pub opts: CompileOptions,
}