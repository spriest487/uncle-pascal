use semantic::{
    Unit,
    Program,
};
use opts::CompileOptions;

pub struct ProgramModule {
    pub program: Program,
    pub units: Vec<Unit>,
    pub opts: CompileOptions,
}