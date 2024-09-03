use std::path::PathBuf;
use std::str::FromStr;
use structopt::StructOpt;

#[derive(StructOpt, Debug, Clone)]
pub struct Opts {
    #[structopt(name = "FILE", parse(from_os_str))]
    pub search_path: PathBuf,
    
    #[structopt(long = "target", parse(from_os_str))]
    pub target_path: PathBuf,

    #[structopt(long = "compiler", parse(from_os_str))]
    pub compiler: PathBuf,
    
    #[structopt(long = "clang-debug", default_value = "false", parse(try_from_str))]
    pub clang_debug: bool,

    #[structopt(long = "clang-codeview", default_value = "false", parse(try_from_str))]
    pub clang_codeview: bool,

    #[structopt(long = "error-continue", default_value = "false", parse(try_from_str))]
    pub error_continue: bool,

    #[structopt(long = "exec", default_value = "interpret", parse(try_from_str))]
    pub execution_method: ExecutionMethod, 
}

#[derive(Debug, Copy, Clone)]
pub enum ExecutionMethod {
    Interpret,
    Clang,
}

impl FromStr for ExecutionMethod {
    type Err = String;

    fn from_str(s: &str) -> Result<ExecutionMethod, String> {
        match s {
            "clang" => Ok(ExecutionMethod::Clang),
            "interpret" => Ok(ExecutionMethod::Interpret),
            _ => Err(s.to_string())
        }
    }
}
