use std::{
    path::PathBuf,
    str::FromStr,
};
use structopt::*;

#[derive(StructOpt, Debug)]
pub struct Args {
    /// source file of program/library main unit
    #[structopt(name = "FILE", parse(from_os_str))]
    pub file: PathBuf,

    /// output file
    /// If the output file extension matches a backend, the output from that backend will be written
    /// to this path.
    /// If no output path is provided the interpreter will be invoked.
    #[structopt(name = "OUTPUT", short = "o", parse(from_os_str))]
    pub output: Option<PathBuf>,

    #[structopt(name="define", short = "d")]
    pub define_syms: Vec<String>,

    /// additional units to compile
    #[structopt(name = "units", short = "u")]
    pub units: Vec<String>,

    /// source dir for unit source files
    #[structopt(name = "search-dir", short = "s", parse(from_os_str))]
    pub search_dirs: Vec<PathBuf>,

    /// target stage. intermediate targets other than `interpret` will cause
    /// compilation to stop at that stage and dump the output.
    #[structopt(short = "t", long = "target", default_value = "interpret")]
    pub target: Target,

    /// interpreter: log RC heap usage
    #[structopt(long = "trace-heap")]
    pub trace_heap: bool,

    /// interpreter: log RC retain/release operations
    #[structopt(long = "trace-rc")]
    pub trace_rc: bool,

    /// interpreter: log executed IR instructions
    #[structopt(long = "trace-ir")]
    pub trace_ir: bool,

    /// print compiler backtrace on compilation failure
    #[structopt(long = "backtrace", short = "bt")]
    pub backtrace: bool,

    #[structopt(long = "verbose", short = "v")]
    pub verbose: bool,

    #[structopt(long = "ir-debug")]
    pub ir_debug: bool,

    #[structopt(long = "ir-scopes")]
    pub ir_annotate_scopes: bool,
}

#[derive(Debug, Ord, PartialOrd, Eq, PartialEq, Copy, Clone)]
pub enum Target {
    Interpret,
    Intermediate,
    SyntaxAst,
    TypecheckAst,
    Preprocessed,
}

impl FromStr for Target {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, String> {
        match s {
            "i" | "interpret" => Ok(Target::Interpret),
            "ir" | "intermediate" => Ok(Target::Intermediate),
            "p" | "parse" => Ok(Target::SyntaxAst),
            "t" | "typecheck" => Ok(Target::TypecheckAst),
            "pp" | "preprocess" => Ok(Target::Preprocessed),
            _ => Err(format!("invalid output kind: {}", s)),
        }
    }
}