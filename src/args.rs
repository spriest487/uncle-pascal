use std::{
    path::PathBuf,
};
use structopt::*;
use common::LanguageMode;

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

    #[structopt(long="define", short = "d")]
    pub define_syms: Vec<String>,

    #[structopt(long="mode", short="m", default_value = "default", parse(try_from_str = parse_lang_mode))]
    pub lang_mode: LanguageMode,

    /// additional units to compile
    #[structopt(long = "units", short = "u")]
    pub units: Vec<String>,

    /// source dir for unit source files
    #[structopt(long = "search-dir", short = "s", parse(from_os_str))]
    pub search_dirs: Vec<PathBuf>,

    /// target stage. intermediate targets other than `interpret` will cause
    /// compilation to stop at that stage and dump the output.
    #[structopt(short = "t", long = "target", default_value = "interpret", parse(try_from_str = parse_target))]
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

    /// print frontend backtrace on compilation failure
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
    Preprocessed,
    SyntaxAst,
    TypecheckAst,
    Intermediate,
    Interpret,
}

fn parse_target(s: &str) -> Result<Target, String> {
    match s {
        "i" | "interpret" => Ok(Target::Interpret),
        "ir" | "intermediate" => Ok(Target::Intermediate),
        "p" | "parse" => Ok(Target::SyntaxAst),
        "t" | "typecheck" => Ok(Target::TypecheckAst),
        "pp" | "preprocess" => Ok(Target::Preprocessed),
        _ => Err(format!("invalid output kind: {}", s)),
    }
}

fn parse_lang_mode(s: &str) -> Result<LanguageMode, String> {
    match s {
        "delphi" | "Delphi" => Ok(LanguageMode::Delphi),
        "fpc" | "FPC" => Ok(LanguageMode::Fpc),
        "default" | "Default" => Ok(LanguageMode::Default),
        _ => Err(format!("invalid language mode: {}", s)),
    }
}
