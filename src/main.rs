extern crate regex;
extern crate getopts;

use std::rc::*;
use std::fmt;
use std::io::{self, Read};
use std::fs;
use std::path::*;
use std::collections::HashMap;
use std::ffi::OsStr;
use std::env::current_dir;
use semantic::scope::Scope;

mod keywords;
mod types;
mod operators;
mod tokens;
mod tokenizer;
mod syntax;
mod target_c;
mod semantic;
mod node;
mod source;

pub enum CompileError {
    TokenizeError(tokenizer::IllegalToken),
    ParseError(syntax::ParseError),
    SemanticError(semantic::SemanticError),
    WriterError(fmt::Error),
    IOError(io::Error),
}

impl fmt::Display for CompileError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &CompileError::TokenizeError(ref token) => write!(f, "failed to read file: {}", token),
            &CompileError::ParseError(ref err) => write!(f, "syntax error: {}", err),
            &CompileError::SemanticError(ref err) => write!(f, "semantic error: {}", err),
            &CompileError::WriterError(ref err) => write!(f, "output error: {}", err),
            &CompileError::IOError(ref err) => write!(f, "io error: {}", err),
        }
    }
}

impl From<tokenizer::IllegalToken> for CompileError {
    fn from(err: tokenizer::IllegalToken) -> Self {
        CompileError::TokenizeError(err)
    }
}

impl From<syntax::ParseError> for CompileError {
    fn from(err: syntax::ParseError) -> Self {
        CompileError::ParseError(err)
    }
}

impl From<semantic::SemanticError> for CompileError {
    fn from(err: semantic::SemanticError) -> Self {
        CompileError::SemanticError(err)
    }
}

impl From<fmt::Error> for CompileError {
    fn from(err: fmt::Error) -> Self {
        CompileError::WriterError(err)
    }
}

impl From<io::Error> for CompileError {
    fn from(err: io::Error) -> Self { CompileError::IOError(err) }
}

fn empty_context() -> source::Token {
    source::Token {
        token: Rc::from(tokens::Keyword(keywords::Program)),
        location: source::Location::new("", 0, 0),
    }
}

fn load_source<TPath: AsRef<Path>>(path: TPath) -> Result<Vec<source::Token>, CompileError> {
    let mut file = fs::File::open(&path)?;

    let mut source = String::new();
    file.read_to_string(&mut source)?;

    let display_filename = path.as_ref()
        .file_name()
        .map(OsStr::to_string_lossy)
        .map(|s| String::from(s))
        .unwrap_or_else(|| "(unknown)".to_owned());

    let tokens = tokenizer::tokenize(&display_filename, &source)?;

    Ok(tokens)
}

pub struct ProgramModule {
    pub program: semantic::Program,
    pub units: Vec<semantic::Unit>,
}

fn pretty_path(path: &Path) -> String {
    let canon = if path.exists() {
        path.canonicalize()
            .map(|canon_path| canon_path.to_string_lossy().into_owned())
            .unwrap_or_else(|_| "(unknown)".to_owned())
    } else {
        path.to_string_lossy().into_owned()
    };

    if cfg!(windows) && canon.starts_with(r"\\?\") {
        canon[4..].to_owned()
    } else {
        canon
    }
}

fn compile_program(program_path: &Path) -> Result<ProgramModule, CompileError> {
    let tokens = load_source(program_path)?;

    let source_dir = program_path.canonicalize()?
        .parent()
        .ok_or_else(|| {
            let msg = format!("unable to resolve source directory from source path `{}`", program_path.to_string_lossy());
            io::Error::new(io::ErrorKind::NotFound, msg)
        })?
        .canonicalize()?;

    let parsed_program = syntax::Program::parse(tokens.into_iter(), &empty_context())?;

    let mut loaded_units = Vec::new();
    let mut loaded_unit_scopes = HashMap::new();

    for unit_ref in parsed_program.uses.iter() {
        let unit_id = unit_ref.name.to_string();

        if !loaded_unit_scopes.contains_key(&unit_id) {
            let unit_path = source_dir.join(format!("{}.pas", unit_id));

            println!("Compiling unit `{}` in `{}`...",
                     unit_id,
                     pretty_path(&unit_path));

            let unit_source = load_source(unit_path)?;
            let parsed_unit = syntax::Unit::parse(unit_source, &empty_context())?;

            let (unit, unit_scope) = semantic::Unit::annotate(&parsed_unit,
                                                              Scope::default())?;

            loaded_unit_scopes.insert(unit_id, unit_scope);
            loaded_units.push(unit);
        }
    }

    let program_scope = loaded_units.iter()
        .fold(Scope::default(), |scope, unit| {
            let unit_scope = loaded_unit_scopes.remove(&unit.name).unwrap();
            scope.with_all(unit_scope)
        });

    let program = semantic::Program::annotate(&parsed_program, program_scope)?;

    Ok(ProgramModule {
        program,
        units: loaded_units,
    })
}

fn print_usage(program: &str, opts: getopts::Options) {
    let brief = format!("Usage: {} FILE [options]", program);
    print!("{}", opts.usage(&brief));
}

fn main() {
    let args = std::env::args().collect::<Vec<String>>();
    let program = args[0].clone();

    let mut opts = getopts::Options::new();
    opts.optopt("o", "outdir", "Output file (use .c extension to emit C intermediate files)", "");

    match opts.parse(&args[1..]) {
        Ok(matched) => {
            if matched.free.len() != 1 {
                print_usage(&program, opts);
                std::process::exit(1);
            } else {
                let out_file = matched.opt_str("o")
                    .map(|out_dir_str| PathBuf::from(out_dir_str))
                    .unwrap_or_else(|| current_dir().unwrap());

                let src_path = &matched.free[0];

                let build_result = compile_program(&PathBuf::from(src_path))
                    .and_then(|module| target_c::pas_to_c(&module, &out_file));

                if let Err(err) = build_result {
                    println!("{}", err);
                    std::process::exit(1);
                }
            }
        }
        Err(err) => {
            println!("{}", err);
            print_usage(&program, opts);

            std::process::exit(1);
        }
    }
}

