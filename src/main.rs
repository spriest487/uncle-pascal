extern crate regex;
extern crate getopts;

use std::rc::*;
use std::fmt;
use std::io::{self, Read, Write};
use std::process;
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

enum CompileError {
    TokenizeError(tokenizer::IllegalToken),
    ParseError(syntax::ParseError),
    SemanticError(semantic::SemanticError),
    WriterError(fmt::Error),
    IOError(io::Error),
}

impl fmt::Display for CompileError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &CompileError::TokenizeError(ref token) => write!(f, "{}", token),
            &CompileError::ParseError(ref err) => write!(f, "{}", err),
            &CompileError::SemanticError(ref err) => write!(f, "{}", err),
            &CompileError::WriterError(ref err) => write!(f, "{}", err),
            &CompileError::IOError(ref err) => write!(f, "{}", err),
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
    let filename = path.as_ref()
        .file_name()
        .map(OsStr::to_string_lossy)
        .map(|s| String::from(s))
        .unwrap();

    let mut file = fs::File::open(path)?;

    let mut source = String::new();
    file.read_to_string(&mut source)?;

    let tokens = tokenizer::tokenize(&filename, &source)?;

    Ok(tokens)
}

struct ProgramModule {
    program: semantic::Program,
    units: Vec<semantic::Unit>,
}

fn compile_program(program_path: &Path) -> Result<ProgramModule, CompileError> {
    let tokens = load_source(program_path)?;

    let source_dir = program_path.parent()
        .unwrap()
        .canonicalize()?;

    let parsed_program = syntax::Program::parse(tokens.into_iter(), &empty_context())?;

    let mut loaded_units = Vec::new();
    let mut loaded_unit_scopes = HashMap::new();

    for unit_ref in parsed_program.uses.iter() {
        let unit_id = unit_ref.name.to_string();
        if unit_id == "System" {
            continue;
        }

        if !loaded_unit_scopes.contains_key(&unit_id) {
            let unit_path = source_dir.join(format!("{}.pas", unit_id));

            println!("Compiling unit `{}` in `{}`...",
                     unit_id,
                     unit_path.to_string_lossy());

            let unit_source = load_source(unit_path)?;
            let parsed_unit = syntax::Unit::parse(unit_source, &empty_context())?;

            let (unit, unit_scope) = semantic::Unit::annotate(&parsed_unit,
                                                              Scope::default())?;

            loaded_unit_scopes.insert(unit_id, unit_scope);
            loaded_units.push(unit);
        }
    }

    let program_scope = loaded_unit_scopes.into_iter()
        .fold(Scope::default(), |scope, (unit_id, unit_scope)| {
            scope.with_child(unit_id, unit_scope)
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

fn pas_to_c(in_path: &str,
            out_dir: Option<&str>,
            clang: bool) -> Result<(), CompileError> {
    let src_path = PathBuf::from(in_path)
        .canonicalize()?;

    let out_path = out_dir.map(|p| PathBuf::from(p))
        .unwrap_or_else(|| current_dir().unwrap());

    let module = compile_program(&src_path)?;
    let c_unit = target_c::write_c(&module.program)?;

    if clang {
        invoke_clang(&c_unit, &src_path)?;
    } else {
        let out_file_path = out_path
            .join(format!("{}.c", module.program.name));
            //.canonicalize()?;

        println!("Writing output to `{}`...", out_file_path.to_string_lossy());

        let mut out_file = fs::File::create(out_file_path)?;
        out_file.write_all(c_unit.as_bytes())?;

        println!("Done!");
    }
    Ok(())
}

fn main() {
    let args = std::env::args().collect::<Vec<String>>();
    let program = args[0].clone();

    let mut opts = getopts::Options::new();
    opts.optflag("c", "invoke-clang", "invoke clang to compile the generated C unit");

    //TODO
    let out_dir = None;

    match opts.parse(&args[1..]) {
        Ok(matched) => {
            if matched.free.len() != 1 {
                print_usage(&program, opts);
                std::process::exit(1);
            } else {
                let clang = matched.opt_present("c");

                match pas_to_c(&matched.free[0], out_dir, clang) {
                    Err(err) => {
                        println!("error: {}", err);
                        std::process::exit(1);
                    }
                    _ => ()
                }
            }
        }
        Err(err) => {
            println!("error: {}", err);
            print_usage(&program, opts);

            std::process::exit(1);
        }
    }
}

fn invoke_clang(c_src: &str, source_path: &Path) -> Result<(), CompileError> {
    let os_ext = if cfg!(windows) {
        "exe"
    } else {
        ""
    };

    let target_path = PathBuf::from(source_path)
        .with_extension(os_ext);

    let name = target_path.file_name()
        .unwrap()
        .to_string_lossy();

    let mut clang = process::Command::new("clang")
        .arg("-x").arg("c")
        .arg("-o").arg(name.to_string())
        .arg("-")
        .stdout(process::Stdio::inherit())
        .stdin(process::Stdio::piped())
        .spawn()?;

    {
        let clang_in = clang.stdin.as_mut().unwrap();
        clang_in.write_all(c_src.to_owned().as_bytes())?;
    }
    clang.wait()?;

    Ok(())
}
