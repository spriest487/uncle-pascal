extern crate regex;
extern crate getopts;

use std::rc::*;
use std::fmt;
use std::io::{self, Read, Write};
use std::process;
use std::fs;
use std::path;

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

fn compile(filename: &str, source: &str) -> Result<String, CompileError> {
    let tokens = tokenizer::tokenize(filename, source)?;

    //no context!
    let empty_context = source::Token {
        token: Rc::from(tokens::Keyword(keywords::Program)),
        location: source::Location::new("test", 0, 0),
    };

    let program = syntax::Program::parse(tokens.into_iter(), &empty_context)?
        .finish()?;

    let typed_program = semantic::Program::annotate(&program.clone())?;
    typed_program.type_check()?;

    Ok(target_c::write_c(&typed_program)?)
}

fn print_usage(program: &str, opts: getopts::Options) {
    let brief = format!("Usage: {} FILE [options]", program);
    print!("{}", opts.usage(&brief));
}

fn compile_file(src_path: &str, clang: bool) -> Result<(), CompileError> {
    let mut file = fs::File::open(src_path)?;

    let mut source = String::new();
    file.read_to_string(&mut source)?;

    let c_unit = compile(src_path, &source)?;

    if clang {
        invoke_clang(&c_unit, src_path)?;
    }
    else {
        println!("{}", c_unit);
    }
    Ok(())
}

fn main() {
    let args = std::env::args().collect::<Vec<String>>();
    let program = args[0].clone();

    let mut opts = getopts::Options::new();
    opts.optflag("c", "invoke-clang", "invoke clang to compile the generated C unit");

    match opts.parse(&args[1..]) {
        Ok(matched) => {
            if matched.free.len() != 1 {
                print_usage(&program, opts);
                std::process::exit(1);
            } else {
                let source_path = &matched.free[0];
                let clang = matched.opt_present("c");

                match compile_file(source_path, clang) {
                    Err(err) => {
                        println!("error: {}", err);
                        std::process::exit(1);
                    }
                    _ => ()
                }
            }
        },
        Err(err) => {
            println!("error: {}", err);
            print_usage(&program, opts);

            std::process::exit(1);
        }
    }
}

fn invoke_clang(c_src: &str, source_path: &str) -> Result<(), CompileError> {
    let os_ext = if cfg!(windows) {
        "exe"
    } else {
        ""
    };

    let target_path = path::PathBuf::from(source_path)
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
