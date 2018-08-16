extern crate regex;

use std::rc::*;
use std::fmt;

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
}

impl fmt::Display for CompileError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &CompileError::TokenizeError(ref token) => write!(f, "{}", token),
            &CompileError::ParseError(ref err) => write!(f, "{}", err),
            &CompileError::SemanticError(ref err) => write!(f, "{}", err),
            &CompileError::WriterError(ref err) => write!(f, "{}", err),
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

fn main() {
    let hello_world_pas = include_str!("../HelloWorld.pas");

    match compile("HelloWorld.pas", hello_world_pas) {
        Ok(c_unit) => {
            println!("{}", c_unit);
        }
        Err(err) => {
            println!("error: {}", err);
            std::process::exit(1);
        }
    }
}