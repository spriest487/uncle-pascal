extern crate regex;

use std::fmt;

mod keywords;
mod types;
mod operators;
mod tokens;
mod tokenizer;
mod syntax;

enum CompileError {
    TokenizeError(tokenizer::IllegalToken),
    ParseError(syntax::ParseError),
}

impl fmt::Display for CompileError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &CompileError::TokenizeError(ref token) => write!(f, "{}", token),
            &CompileError::ParseError(ref err) => write!(f, "{}", err),
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

fn compile(source: &str) -> Result<(), CompileError> {
    let tokens = tokenizer::tokenize(source)?;

    let program = syntax::program::Program::parse(tokens.into_iter())?;

    println!("{:?}", &program.value);

    program.finish()?;

    Ok(())
}

fn main() {
    let hello_world_pas = include_str!("../HelloWorld.pas");

    match compile(hello_world_pas) {
        Ok(_) => println!("Success!"),
        Err(err) => println!("Error: {}", err),
    }
}