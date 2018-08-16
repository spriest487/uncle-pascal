extern crate regex;

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

pub trait ToSource {
    fn to_source(&self) -> String;
}

enum CompileError {
    TokenizeError(tokenizer::IllegalToken),
    ParseError(syntax::ParseError<tokenizer::SourceToken>),
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

impl From<syntax::ParseError<tokenizer::SourceToken>> for CompileError {
    fn from(err: syntax::ParseError<tokenizer::SourceToken>) -> Self {
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

fn compile(source: &str) -> Result<String, CompileError> {
    let tokens = tokenizer::tokenize(source)?;

    //no context!
    let empty_context = tokenizer::SourceToken {
        token: tokens::Keyword(keywords::Program),
        line: 0,
        col: 0,
    };

    let program = syntax::Program::parse(tokens.into_iter(), &empty_context)?
        .finish()?;

    let typed_program = semantic::Program::annotate(&program.clone())?;
    typed_program.type_check()?;

    //println!("{:?}", typed_program);
    //println!("{}", program.to_source());

    Ok(target_c::write_c(&typed_program)?)
}

fn main() {
    let hello_world_pas = include_str!("../HelloWorld.pas");

    match compile(hello_world_pas) {
        Ok(c_unit) => {
            println!("{}", c_unit);
        }
        Err(err) => {
            println!("error: {}", err);
            std::process::exit(1);
        }
    }
}