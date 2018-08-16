extern crate regex;

mod keywords;
mod types;
mod operators;
mod tokens;
mod tokenizer;

fn compile(source: &str) -> tokenizer::TokenizeResult<()> {
    let tokens = tokenizer::tokenize(source)?;

    println!("{:?}", tokens.iter()
        .map(|source_token| tokens::ToSource::to_source(&source_token.token))
        .collect::<Vec<_>>()
        .join(" "));

    Ok(())
}

fn main() {
    let hello_world_pas = include_str!("../HelloWorld.pas");

    match compile(hello_world_pas) {
        Ok(_) => println!("Success!"),
        Err(err) => println!("Error: {}", err),
    }
}