use pas_syn::{
    token_tree::*,
    span::SpanDisplay,
};

fn main() {
    let src = "let x: Integer = 123;";
    match TokenTree::tokenize("test.pas", src, true) {
        Ok(tokens) => { println!("{:#?}", tokens) }
        Err(err) => err.print_context(src),
    }
}
