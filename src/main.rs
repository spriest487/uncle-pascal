use {
    pas_common::BuildOptions,
    pas_syn::{
        TokenTree,
        SpanDisplay,
        TokenStream,
        ast::*,
    }
};

fn main() {
    let src = include_str!("../demos/HelloWorld.pas");
    let opts = BuildOptions { case_sensitive: true };

    match TokenTree::tokenize("test.pas", src, &opts) {
        Ok(tokens) => {
//            println!("{:#?}", tokens)
            let context = tokens[0].clone();
            let mut token_stream = TokenStream::new(tokens, context);
            let unit = Unit::parse(&mut token_stream);

            println!("{:#?}", unit);
        }
        Err(err) => err.print_context(src),
    }
}
