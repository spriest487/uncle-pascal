use {
    std::{
        path::PathBuf,
        fmt,
    },
    pas_common::{
        TracedError,
        BuildOptions
    },
    pas_syn::{
        TokenizeError,
        TokenTree,
        TokenStream,
        ast::ParseError,
        Span,
        Spanned,
        ast as syn,
    },
    pas_typecheck::{
        TypecheckError,
        ast as typ,
    },
    pas_ir,
};

#[derive(Debug)]
pub enum CompileError {
    TokenizeError(TracedError<TokenizeError>),
    ParseError(TracedError<ParseError>),
    TypecheckError(TypecheckError),
}

impl From<TracedError<TokenizeError>> for CompileError {
    fn from(err: TracedError<TokenizeError>) -> Self {
        CompileError::TokenizeError(err)
    }
}

impl From<TracedError<ParseError>> for CompileError {
    fn from(err: TracedError<ParseError>) -> Self {
        CompileError::ParseError(err)
    }
}

impl From<TypecheckError> for CompileError {
    fn from(err: TypecheckError) -> Self {
        CompileError::TypecheckError(err)
    }
}

impl Spanned for CompileError {
    fn span(&self) -> &Span {
        match self {
            CompileError::TokenizeError(err) => err.span(),
            CompileError::ParseError(err) => err.span(),
            CompileError::TypecheckError(err) => err.span(),
        }
    }
}

impl fmt::Display for CompileError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            CompileError::TokenizeError(err) => write!(f, "{}", err.err),
            CompileError::ParseError(err) => write!(f, "{}", err.err),
            CompileError::TypecheckError(err) => write!(f, "{}", err),
        }
    }
}

fn compile(filename: impl Into<PathBuf>, src: &str, opts: &BuildOptions) -> Result<(), CompileError> {
    let tokens = TokenTree::tokenize(filename, src, &opts)?;

    let context = tokens[0].clone();
    let mut token_stream = TokenStream::new(tokens, context);

    let unit = syn::Unit::parse(&mut token_stream)?;

    println!("Parsed: {:#?}", unit);

    let unit = typ::typecheck_unit(&unit)?;

    println!("Typechecked: {:#?}", unit);

    let ir = pas_ir::translate(&unit);
    println!("IR: {:#?}", ir);

    Ok(())
}

fn main() -> Result<(), CompileError> {
    let src = include_str!("../demos/HelloWorld.pas");
    let opts = BuildOptions { case_sensitive: true };

    if let Err(err) = compile("HelloWorld.pas", src, &opts) {
        err.print_context(src);
    }

    Ok(())
}
