use {
    std::{
        path::PathBuf,
        fmt,
        fs::{File},
        io::{Read},
        str::FromStr,
    },
    structopt::{
        StructOpt,
    },
    pas_common::{
        TracedError,
        BuildOptions,
        span::*,
    },
    pas_syn::{
        parse::*,
        TokenizeError,
        TokenTree,
        ast as syn,
    },
    pas_typecheck::{
        TypecheckError,
        ast as typ,
    },
    pas_pp::{
        self as pp,
        PreprocessorError,
    },
    pas_ir,
};

#[derive(Debug)]
pub enum CompileError {
    TokenizeError(TracedError<TokenizeError>),
    ParseError(TracedError<ParseError>),
    TypecheckError(TypecheckError),
    PreprocessorError(PreprocessorError),
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

impl From<PreprocessorError> for CompileError {
    fn from(err: PreprocessorError) -> Self {
        CompileError::PreprocessorError(err)
    }
}

impl Spanned for CompileError {
    fn span(&self) -> &Span {
        match self {
            CompileError::TokenizeError(err) => err.span(),
            CompileError::ParseError(err) => err.span(),
            CompileError::TypecheckError(err) => err.span(),
            CompileError::PreprocessorError(err) => err.span(),
        }
    }
}

impl fmt::Display for CompileError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            CompileError::TokenizeError(err) => write!(f, "{}", err.err),
            CompileError::ParseError(err) => write!(f, "{}", err.err),
            CompileError::TypecheckError(err) => write!(f, "{}", err),
            CompileError::PreprocessorError(err) => write!(f, "{}", err),
        }
    }
}

#[derive(Debug, Ord, PartialOrd, Eq, PartialEq)]
enum OutputKind {
    Interpret,
    Intermediate,
    SyntaxAst,
    TypecheckAst,
    Preprocessed,
}

impl FromStr for OutputKind {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, String> {
        match s {
            "interpret" => Ok(OutputKind::Interpret),
            "ir" => Ok(OutputKind::Intermediate),
            "syn" => Ok(OutputKind::SyntaxAst),
            "tyck" => Ok(OutputKind::TypecheckAst),
            "pp" => Ok(OutputKind::Preprocessed),
            _ => Err(format!("invalid output kind: {}", s)),
        }
    }
}

#[derive(StructOpt, Debug)]
struct Args {
    #[structopt(name = "FILE", parse(from_os_str))]
    file: PathBuf,

    #[structopt(short = "k", long="output-kind", default_value = "interpret")]
    output: OutputKind,
}

fn compile(filename: impl Into<PathBuf>, src: &str, opts: BuildOptions, out_kind: OutputKind) -> Result<(), CompileError> {
    let filename = filename.into();

    let pp = pp::Preprocessor::new(filename.clone(), opts);
    let preprocessed = pp.preprocess(src)?;

    if out_kind == OutputKind::Preprocessed {
        println!("{}", preprocessed.source);
        return Ok(());
    }

    let tokens = TokenTree::tokenize(filename.clone(), &preprocessed.source, &preprocessed.opts)?;

    let context = Span::zero(filename);

    let mut token_stream = TokenStream::new(tokens, context);
    let unit = syn::Unit::parse(&mut token_stream)?;
    token_stream.finish()?;

    if out_kind == OutputKind::SyntaxAst {
        println!("{}", unit);
        return Ok(());
    }

    let unit = typ::typecheck_unit(&unit)?;
    if out_kind == OutputKind::TypecheckAst {
        println!("{}", unit);
        return Ok(())
    }

    let unit = pas_ir::translate_unit(&unit);
    if out_kind == OutputKind::Intermediate {
        println!("{}", unit);
        return Ok(())
    }

    let mut interpreter = pas_ir::Interpreter::new();
    interpreter.load_unit(&unit);

    Ok(())
}

fn main() -> Result<(), CompileError> {
    let args: Args = Args::from_args();
    let opts = BuildOptions::default();

    let open_file = File::open(&args.file).and_then(|mut f| {
        let mut src = String::new();
        f.read_to_string(&mut src)?;
        Ok(src)
    });

    let src = match open_file {
        Err(err) => {
            eprintln!("failed to open {}: {}", args.file.display(), err);
            std::process::exit(1);
        },
        Ok(file) => file,
    };

    compile(args.file, &src, opts, args.output).map_err(|err| {
        err.print_context(&src);

        match &err {
            CompileError::TokenizeError(err) => {
                println!("{:?}", err.bt);
            }

            CompileError::ParseError(err) => {
                println!("{:?}", err.bt);
            }

            _ => {}
        }

        err
    })
}
