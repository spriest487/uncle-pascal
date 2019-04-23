use {
    pas_common::{
        BuildOptions,
        span::*,
        TracedError,
    },
    structopt::StructOpt,
    pas_pp::{
        self as pp,
        PreprocessorError,
    },
    pas_syn::{
        ast as syn,
        parse::*,
        TokenizeError,
        TokenTree,
    },
    pas_typecheck::{
        ast as typ,
        TypecheckError,
    },
    pas_ir::{
        self as ir,
        InterpreterOpts,
        Interpreter,
    },
    std::{
        fmt,
        process,
        fs::File,
        io::Read,
        path::PathBuf,
        str::FromStr,
    },
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

    fn fmt_context(&self, f: impl fmt::Write, source: &str) -> fmt::Result {
        match self {
            CompileError::TokenizeError(err) => err.fmt_context(f, source),
            CompileError::ParseError(err) => err.fmt_context(f, source),
            CompileError::TypecheckError(err) => err.fmt_context(f, source),
            CompileError::PreprocessorError(err) => err.fmt_context(f, source),
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

#[derive(Debug, Ord, PartialOrd, Eq, PartialEq, Copy, Clone)]
enum Stage {
    Interpret,
    Intermediate,
    SyntaxAst,
    TypecheckAst,
    Preprocessed,
}

impl FromStr for Stage {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, String> {
        match s {
            "interpret" => Ok(Stage::Interpret),
            "ir" => Ok(Stage::Intermediate),
            "syn" => Ok(Stage::SyntaxAst),
            "tyck" => Ok(Stage::TypecheckAst),
            "pp" => Ok(Stage::Preprocessed),
            _ => Err(format!("invalid output kind: {}", s)),
        }
    }
}

#[derive(StructOpt, Debug)]
struct Args {
    /// source file of program/library main unit
    #[structopt(name = "FILE", parse(from_os_str))]
    file: PathBuf,

    /// target stage. intermediate stages other than `interpret` will cause
    /// compilation to stop at that stage and dump the output.
    #[structopt(short = "s", long = "stage", default_value = "interpret")]
    stage: Stage,

    /// interpreter: log RC heap usage
    #[structopt(long = "trace-heap")]
    trace_heap: bool,

    /// interpreter: log RC retain/release operations
    #[structopt(long = "trace-rc")]
    trace_rc: bool,

    /// interpreter: log executed IR instructions
    #[structopt(long = "trace-ir")]
    trace_ir: bool,

    /// print compiler backtrace on compilation failure
    #[structopt(long = "backtrace", short = "bt")]
    backtrace: bool,
}

fn compile(filename: impl Into<PathBuf>,
    src: &str,
    opts: BuildOptions,
    interpret_opts: InterpreterOpts,
    out_kind: Stage)
    -> Result<(), CompileError>
{
    let filename = filename.into();

    let pp = pp::Preprocessor::new(filename.clone(), opts);
    let preprocessed = pp.preprocess(src)?;

    if out_kind == Stage::Preprocessed {
        println!("{}", preprocessed.source);
        return Ok(());
    }

    let tokens = TokenTree::tokenize(filename.clone(), &preprocessed.source, &preprocessed.opts)?;

    let context = Span::zero(filename);

    let mut token_stream = TokenStream::new(tokens, context);
    let unit = syn::Unit::parse(&mut token_stream)?;
    token_stream.finish()?;

    if out_kind == Stage::SyntaxAst {
        println!("{}", unit);
        return Ok(());
    }

    let unit = typ::typecheck_unit(&unit)?;
    if out_kind == Stage::TypecheckAst {
        println!("{}", unit);
        return Ok(());
    }

    let unit = ir::translate_unit(&unit);
    if out_kind == Stage::Intermediate {
        println!("{}", unit);
        return Ok(());
    }

    let mut interpreter = Interpreter::new(&interpret_opts);
    interpreter.load_module(&unit);

    Ok(())
}

fn main() {
    let args: Args = Args::from_args();
    let opts = BuildOptions::default();
    let interpret_opts = InterpreterOpts {
        trace_rc: args.trace_rc,
        trace_heap: args.trace_heap,
        trace_ir: args.trace_ir,
    };

    let open_file = File::open(&args.file).and_then(|mut f| {
        let mut src = String::new();
        f.read_to_string(&mut src)?;
        Ok(src)
    });

    let src = match open_file {
        Err(err) => {
            eprintln!("failed to open {}: {}", args.file.display(), err);
            std::process::exit(1);
        }
        Ok(file) => file,
    };

    let print_bt = args.backtrace;

    if let Err(err) = compile(args.file, &src, opts, interpret_opts, args.stage) {
        err.print_context(&src);

        if print_bt {
            match err {
                CompileError::TokenizeError(err) => {
                    println!("{:?}", err.bt);
                }

                CompileError::ParseError(err) => {
                    println!("{:?}", err.bt);
                }

                _ => {},
            }
        }

        process::exit(1)
    }
}
