mod reporting;

use pas_common::{
    span::*, Backtrace, BuildOptions, DiagnosticLabel, DiagnosticMessage, DiagnosticOutput,
    TracedError,
};
use pas_ir::{self as ir, Interpreter, InterpreterOpts};
use pas_pp::{self as pp, PreprocessedUnit, PreprocessorError};
use pas_syn::{ast as syn, parse::*, TokenTree, TokenizeError};
use pas_typecheck::{self as ty, ast as ty_ast, TypecheckError};
use std::{env, fmt, fs::File, io::Read, path::PathBuf, process, str::FromStr};
use structopt::StructOpt;

#[derive(Debug)]
pub enum CompileError {
    TokenizeError(TracedError<TokenizeError>),
    ParseError(TracedError<ParseError>),
    TypecheckError(TypecheckError),
    PreprocessorError(PreprocessorError),
    InvalidUnitFilename(Span),
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

impl DiagnosticOutput for CompileError {
    fn main(&self) -> DiagnosticMessage {
        match self {
            CompileError::TokenizeError(err) => err.err.main(),
            CompileError::ParseError(err) => err.err.main(),
            CompileError::TypecheckError(err) => err.main(),
            CompileError::PreprocessorError(err) => err.main(),
            CompileError::InvalidUnitFilename(at) => DiagnosticMessage {
                title: "Invalid unit filename".to_string(),
                label: Some(DiagnosticLabel {
                    text: None,
                    span: at.clone(),
                }),
            },
        }
    }

    fn see_also(&self) -> Vec<DiagnosticMessage> {
        match self {
            CompileError::TokenizeError(err) => err.see_also(),
            CompileError::ParseError(err) => err.see_also(),
            CompileError::TypecheckError(err) => err.see_also(),
            CompileError::PreprocessorError(err) => err.see_also(),
            CompileError::InvalidUnitFilename(_) => Vec::new(),
        }
    }

    fn backtrace(&self) -> Option<&Backtrace> {
        match self {
            CompileError::TokenizeError(err) => Some(&err.bt),
            CompileError::ParseError(err) => Some(&err.bt),
            CompileError::TypecheckError(_) => None,
            CompileError::PreprocessorError(_) => None,
            CompileError::InvalidUnitFilename(_) => None,
        }
    }
}

impl Spanned for CompileError {
    fn span(&self) -> &Span {
        match self {
            CompileError::TokenizeError(err) => err.span(),
            CompileError::ParseError(err) => err.span(),
            CompileError::TypecheckError(err) => err.span(),
            CompileError::PreprocessorError(err) => err.span(),
            CompileError::InvalidUnitFilename(span) => span,
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
            CompileError::InvalidUnitFilename(span) => write!(
                f,
                "invalid unit identifier in filename: {}",
                span.file.display()
            ),
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

    /// additional units to link
    #[structopt(name = "units", short = "u")]
    units: Vec<String>,

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

    #[structopt(long = "verbose", short="v")]
    verbose: bool,
}

fn find_in_paths(filename: &PathBuf, search_paths: &[PathBuf]) -> Option<PathBuf> {
    for search_path in search_paths.iter() {
        if search_path.exists() && search_path.is_dir() {
            let file_path = search_path.join(filename);
            if file_path.exists() {
                return Some(file_path);
            }
        }
    }

    None
}

fn preprocess(
    filename: PathBuf,
    search_paths: &[PathBuf],
    opts: &BuildOptions,
) -> Result<PreprocessedUnit, CompileError> {
    let filename = find_in_paths(&filename, search_paths).unwrap_or_else(|| {
        eprintln!("could not find unit {}", filename.display());
        process::exit(1);
    });

    let open_file = File::open(&filename).and_then(|mut f| {
        let mut src = String::new();
        f.read_to_string(&mut src)?;
        Ok(src)
    });

    let src = match open_file {
        Err(err) => {
            eprintln!("failed to open {}: {}", filename.display(), err);
            process::exit(1);
        }
        Ok(file) => file,
    };

    let pp = pp::Preprocessor::new(filename, opts.clone());
    let preprocessed = pp.preprocess(&src)?;

    Ok(preprocessed)
}

fn parse(
    unit_path: impl Into<PathBuf>,
    src: &str,
    opts: &BuildOptions,
) -> Result<syn::Unit<Span>, CompileError> {
    let unit_path = unit_path.into();
    let file_span = Span::zero(unit_path.clone());

    let unit_ident = unit_path
        .with_extension("")
        .file_name()
        .and_then(|file_name| {
            let file_name = file_name.to_str()?;
            let token = TokenTree::tokenize(&unit_path, file_name, opts).ok()?;
            match token.as_slice() {
                [TokenTree::Ident(ident)] => Some(ident.clone()),
                _ => None,
            }
        })
        .ok_or_else(|| CompileError::InvalidUnitFilename(file_span.clone()))?;

    let tokens = TokenTree::tokenize(unit_path.clone(), src, opts)?;

    let mut token_stream = TokenStream::new(tokens, file_span);
    let unit = syn::Unit::parse(&mut token_stream, unit_ident)?;
    token_stream.finish()?;

    Ok(unit)
}

fn compile(
    module_path: impl Into<PathBuf>,
    units: impl IntoIterator<Item = PathBuf>,
    opts: BuildOptions,
    interpret_opts: InterpreterOpts,
    out_kind: Stage,
) -> Result<(), CompileError> {
    let module_path = module_path.into();

    let all_filenames = units.into_iter().chain(vec![module_path.clone()]);

    // todo: search paths should be an arg or env var
    let compiler_dir = env::current_exe()
        .unwrap()
        .parent()
        .unwrap()
        .parent()
        .unwrap()
        .parent()
        .unwrap()
        .join("units")
        .canonicalize()
        .unwrap();

    let cwd = env::current_dir().unwrap().canonicalize().unwrap();
    let search_paths = vec![cwd, compiler_dir];

    if opts.verbose {
        println!("Unit search paths:");
        for path in &search_paths {
            println!("  {}", path.display());
        }
    }

    let pp_units: Vec<_> = all_filenames
        .map(|unit_filename| preprocess(unit_filename, &search_paths, &opts))
        .collect::<Result<_, CompileError>>()?;

    if out_kind == Stage::Preprocessed {
        for unit in pp_units {
            println!("{}", unit.source);
        }
        return Ok(());
    }

    let parsed_units: Vec<_> = pp_units
        .into_iter()
        .map(|preprocessed| {
            parse(
                preprocessed.filename,
                &preprocessed.source,
                &preprocessed.opts,
            )
        })
        .collect::<Result<_, CompileError>>()?;

    if out_kind == Stage::SyntaxAst {
        for unit in parsed_units {
            println!("{}", unit);
        }
        return Ok(());
    }

    let mut root_ctx = ty::Context::root();
    let mut typed_units = Vec::new();

    for unit in parsed_units {
        typed_units.push(ty_ast::typecheck_unit(&unit, &mut root_ctx)?);
    }

    if out_kind == Stage::TypecheckAst {
        for unit in typed_units {
            println!("{}", unit);
        }
        return Ok(());
    }

    let module = ir::translate_units(&typed_units);
    if out_kind == Stage::Intermediate {
        println!("{}", module);
        return Ok(());
    }

    let mut interpreter = Interpreter::new(&interpret_opts);
    interpreter.load_module(&module);

    Ok(())
}

fn main() {
    let args: Args = Args::from_args();
    let mut opts = BuildOptions::default();
    opts.verbose = args.verbose;

    let interpret_opts = InterpreterOpts {
        trace_rc: args.trace_rc,
        trace_heap: args.trace_heap,
        trace_ir: args.trace_ir,
    };

    let print_bt = args.backtrace;

    let mut unit_paths = vec![PathBuf::from("System.pas")];
    unit_paths.extend(args.units.into_iter().map(PathBuf::from));

    if let Err(err) = compile(args.file, unit_paths, opts, interpret_opts, args.stage) {
        if let Err(io_err) = reporting::report_err(&err) {
            eprintln!(
                "error occurred displaying source for compiler message: {}",
                io_err
            );
            eprintln!("{}", err);
        }

        if print_bt {
            match err {
                CompileError::TokenizeError(err) => {
                    println!("{:?}", err.bt);
                }

                CompileError::ParseError(err) => {
                    println!("{:?}", err.bt);
                }

                _ => {}
            }
        }

        process::exit(1)
    }
}
