use std::{
    env,
    ffi::OsStr,
    fmt,
    fs::{self, File},
    io::{Read as _, Write as _},
    path::{PathBuf},
    process,
    str::FromStr,
};

use structopt::StructOpt;

use pas_backend_c as backend_c;
use pas_common::{span::*, BuildOptions};
use pas_interpreter::{Interpreter, InterpreterOpts};
use pas_ir::{self as ir, IROptions};
use pas_pp::{self as pp, PreprocessedUnit};
use pas_syn::{ast, parse, IdentPath, TokenTree};
use pas_typecheck as ty;

use crate::compile_error::CompileError;

mod compile_error;
mod reporting;

#[derive(Debug, Ord, PartialOrd, Eq, PartialEq, Copy, Clone)]
enum Target {
    Interpret,
    Intermediate,
    SyntaxAst,
    TypecheckAst,
    Preprocessed,
}

impl FromStr for Target {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, String> {
        match s {
            "i" | "interpret" => Ok(Target::Interpret),
            "ir" | "intermediate" => Ok(Target::Intermediate),
            "p" | "parse" => Ok(Target::SyntaxAst),
            "t" | "typecheck" => Ok(Target::TypecheckAst),
            "pp" | "preprocess" => Ok(Target::Preprocessed),
            _ => Err(format!("invalid output kind: {}", s)),
        }
    }
}

#[derive(StructOpt, Debug)]
struct Args {
    /// source file of program/library main unit
    #[structopt(name = "FILE", parse(from_os_str))]
    file: PathBuf,

    /// output file
    /// If the output file extension matches a backend, the output from that backend will be written
    /// to this path.
    /// If no output path is provided the interpreter will be invoked.
    #[structopt(name = "OUTPUT", short = "o", parse(from_os_str))]
    output: Option<PathBuf>,

    /// additional units to compile
    #[structopt(name = "units", short = "u")]
    units: Vec<String>,

    /// source dir for unit source files
    #[structopt(name= "search-dir", short = "s", parse(from_os_str))]
    search_dirs: Vec<PathBuf>,

    /// don't automatically reference the standard library units
    #[structopt(long = "no-stdlib")]
    no_stdlib: bool,

    /// target stage. intermediate targets other than `interpret` will cause
    /// compilation to stop at that stage and dump the output.
    #[structopt(short = "t", long = "target", default_value = "interpret")]
    target: Target,

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

    #[structopt(long = "verbose", short = "v")]
    verbose: bool,

    #[structopt(long = "ir-debug")]
    ir_debug: bool,

    #[structopt(long = "ir-scopes")]
    ir_annotate_scopes: bool,
}

fn find_in_paths(filename: &PathBuf, search_paths: &[PathBuf]) -> Option<PathBuf> {
    for search_path in search_paths.iter() {
        if search_path.exists() && search_path.is_dir() {
            let file_path = search_path.join(filename);

            if file_path.exists() {
                // try to canonicalize the filename (not the rest of the path)
                let file_path_with_canon_name = file_path.canonicalize().ok()
                    .and_then(|canon_path| {
                        let canon_filename = canon_path.file_name()?;
                        Some(file_path.with_file_name(canon_filename))
                    })
                    .unwrap_or(file_path);

                return Some(file_path_with_canon_name);
            }
        }
    }

    None
}

fn preprocess(
    filename: &PathBuf,
    search_paths: &[PathBuf],
    opts: &BuildOptions,
) -> Result<PreprocessedUnit, CompileError> {
    let filename = find_in_paths(&filename, search_paths).unwrap_or_else(|| {
        eprintln!("unit not found: {}", filename.display());
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
) -> Result<ast::Unit<Span>, CompileError> {
    let unit_path = unit_path.into();
    let file_span = Span::zero(unit_path.clone());

    let unit_ident = unit_path
        .with_extension("")
        .file_name()
        .and_then(|file_name| {
            let file_name = file_name.to_str()?;

            let tokens = TokenTree::tokenize(&unit_path, file_name, opts).ok()?;
            let mut stream = parse::TokenStream::new(tokens, file_span.clone());
            let ident = IdentPath::parse(&mut stream).ok()?;
            stream.finish().ok()?;

            Some(ident)
        })
        .ok_or_else(|| CompileError::InvalidUnitFilename(file_span.clone()))?;

    let tokens = TokenTree::tokenize(unit_path.clone(), src, opts)?;

    let mut token_stream = parse::TokenStream::new(tokens, file_span);
    let unit = ast::Unit::parse(&mut token_stream, unit_ident)?;
    token_stream.finish()?;

    Ok(unit)
}

fn write_output_file(out_path: &PathBuf, output: &impl fmt::Display) -> Result<(), CompileError> {
    let create_dirs = match out_path.parent() {
        Some(parent) => fs::create_dir_all(parent),
        None => Ok(()),
    };

    create_dirs
        .and_then(|_| File::create(out_path))
        .and_then(|mut file| write!(file, "{}", output))
        .map_err(|io_err| {
            let span = Span::zero(out_path);
            CompileError::OutputFailed(span, io_err)
        })
}

fn compile(units: impl IntoIterator<Item = PathBuf>, args: &Args) -> Result<(), CompileError> {
    let mut opts = BuildOptions::default();
    opts.verbose = args.verbose;

    let all_filenames = units.into_iter().chain(vec![args.file.clone()]);
    let source_dirs = source_dirs(args);

    if opts.verbose {
        println!("Unit source directories:");
        for path in &source_dirs {
            println!("\t{}", path.display());
        }
    }

    let pp_units: Vec<_> = all_filenames
        .map(|unit_filename| preprocess(&unit_filename, &source_dirs, &opts))
        .collect::<Result<_, CompileError>>()?;

    if args.target == Target::Preprocessed {
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

    if args.target == Target::SyntaxAst {
        for unit in parsed_units {
            println!("{}", unit);
        }
        return Ok(());
    }

    let typed_module = ty::Module::typecheck(&parsed_units, args.no_stdlib)?;

    if args.target == Target::TypecheckAst {
        for unit in &typed_module.units {
            println!("{}", unit.unit);
        }
        return Ok(());
    }

    let ir_opts = IROptions {
        annotate_scopes: args.ir_annotate_scopes,
        annotate_rc: args.trace_rc,
        debug_info: args.ir_debug,
    };

    let module = ir::translate(&typed_module, ir_opts);
    if args.target == Target::Intermediate {
        println!("{}", module);
        return Ok(());
    }

    if let Some(out_path) = &args.output {
        let ext = out_path.extension().map(OsStr::to_string_lossy);
        match ext.as_ref().map(AsRef::as_ref) {
            Some("c") => {
                let opts = backend_c::Options {
                    trace_heap: args.trace_heap,
                    trace_rc: args.trace_rc,
                    trace_ir: args.trace_ir,
                    no_stdlib: args.no_stdlib,
                };
                let module = backend_c::translate(&module, opts);
                write_output_file(&out_path, &module)?;
            }
            _ => unimplemented!("backend supporting output file {}", out_path.display()),
        }
    } else {
        let interpret_opts = InterpreterOpts {
            trace_rc: args.trace_rc,
            trace_heap: args.trace_heap,
            trace_ir: args.trace_ir,
            no_stdlib: args.no_stdlib,
        };

        let mut interpreter = Interpreter::new(&interpret_opts);
        interpreter.load_module(&module, !args.no_stdlib)?;
        interpreter.shutdown()?;
    }

    Ok(())
}

fn source_dirs(args: &Args) -> Vec<PathBuf> {
    args.search_dirs.iter()
        .filter(|dir| dir.exists())
        .cloned()
        .chain({
            let cwd = env::current_dir().ok();
            let units_dir = env::var("PASCAL2_UNITS").ok().map(PathBuf::from);

            [cwd, units_dir]
                .iter()
                .filter_map(|dir| dir.as_ref())
                .filter(|dir| dir.exists())
                .cloned()
        })
        .collect()
}

fn main() {
    let args: Args = Args::from_args();

    let print_bt = args.backtrace;

    let mut unit_paths = if args.no_stdlib {
        Vec::new()
    } else {
        vec![PathBuf::from("System.pas")]
    };

    unit_paths.extend(args.units.iter().map(PathBuf::from));

    if let Err(err) = compile(unit_paths, &args) {
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
