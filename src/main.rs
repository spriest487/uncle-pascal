mod args;
mod compile_error;
mod reporting;
mod sources;

use std::{
    ffi::OsStr,
    fmt,
    fs::{self, File},
    io::{Read as _, Write as _},
    path::{PathBuf},
    process,
};
use linked_hash_map::{Entry, LinkedHashMap};
use structopt::StructOpt;
use pas_backend_c as backend_c;
use pas_common::{span::*, BuildOptions};
use pas_interpreter::{Interpreter, InterpreterOpts};
use pas_ir::{self as ir, IROptions};
use pas_pp::{self as pp, PreprocessedUnit};
use pas_syn::{ast, parse, IdentPath, TokenTree};
use pas_typecheck as ty;
use crate::{
    args::*,
    compile_error::*,
    sources::*,
};

fn preprocess(
    filename: &PathBuf,
    opts: BuildOptions,
) -> Result<PreprocessedUnit, CompileError> {
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

    let pp = pp::Preprocessor::new(filename, opts);
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

fn compile(args: &Args) -> Result<(), CompileError> {
    let mut opts = BuildOptions::default();
    opts.verbose = args.verbose;

    if args.no_stdlib {
        opts.define("NO_STDLIB".to_string());
    }

    let mut sources = SourceCollection::new(args)?;

    if opts.verbose {
        println!("Unit source directories:");
        for path in sources.source_dirs() {
            println!("\t{}", path.display());
        }
    }

    // if we just want preprocessor output, no unit refs need to be looked up, just process and
    // print the units provided on the cli
    if args.target == Target::Preprocessed {
        while let Some(source_path) = sources.next() {
            let pp_unit = preprocess(&source_path, opts.clone())?;
            println!("{}", pp_unit.source);
        }
        return Ok(());
    }

    let mut parsed_units = LinkedHashMap::new();

    loop {
        let unit_filename = match sources.next() {
            None => break,
            Some(f) => f,
        };

        let pp_unit = preprocess(&unit_filename, opts.clone())?;
        let parsed_unit = parse(&pp_unit.filename, &pp_unit.source, &pp_unit.opts)?;

        let uses_units: Vec<_> = parsed_unit.decls.iter()
            .filter_map(|decl| match decl {
                ast::UnitDecl::Uses { decl } => Some(decl.clone()),
                _ => None,
            })
            .flat_map(|uses| uses.units)
            .collect();

        match parsed_units.entry(parsed_unit.ident.clone()) {
            Entry::Occupied(..) => {
                return Err(CompileError::DuplicateUnit {
                    unit_ident: parsed_unit.ident,
                    duplicate_path: unit_filename,
                });
            },

            Entry::Vacant(entry) => {
                entry.insert(parsed_unit);
            }
        }

        for used_unit in uses_units {
            if !parsed_units.contains_key(&used_unit) {
                let filename = PathBuf::from(used_unit.to_string()).with_extension("pas");
                sources.add(&filename, Some(used_unit.path_span()))?;
            }
        }
    }

    if args.target == Target::SyntaxAst {
        for (_, unit) in parsed_units {
            println!("{}", unit);
        }
        return Ok(());
    }

    let parsed_units: Vec<_> = parsed_units.into_iter().map(|(_path, unit)| unit).collect();
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

fn main() {
    let args: Args = Args::from_args();

    let print_bt = args.backtrace;

    if let Err(err) = compile(&args) {
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
