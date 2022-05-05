mod args;
mod compile_error;
mod reporting;
mod sources;

use crate::{args::*, compile_error::*, sources::*};
use pas_backend_c as backend_c;
use pas_common::{span::*, BuildOptions};
use pas_interpreter::{Interpreter, InterpreterOpts};
use pas_ir::{self as ir, IROptions};
use pas_pp::{self as pp, PreprocessedUnit};
use pas_syn::{ast, parse, IdentPath, TokenTree, Ident};
use pas_typecheck as ty;
use std::{
    collections::hash_map::{Entry, HashMap},
    fs::{self, File},
    io,
    io::{Read as _},
    path::PathBuf,
    process,
};
use structopt::StructOpt;
use topological_sort::TopologicalSort;

enum CompileOutput {
    Preprocess(Vec<PreprocessedUnit>),
    Parse(Vec<pas_syn::ast::Unit<Span>>),
    Typecheck(ty::Module),
    IR(ir::Module),
}

fn preprocess(filename: &PathBuf, opts: BuildOptions) -> Result<PreprocessedUnit, CompileError> {
    let open_file = File::open(&filename).and_then(|mut f| {
        let mut src = String::new();
        f.read_to_string(&mut src)?;
        Ok(src)
    });

    let src = match open_file {
        Err(err) => {
            eprintln!("failed to open {}: {}", filename.display(), err);
            process::exit(1);
        },
        Ok(file) => file,
    };

    let pp = pp::Preprocessor::new(filename, opts);
    let preprocessed = pp.preprocess(&src)?;

    Ok(preprocessed)
}

fn parse(unit: PreprocessedUnit) -> Result<ast::Unit<Span>, CompileError> {
    let file_span = Span {
        file: unit.filename.clone(),
        start: Location::zero(),
        end: Location::zero()
    };

    let unit_ident = unit.filename
        .with_extension("")
        .file_name()
        .and_then(|file_name| {
            let unit_ident = IdentPath::from_parts(file_name
                .to_string_lossy()
                .split('.')
                .map(|part| Ident::new(part, file_span.clone())));

            Some(unit_ident)
        })
        .ok_or_else(|| CompileError::InvalidUnitFilename(file_span.clone()))?;

    let tokens = TokenTree::tokenize(unit)?;

    let mut token_stream = parse::TokenStream::new(tokens, file_span);
    let unit = ast::Unit::parse(&mut token_stream, unit_ident)?;
    token_stream.finish()?;

    Ok(unit)
}

fn compile(args: &Args) -> Result<CompileOutput, CompileError> {
    let mut opts = BuildOptions::default();
    opts.verbose = args.verbose;

    for define_sym in &args.define_syms {
        opts.define(define_sym.clone());
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
        let mut pp_units = Vec::new();
        while let Some(source_path) = sources.next() {
            let pp_unit = preprocess(&source_path, opts.clone())?;
            pp_units.push(pp_unit);
        }
        return Ok(CompileOutput::Preprocess(pp_units));
    }

    let mut parsed_units = HashMap::new();
    let mut compilation_order = TopologicalSort::<IdentPath>::new();

    loop {
        let unit_filename = match sources.next() {
            None => break,
            Some(f) => f,
        };

        let pp_unit = preprocess(&unit_filename, opts.clone())?;
        let parsed_unit = parse(pp_unit)?;

        let unit_ident = parsed_unit.ident.clone();
        let uses_units: Vec<_> = parsed_unit
            .all_decls()
            .filter_map(|(_vis, decl)| match decl {
                ast::UnitDecl::Uses { decl } => Some(decl.clone()),
                _ => None,
            })
            .flat_map(|uses| uses.units)
            .collect();

        match parsed_units.entry(unit_ident.clone()) {
            Entry::Occupied(..) => {
                return Err(CompileError::DuplicateUnit {
                    unit_ident: parsed_unit.ident,
                    duplicate_path: unit_filename,
                });
            },

            Entry::Vacant(entry) => {
                entry.insert(parsed_unit);
                compilation_order.insert(unit_ident.clone());
            },
        }

        for used_unit in uses_units {
            let span = used_unit.span().clone();

            if args.verbose {
                println!("unit {} used from {}", used_unit, unit_ident);
            }

            compilation_order.add_dependency(used_unit.clone(), unit_ident.clone());
            if compilation_order.peek().is_none() {
                return Err(CompileError::CircularDependency {
                    unit_ident,
                    used_unit,
                    span,
                });
            }

            if !parsed_units.contains_key(&used_unit) {
                sources.add_used_unit(&unit_filename, &used_unit)?;
            }
        }
    }

    if args.target == Target::SyntaxAst {
        return Ok(CompileOutput::Parse(parsed_units.into_values().collect()));
    }

    let mut compile_units = Vec::new();
    while let Some(next_compiled_unit) = compilation_order.pop() {
        compile_units.push(parsed_units.remove(&next_compiled_unit).unwrap());
    }

    assert_eq!(compilation_order.len(), 0);

    if args.verbose {
        println!("Compilation units:");
        for compile_unit in &compile_units {
            println!("\t{}", compile_unit.ident);
        }
    }

    let typed_module = ty::Module::typecheck(&compile_units)?;

    if args.target == Target::TypecheckAst {
        return Ok(CompileOutput::Typecheck(typed_module));
    }

    let ir_opts = IROptions {
        annotate_scopes: args.ir_annotate_scopes,
        annotate_rc: args.trace_rc,
        debug_info: args.ir_debug,
    };

    let module = ir::translate(&typed_module, ir_opts);
    Ok(CompileOutput::IR(module))
}

fn write_output<F>(out_path: Option<&PathBuf>, f: F) -> Result<(), CompileError>
where
    F: FnOnce(&mut dyn io::Write) -> io::Result<()>,
{
    let out_span;

    let io_result = match out_path {
        Some(out_path) => {
            let create_dirs = match out_path.parent() {
                Some(parent) => fs::create_dir_all(parent),
                None => Ok(()),
            };

            out_span = Span::zero(out_path.clone());

            create_dirs
                .and_then(|_| File::create(out_path))
                .and_then(|mut file| f(&mut file))
        },

        None => {
            let stdout = io::stdout();
            let mut stdout_lock = stdout.lock();

            out_span = Span::zero("stdout");

            f(&mut stdout_lock)
        },
    };

    io_result.map_err(|io_err| CompileError::OutputFailed(out_span, io_err))
}

fn handle_output(output: CompileOutput, args: &Args) -> Result<(), CompileError> {
    match output {
        CompileOutput::Preprocess(units) => write_output(args.output.as_ref(), |dst| {
            for pp_unit in units {
                write!(dst, "{}", pp_unit.source)?;
            }
            Ok(())
        }),

        CompileOutput::Parse(units) => write_output(args.output.as_ref(), |dst| {
            for unit in units {
                write!(dst, "{}", unit)?;
            }

            Ok(())
        }),

        CompileOutput::Typecheck(module) => write_output(args.output.as_ref(), |dst| {
            for unit in &module.units {
                write!(dst, "{}", unit.unit)?;
            }

            Ok(())
        }),

        CompileOutput::IR(module) if args.target == Target::Intermediate => {
            write_output(args.output.as_ref(), |dst| write!(dst, "{}", module))
        },

        CompileOutput::IR(module) => {
            let c_hint = args.output.as_ref()
                .and_then(|out_path| out_path.extension())
                .map(|ext| ext.to_string_lossy().eq_ignore_ascii_case("c"))
                .unwrap_or(false);

            if c_hint {
                let c_opts = backend_c::Options {
                    trace_heap: args.trace_heap,
                    trace_rc: args.trace_rc,
                    trace_ir: args.trace_ir,
                };

                let c_module = backend_c::translate(&module, c_opts);

                write_output(args.output.as_ref(), |dst| {
                    write!(dst, "{}", c_module)
                })
            } else {
                let interpret_opts = InterpreterOpts {
                    trace_rc: args.trace_rc,
                    trace_heap: args.trace_heap,
                    trace_ir: args.trace_ir,
                };

                let mut interpreter = Interpreter::new(interpret_opts);
                interpreter.load_module(&module)?;
                interpreter.shutdown()?;

                Ok(())
            }
        }
    }
}

fn main() {
    let args: Args = Args::from_args();

    let print_bt = args.backtrace;

    if let Err(err) = compile(&args).and_then(|output| handle_output(output, &args)) {
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
                },

                CompileError::ParseError(err) => {
                    println!("{:?}", err.bt);
                },

                _ => {},
            }
        }

        process::exit(1)
    }
}
