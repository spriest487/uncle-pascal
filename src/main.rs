mod args;
mod compile_error;
mod reporting;
mod sources;

use crate::args::*;
use crate::compile_error::*;
use crate::reporting::report_err;
use crate::sources::*;
use codespan_reporting::diagnostic::Severity;
use frontend::ast;
use frontend::ast::IdentPath;
use frontend::pp as pp;
use frontend::typ as ty;
use backend_c as backend_c;
use common::read_source_file;
use common::span::*;
use common::BuildOptions;
use interpreter::Interpreter;
use interpreter::InterpreterOpts;
use ir_lang as ir;
use pp::PreprocessedUnit;
use std::collections::hash_map::Entry;
use std::collections::hash_map::HashMap;
use std::fs;
use std::fs::File;
use std::io;
use std::path::PathBuf;
use std::process;
use structopt::StructOpt;
use topological_sort::TopologicalSort;
use frontend::emit::IROptions;

enum CompileOutput {
    Preprocess(Vec<PreprocessedUnit>),
    Parse(Vec<ast::Unit<Span>>),
    Typecheck(ty::Module),
    IR(ir::Module),
}

fn preprocess(filename: &PathBuf, opts: BuildOptions) -> Result<PreprocessedUnit, CompileError> {
    let src = read_source_file(filename)
        .map_err(|err| {
            CompileError::ReadSourceFileFailed {
                path: filename.to_path_buf(),
                msg: err.to_string(),
            }
        })?;
    
    let preprocessed = frontend::preprocess(filename, &src, opts)?;
    Ok(preprocessed)
}

fn compile(args: &Args) -> Result<CompileOutput, CompileError> {
    let mut opts = BuildOptions::default();
    opts.verbose = args.verbose;
    opts.lang_mode = args.lang_mode;

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
        for warning in &pp_unit.warnings {
            if report_err(warning, Severity::Warning).is_err() {
                eprintln!("warning: {}", warning);
            }
        }

        let tokens = frontend::tokenize(pp_unit)?;
        let parsed_unit = frontend::parse(unit_filename.clone(), tokens)?;

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

            compilation_order.add_dependency(used_unit.ident.clone(), unit_ident.clone());
            if compilation_order.peek().is_none() {
                return Err(CompileError::CircularDependency {
                    unit_ident,
                    used_unit: used_unit.ident,
                    span,
                });
            }

            if !parsed_units.contains_key(&used_unit.ident) {
                match used_unit.path {
                    Some(path) => {
                        let filename = PathBuf::from(path);
                        sources.add_used_unit_in_file(&unit_filename, &used_unit.ident, &filename)?;
                    }

                    None => {
                        sources.add_used_unit(&unit_filename, &used_unit.ident)?;
                    }
                }

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

    let typed_module = frontend::typecheck(&compile_units)?;

    if args.target == Target::TypecheckAst {
        return Ok(CompileOutput::Typecheck(typed_module));
    }

    let ir_opts = IROptions {
        annotate_scopes: args.ir_annotate_scopes,
        annotate_rc: args.trace_rc,
        debug_info: args.ir_debug,
    };

    let module = frontend::emit_ir(&typed_module, ir_opts);
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
        if report_err(&err, Severity::Error).is_err() {
            eprintln!("error: {}", err);
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
