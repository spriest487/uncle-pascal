mod args;
mod compile_error;
mod reporting;
mod sources;

use crate::args::*;
use crate::compile_error::*;
use crate::reporting::report_err;
use crate::sources::*;
use backend_c as backend_c;
use backend_c::c;
use codespan_reporting::diagnostic::Severity;
use common::read_source_file;
use common::span::*;
use common::BuildOptions;
use frontend::ast;
use frontend::ast::{IdentPath, Unit};
use frontend::emit::IROptions;
use frontend::pp as pp;
use frontend::typ as ty;
use interpreter::Interpreter;
use interpreter::InterpreterOpts;
use ir_lang as ir;
use pp::PreprocessedUnit;
use std::collections::hash_map::Entry;
use std::collections::hash_map::HashMap;
use std::env;
use std::ffi::OsStr;
use std::fs;
use std::fs::File;
use std::io;
use std::io::Read;
use std::io::Write;
use std::path::Path;
use std::path::PathBuf;
use std::process;
use std::process::Command;
use std::process::Stdio;
use structopt::StructOpt;
use topological_sort::TopologicalSort;

const IR_LIB_EXT: &str = "lib";

enum CompileOutput {
    Preprocess(Vec<PreprocessedUnit>),
    Parse(Vec<Unit>),
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
    if args.output.is_some() && args.print_stage.is_some() {
        let msg = "output file and print stage arguments are mutually exclusive".to_string();
        return Err(CompileError::InvalidArguments(msg));
    }
    
    let mut opts = BuildOptions::default();
    opts.verbose = args.verbose;
    opts.lang_mode = args.lang_mode;

    for define_sym in &args.define_syms {
        opts.define(define_sym.clone());
    }
    
    let source_ext = get_extension(&args.file);
    if source_ext.eq_ignore_ascii_case(IR_LIB_EXT) {
        let module = load_module_from_file(&args.file)?;
        return Ok(CompileOutput::IR(module));
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
    if let Some(CompileStage::Preprocess) = args.print_stage {
        let mut pp_units = Vec::new();
        while let Some(source_path) = sources.next() {
            let pp_unit = preprocess(&source_path, opts.clone())?;
            pp_units.push(pp_unit);
        }
        return Ok(CompileOutput::Preprocess(pp_units));
    }
    
    let mut parsed_files: HashMap<PathBuf, Unit> = HashMap::new();
    let mut filenames_by_ident: HashMap<IdentPath, PathBuf> = HashMap::new();

    let mut compilation_order = TopologicalSort::<IdentPath>::new();

    loop {
        let unit_filename = match sources.next() {
            None => break,
            Some(f) => f,
        };
        
        let canon_filename = unit_filename
            .canonicalize()
            .map_err(|e| CompileError::ReadSourceFileFailed {
                msg: e.to_string(),
                path: unit_filename.clone(),
            })?;
        
        let unit = match parsed_files.get(&canon_filename) {
            Some(unit) => unit,
            None => {
                let pp_unit = preprocess(&unit_filename, opts.clone())?;

                for warning in &pp_unit.warnings {
                    if report_err(warning, Severity::Warning).is_err() {
                        eprintln!("warning: {}", warning);
                    }
                }

                let tokens = frontend::tokenize(pp_unit)?;
                let parsed_unit = frontend::parse(unit_filename.clone(), tokens)?;

                match filenames_by_ident.entry(parsed_unit.ident.clone()) {
                    Entry::Occupied(occupied_ident_filename) => {
                        // the same unit can't be loaded from two separate filenames
                        return Err(CompileError::DuplicateUnit {
                            unit_ident: occupied_ident_filename.key().clone(),
                            duplicate_path: occupied_ident_filename.get().to_path_buf(),
                        });
                    },

                    Entry::Vacant(vacant_ident_filename) => {
                        let unit_ident = vacant_ident_filename.key().clone();

                        compilation_order.insert(unit_ident);
                        vacant_ident_filename.insert(canon_filename.clone());
                    }
                }

                parsed_files.insert(canon_filename.clone(), parsed_unit);
                &parsed_files[&canon_filename]
            }
        };

        let unit_ident = unit.ident.clone();

        let uses_units: Vec<_> = unit
            .all_decls()
            .filter_map(|(_vis, decl)| match decl {
                ast::UnitDecl::Uses { decl } => Some(decl.clone()),
                _ => None,
            })
            .flat_map(|uses| uses.units)
            .collect();

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

            if !filenames_by_ident.contains_key(&used_unit.ident) {
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

    if let Some(CompileStage::Parse) = args.print_stage {
        return Ok(CompileOutput::Parse(parsed_files.into_values().collect()));
    }

    let mut compile_units = Vec::new();
    while let Some(next_compiled_unit) = compilation_order.pop() {
        let filename = filenames_by_ident.remove(&next_compiled_unit).unwrap();
        let unit = parsed_files.remove(&filename).unwrap();

        compile_units.push(unit);
    }

    assert_eq!(compilation_order.len(), 0);

    if args.verbose {
        println!("Compilation units:");
        for compile_unit in &compile_units {
            println!("\t{}", compile_unit.ident);
        }
    }

    let typed_module = frontend::typecheck(&compile_units)?;

    if let Some(CompileStage::Typecheck) = args.print_stage {
        return Ok(CompileOutput::Typecheck(typed_module));
    }

    let ir_opts = IROptions {
        annotate_rc: args.trace_rc,
        debug: args.debug,
    };

    let module = frontend::emit_ir(&typed_module, ir_opts);
    Ok(CompileOutput::IR(module))
}

fn load_module_from_file(path: &Path) -> Result<ir::Module, CompileError> {
    let mut module_bytes = Vec::new();

    File::open(&path)
        .and_then(|mut file| {
            file.read_to_end(&mut module_bytes)
        })
        .map_err(|err| {
            CompileError::ReadSourceFileFailed {
                msg: err.to_string(),
                path: path.to_path_buf(),
            }
        })?;

    let module: ir::Module = bincode::deserialize(&module_bytes)
        .map_err(|err| {
            CompileError::ReadSourceFileFailed {
                msg: err.to_string(),
                path: path.to_path_buf(),
            }
        })?;

    Ok(module)
}

fn print_output<F>(out_path: Option<&PathBuf>, f: F) -> Result<(), CompileError>
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
        CompileOutput::Preprocess(units) => print_output(args.output.as_ref(), |dst| {
            for pp_unit in units {
                write!(dst, "{}", pp_unit.source)?;
            }
            Ok(())
        }),

        CompileOutput::Parse(units) => print_output(args.output.as_ref(), |dst| {
            for unit in units {
                write!(dst, "{}", unit)?;
            }

            Ok(())
        }),

        CompileOutput::Typecheck(module) => print_output(args.output.as_ref(), |dst| {
            for unit in &module.units {
                write!(dst, "{}", unit.unit)?;
            }

            Ok(())
        }),

        CompileOutput::IR(module) => {
            if let Some(CompileStage::EmitIR) = args.print_stage {
                return print_output(args.output.as_ref(), |dst| write!(dst, "{}", module));
            }
            
            if let Some(output_arg) = args.output.as_ref() {
                let output_ext = args.output.as_ref()
                    .map(|out_path| get_extension(out_path))
                    .unwrap_or_else(String::new);

                if output_ext.eq_ignore_ascii_case("c") {
                    // output C code
                    let c_module = translate_c(&module, args);

                    print_output(args.output.as_ref(), |dst| {
                        write!(dst, "{}", c_module)
                    })
                } else if output_ext.eq_ignore_ascii_case(IR_LIB_EXT) {
                    // the IR object is the output
                    let module_bytes = bincode::serialize(&module)?;

                    print_output(args.output.as_ref(), |dst| {
                        dst.write_all(&module_bytes)
                    })
                } else if output_ext.eq_ignore_ascii_case(env::consts::EXE_EXTENSION) {
                    clang_compile(&module, args, output_arg.as_os_str())
                        .map_err(|err| CompileError::ClangBuildFailed(err))
                } else {
                    return Err(CompileError::UnknownOutputFormat(output_ext))
                }
            } else {
                // execute the IR immediately
                let interpret_opts = InterpreterOpts {
                    trace_rc: args.trace_rc,
                    trace_heap: args.trace_heap,
                    trace_ir: args.trace_ir,
                    
                    diag_port: args.diag_port,
                };

                let mut interpreter = Interpreter::new(interpret_opts);
                interpreter.load_module(&module)?;
                interpreter.shutdown()?;

                Ok(())
            }
        }
    }
}

fn translate_c(module: &ir::Module, args: &Args) -> c::Module {
    let c_opts = backend_c::Options {
        trace_heap: args.trace_heap,
        trace_rc: args.trace_rc,
        trace_ir: args.trace_ir,
    };

    backend_c::translate(&module, c_opts)
}

fn clang_compile(module: &ir::Module, args: &Args, out_path: &OsStr) -> io::Result<()> {
    let c_module = translate_c(module, args);
    
    let mut clang_cmd = Command::new("clang");
    clang_cmd
        .arg("-Werror")
        .arg("-Wall")
        .arg("-Wextra")
        .arg("-Wno-unused-function")
        .arg("-Wno-unused-parameter")
        .arg("-Wno-unused-variable")
        .arg("-Wno-unused-label")
        .arg("-x").arg("c")
        .arg("-o").arg(out_path);
    
    let debug = args.debug || args.debug_codeview;

    let mut clang = if debug {
        // debug: generate a source file next to the output
        let c_file_path = PathBuf::from(out_path).with_extension("c");
        
        let mut c_file = File::create(&c_file_path)?; 
        write!(c_file, "{c_module}")?;

        clang_cmd.arg(c_file_path)
            .arg("-g")
            .arg("-O0");

        if args.debug_codeview {
            clang_cmd.arg("-gcodeview");
        }

        clang_cmd.spawn()?
    } else {
        // release: compile from stdin
        clang_cmd.arg("-")
            .stdin(Stdio::piped());

        let mut clang = clang_cmd.spawn()?;

        let mut clang_in = clang.stdin
            .take()
            .ok_or_else(|| io::Error::new(io::ErrorKind::BrokenPipe, "unable to write to stdin"))?;

        write!(clang_in, "{}", c_module)?;
        drop(clang_in);
        
        clang
    };

    let status = clang.wait()?;

    if !status.success() {
        return Err(io::Error::new(io::ErrorKind::Other, status.to_string()));
    }
    
    Ok(())
}

fn get_extension(path: &Path) -> String {
    path.extension()
        .map(|ext| ext.to_string_lossy().into_owned())
        .unwrap_or_else(String::new)
} 

fn main() {
    let args: Args = Args::from_args();

    let print_bt = args.backtrace;

    if let Err(err) = compile(&args).and_then(|output| handle_output(output, &args)) {
        if let Err(output_err) = report_err(&err, Severity::Error) {
            eprintln!("error: {}", err);
            eprintln!("error reporting output: {}", output_err);
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
