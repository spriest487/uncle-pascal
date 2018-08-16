extern crate regex;
extern crate getopts;

use std::{
    rc::*,
    fmt,
    io::{self},
    fs,
    path::*,
    ffi::OsStr,
    env::current_dir,
    collections::{
        hash_map::HashMap,
        HashSet,
    },
};

mod keywords;
mod types;
mod operators;
mod tokens;
mod tokenizer;
mod syntax;
mod target_c;
mod semantic;
mod node;
mod source;
mod pp;

pub enum CompileError {
    TokenizeError(tokenizer::IllegalToken),
    ParseError(syntax::ParseError),
    SemanticError(semantic::SemanticError),
    UnresolvedUnit(String),
    PreprocessorError(pp::PreprocessorError),
    WriterError(fmt::Error),
    IOError(io::Error),
}

impl fmt::Display for CompileError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            CompileError::TokenizeError(token) => write!(f, "failed to read file: {}", token),
            CompileError::ParseError(err) => write!(f, "syntax error: {}", err),
            CompileError::SemanticError(err) => write!(f, "semantic error: {}", err),
            CompileError::WriterError(err) => write!(f, "output error: {}", err),
            CompileError::IOError(err) => write!(f, "io error: {}", err),
            CompileError::UnresolvedUnit(unit) => write!(f, "unresolved unit: {}", unit),
            CompileError::PreprocessorError(err) => write!(f, "preprocessor error: {}", err),
        }
    }
}

impl From<tokenizer::IllegalToken> for CompileError {
    fn from(err: tokenizer::IllegalToken) -> Self {
        CompileError::TokenizeError(err)
    }
}

impl From<syntax::ParseError> for CompileError {
    fn from(err: syntax::ParseError) -> Self {
        CompileError::ParseError(err)
    }
}

impl From<semantic::SemanticError> for CompileError {
    fn from(err: semantic::SemanticError) -> Self {
        CompileError::SemanticError(err)
    }
}

impl From<fmt::Error> for CompileError {
    fn from(err: fmt::Error) -> Self {
        CompileError::WriterError(err)
    }
}

impl From<io::Error> for CompileError {
    fn from(err: io::Error) -> Self { CompileError::IOError(err) }
}

impl From<pp::PreprocessorError> for CompileError {
    fn from(err: pp::PreprocessorError) -> Self {
        match err {
            pp::PreprocessorError::IOError(io_err) =>
                CompileError::IOError(io_err),

            err @ _ => CompileError::PreprocessorError(err),
        }
    }
}

fn empty_context() -> source::Token {
    source::Token {
        token: Rc::from(tokens::Keyword(keywords::Program)),
        location: source::Location::new("", 0, 0),
    }
}

fn load_source<TPath: AsRef<Path>>(path: TPath,
                                   pp_symbols: HashSet<String>)
                                   -> Result<Vec<source::Token>, CompileError> {
    let display_filename = path.as_ref()
        .file_name()
        .map(OsStr::to_string_lossy)
        .map(|s| String::from(s))
        .unwrap_or_else(|| "(unknown)".to_owned());

    let preprocessor = pp::Preprocessor::new(&display_filename, pp_symbols);

    let file = fs::File::open(&path)?;
    let source = preprocessor.preprocess(file)?;

    let tokens = tokenizer::tokenize(&display_filename, &source)?;

    Ok(tokens)
}

pub struct ProgramModule {
    pub program: semantic::Program,
    pub units: Vec<semantic::Unit>,
}

fn pretty_path(path: &Path) -> String {
    let canon = if path.exists() {
        path.canonicalize()
            .map(|canon_path| canon_path.to_string_lossy().into_owned())
            .unwrap_or_else(|_| "(unknown)".to_owned())
    } else {
        path.to_string_lossy().into_owned()
    };

    if cfg!(windows) && canon.starts_with(r"\\?\") {
        canon[4..].to_owned()
    } else {
        canon
    }
}

fn scope_from_uses(uses: &[node::UnitReference],
                   unit_scopes: &HashMap<String, semantic::Scope>)
                   -> Result<semantic::Scope, CompileError> {
    let mut scope = semantic::Scope::default();
    for unit_ref in uses.iter() {
        let ref_name = unit_ref.name.to_string();

        match unit_scopes.get(&ref_name) {
            Some(ref_scope) => {
                scope = scope.reference(ref_scope.clone(), unit_ref.kind.clone());
            }
            None => return Err({
                CompileError::UnresolvedUnit(ref_name.clone())
            })
        }
    }

    Ok(scope)
}

fn compile_program(program_path: &Path,
                   pp_symbols: HashSet<String>)
                   -> Result<ProgramModule, CompileError> {
    let tokens = load_source(program_path, pp_symbols.clone())?;

    let source_dir = program_path.canonicalize()?
        .parent()
        .ok_or_else(|| {
            let msg = format!("unable to resolve source directory from source path `{}`", program_path.to_string_lossy());
            io::Error::new(io::ErrorKind::NotFound, msg)
        })?
        .canonicalize()?;

    let parsed_program = syntax::Program::parse(tokens.into_iter(), &empty_context())?;

    let mut loaded_units: Vec<semantic::Unit> = Vec::new();
    let mut unit_scopes: HashMap<String, semantic::Scope> = HashMap::new();

    for unit_ref in parsed_program.uses.iter() {
        let unit_id = unit_ref.name.to_string();

        if !loaded_units.iter().any(|unit| unit.name == unit_id) {
            let unit_path = source_dir.join(format!("{}.pas", unit_id));

            println!("Compiling unit `{}` in `{}`...",
                     unit_id,
                     pretty_path(&unit_path));

            let unit_source = load_source(unit_path, pp_symbols.clone())?;
            let parsed_unit = syntax::Unit::parse(unit_source, &empty_context())?;

            /* each unit imports all the units before it in the programs' units
            clause (used units can't import any new units not referenced in the main
            uses clause) */
            let unit_scope = scope_from_uses(&parsed_unit.uses, &unit_scopes)?;

            let (unit, unit_scope) = semantic::Unit::annotate(&parsed_unit, unit_scope)?;

            unit_scopes.insert(unit.name.clone(), unit_scope);
            loaded_units.push(unit);
        }
    }

    let program_scope = scope_from_uses(&parsed_program.uses, &unit_scopes)?;

    let program = semantic::Program::annotate(&parsed_program, program_scope)?;

    Ok(ProgramModule {
        program,
        units: loaded_units,
    })
}

fn print_usage(program: &str, opts: getopts::Options) {
    let brief = format!("Usage: {} FILE [options]", program);
    print!("{}", opts.usage(&brief));
}

fn main() {
    let args = std::env::args().collect::<Vec<String>>();
    let program = args[0].clone();

    let mut opts = getopts::Options::new();
    opts.optopt("o", "outdir", "Output file (use .c extension to emit C intermediate files)", "");

    match opts.parse(&args[1..]) {
        Ok(matched) => {
            if matched.free.len() != 1 {
                print_usage(&program, opts);
                std::process::exit(1);
            } else {
                let out_file = matched.opt_str("o")
                    .map(|out_dir_str| PathBuf::from(out_dir_str))
                    .unwrap_or_else(|| current_dir().unwrap());

                let src_path = &matched.free[0];

                //todo: -D preprocessor options
                let pp_symbols = HashSet::new();

                let build_result = compile_program(&PathBuf::from(src_path), pp_symbols)
                    .and_then(|module| target_c::pas_to_c(&module, &out_file));

                if let Err(err) = build_result {
                    println!("{}", err);
                    std::process::exit(1);
                }
            }
        }
        Err(err) => {
            println!("{}", err);
            print_usage(&program, opts);

            std::process::exit(1);
        }
    }
}

