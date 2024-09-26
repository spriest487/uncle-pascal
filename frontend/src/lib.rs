pub mod ast;
pub mod consts;
pub mod parse;
pub mod token_tree;
pub mod typ;
pub mod pp;
pub mod emit;

use std::path::PathBuf;
use std::rc::Rc;
pub use ast::Ident;
pub use ast::keyword::Keyword;
pub use ast::operators::CompoundAssignmentOperator;
pub use ast::operators::Operator;
pub use ast::operators::Position;
use common::{BuildOptions, TracedError};
use common::span::{Location, Span};
use crate::ast::{IdentPath, Unit};
use crate::emit::{ir, IROptions};
use crate::parse::{ParseError, ParseResult};
use crate::pp::error::PreprocessorError;
use crate::pp::PreprocessedUnit;
use crate::typ::{Module, TypeResult};
pub use self::consts::EnumConstant;
pub use self::consts::IntConstant;
pub use self::consts::RealConstant;
pub use self::consts::SetConstant;
pub use self::token_tree::DelimiterPair;
pub use self::token_tree::Separator;
pub use self::token_tree::TokenTree;
pub use self::token_tree::TokenStream;
pub use self::token_tree::TokenizeError;
pub use self::token_tree::TokenizeResult;

pub fn preprocess(
    filename: impl Into<PathBuf>,
    src: &str,
    opts: BuildOptions
) -> Result<PreprocessedUnit, PreprocessorError> {
    let pp = pp::Preprocessor::new(filename, opts);
    pp.preprocess(&src)
}

pub fn tokenize(unit: PreprocessedUnit) -> TokenizeResult<Vec<TokenTree>>{
    TokenTree::tokenize(unit)
}

pub fn parse(
    filename: impl Into<PathBuf>,
    tokens: impl IntoIterator<Item=TokenTree>
) -> ParseResult<Unit<Span>> {
    let file_span = Span {
        file: Rc::new(filename.into()),
        start: Location::zero(),
        end: Location::zero()
    };

    let unit_ident = file_span.file
        .with_extension("")
        .file_name()
        .and_then(|file_name| {
            let unit_ident = IdentPath::from_parts(file_name
                .to_string_lossy()
                .split('.')
                .map(|part| Ident::new(part, file_span.clone())));

            Some(unit_ident)
        })
        .ok_or_else(|| {
            let err = ParseError::InvalidUnitFilename(file_span.clone());
            TracedError::trace(err)
        })?;

    let mut tokens = TokenStream::new(tokens, file_span); 
    let parsed_unit = ast::Unit::parse(&mut tokens, unit_ident)?;
    tokens.finish()?;

    Ok(parsed_unit)
}

pub fn typecheck(units: &[Unit<Span>]) -> TypeResult<Module> {
    Module::typecheck(units)
}

pub fn emit_ir(module: &Module, opts: IROptions) -> ir::Module {
    emit::translate(module, opts)
}
