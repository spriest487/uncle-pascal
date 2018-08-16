pub mod identifier;
pub mod expression;
pub mod to_source;

use std::fmt;

use source;
use types::{ RecordKind };
pub use self::identifier::*;
pub use self::expression::*;
pub use self::to_source::*;

pub trait Symbol: ToSource {
    type Type: Clone + ToSource + fmt::Debug;
}

#[derive(Clone, Debug)]
pub struct UnitReference {
    pub name: Identifier,
    pub context: source::Token,
}

impl fmt::Display for UnitReference {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} ({})", self.name, self.context)
    }
}

#[derive(Clone, Debug)]
pub enum UnitDeclaration<TSymbol>
    where TSymbol: Symbol
{
    Function(FunctionDecl<TSymbol>),
    Record(RecordDecl<TSymbol>),
    Vars(VarDecls<TSymbol>),
}

#[derive(Clone, Debug)]
pub struct Program<TSymbol>
    where TSymbol: Symbol
{
    pub name: String,

    pub uses: Vec<UnitReference>,
    pub decls: Vec<UnitDeclaration<TSymbol>>,

    pub program_block: Block<TSymbol>,
}

#[derive(Clone, Debug)]
pub struct Unit<TSymbol>
    where TSymbol: Symbol
{
    pub name: String,

    pub uses: Vec<UnitReference>,

    pub interface: Vec<UnitDeclaration<TSymbol>>,
    pub implementation: Vec<UnitDeclaration<TSymbol>>,
}

#[derive(Debug, Clone)]
pub struct FunctionDecl<TSymbol>
    where TSymbol: Symbol
{
    pub name: Identifier,
    pub context: source::Token,

    pub return_type: Option<TSymbol::Type>,
    pub constructor: bool,

    pub args: VarDecls<TSymbol>,
    pub local_vars: VarDecls<TSymbol>,

    pub body: Block<TSymbol>,
}

#[derive(Clone, Debug)]
pub struct Block<TSymbol> {
    pub context: source::Token,
    pub statements: Vec<Expression<TSymbol>>,
}

#[derive(Clone, Debug)]
pub struct RecordDecl<TSymbol>
    where TSymbol: Symbol
{
    pub name: Identifier,
    pub kind: RecordKind,

    pub context: source::Token,
    pub members: Vec<VarDecl<TSymbol>>,
}

#[derive(Clone, Debug)]
pub struct VarDecl<TSymbol>
    where TSymbol: Symbol
{
    pub name: Identifier,
    pub context: source::Token,

    pub decl_type: TSymbol::Type,
}

#[derive(Clone, Debug)]
pub struct VarDecls<TSymbol>
    where TSymbol: Symbol
{
    pub decls: Vec<VarDecl<TSymbol>>,
}

impl<TSymbol> Default for VarDecls<TSymbol>
    where TSymbol: Symbol
{
    fn default() -> Self {
        Self { decls: Vec::new() }
    }
}