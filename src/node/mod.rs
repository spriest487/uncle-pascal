pub mod identifier;
pub mod expression;

use std::fmt;
use std::collections::HashSet;

use source;
pub use self::identifier::*;
pub use self::expression::*;

pub trait ToSource {
    fn to_source(&self) -> String;
}

pub trait Symbol {
    type Type: Clone + fmt::Debug;
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


#[derive(Debug, Clone)]
pub struct FunctionDecl<TSymbol>
    where TSymbol: Symbol
{
    pub name: String,
    pub context: source::Token,

    pub return_type: Option<TSymbol::Type>,

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
    pub name: String,
    pub context: source::Token,
    pub members: Vec<VarDecl<TSymbol>>,
}

#[derive(Clone, Debug, Copy, Hash, Eq, PartialEq)]
pub enum VarModifier {
    Pointer,
}

#[derive(Clone, Debug)]
pub struct VarDecl<TSymbol>
    where TSymbol: Symbol
{
    pub name: String,
    pub context: source::Token,

    pub decl_type: TSymbol::Type,
    pub modifiers: HashSet<VarModifier>,
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