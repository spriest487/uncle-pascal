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

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum UnitReferenceKind {
    /** `uses System` adds all symbols from the unit to the scope,
        under their original namespace **/
    Namespaced,

    /** `uses System.*` adds all symbols from the unit to the scope, */
    All,

    /** `uses System.String` reference a particular name */
    Name(String),
}

#[derive(Clone, Debug)]
pub struct UnitReference {
    pub name: Identifier,
    pub kind: UnitReferenceKind,
    pub context: source::Token,
}

impl fmt::Display for UnitReference {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let name = match &self.kind {
            UnitReferenceKind::Namespaced => self.name.to_string(),
            UnitReferenceKind::All => format!("{}.*", self.name),
            UnitReferenceKind::Name(name) => format!("{}.{}", self.name, name),
        };

        write!(f, "{} ({})", name, self.context)
    }
}

#[derive(Clone, Debug)]
pub enum UnitDeclaration<TSymbol>
    where TSymbol: Symbol
{
    Function(FunctionDecl<TSymbol>),
    Type(TypeDecl<TSymbol>),
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

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum FunctionKind {
    Function,
    Constructor,
    Destructor,
}

#[derive(Debug, Clone)]
pub struct FunctionDeclBody<TSymbol>
    where TSymbol: Symbol
{
    pub local_vars: VarDecls<TSymbol>,
    pub block: Block<TSymbol>,
}

#[derive(Debug, Clone)]
pub struct FunctionDecl<TSymbol>
    where TSymbol: Symbol
{
    pub name: Identifier,
    pub context: source::Token,

    pub return_type: Option<TSymbol::Type>,
    pub kind: FunctionKind,

    pub args: VarDecls<TSymbol>,

    // a function without a body is a forward declaration
    pub body: Option<FunctionDeclBody<TSymbol>>,
}

#[derive(Clone, Debug)]
pub struct Block<TSymbol> {
    pub context: source::Token,
    pub statements: Vec<Expression<TSymbol>>,
}

#[derive(Clone, Debug)]
pub enum TypeDecl<TSymbol>
    where TSymbol: Symbol
{
    Record(RecordDecl<TSymbol>),
    Alias {
        context: source::Token,

        alias: String,
        of: TSymbol::Type,
    }
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