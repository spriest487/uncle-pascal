pub mod identifier;
pub mod type_name;
pub mod expression;
pub mod to_source;

use std::fmt;

use source;
pub use self::type_name::{TypeName, IndexRange};
pub use self::identifier::*;
pub use self::expression::*;
pub use self::to_source::*;

pub trait Context {
    fn token(&self) -> &source::Token;
}

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
pub struct UnitReference<TContext>
    where TContext: Context
{
    pub name: Identifier,
    pub kind: UnitReferenceKind,
    pub context: TContext,
}

impl<TContext> fmt::Display for UnitReference<TContext>
    where TContext: Context
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let name = match &self.kind {
            UnitReferenceKind::Namespaced => self.name.to_string(),
            UnitReferenceKind::All => format!("{}.*", self.name),
            UnitReferenceKind::Name(name) => format!("{}.{}", self.name, name),
        };

        write!(f, "{} ({})", name, self.context.token())
    }
}

#[derive(Clone, Debug)]
pub enum UnitDeclaration<TSymbol, TContext>
    where TSymbol: Symbol,
          TContext: Context
{
    Function(FunctionDecl<TSymbol, TContext>),
    Type(TypeDecl<TSymbol, TContext>),
    Vars(VarDecls<TSymbol, TContext>),
}

#[derive(Clone, Debug)]
pub struct Program<TSymbol, TContext>
    where TSymbol: Symbol,
          TContext: Context
{
    pub name: String,

    pub uses: Vec<UnitReference<TContext>>,
    pub decls: Vec<UnitDeclaration<TSymbol, TContext>>,

    pub program_block: Block<TSymbol, TContext>,
}

#[derive(Clone, Debug)]
pub struct Unit<TSymbol, TContext>
    where TSymbol: Symbol,
          TContext: Context
{
    pub name: String,

    pub uses: Vec<UnitReference<TContext>>,

    pub interface: Vec<UnitDeclaration<TSymbol, TContext>>,
    pub implementation: Vec<UnitDeclaration<TSymbol, TContext>>,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum FunctionKind {
    Function,
    Constructor,
    Destructor,
}

#[derive(Debug, Clone)]
pub struct FunctionDeclBody<TSymbol, TContext>
    where TSymbol: Symbol,
          TContext: Context
{
    pub local_vars: VarDecls<TSymbol, TContext>,
    pub block: Block<TSymbol, TContext>,
}

#[derive(Debug, Clone)]
pub struct FunctionDecl<TSymbol, TContext>
    where TSymbol: Symbol,
          TContext: Context
{
    pub name: Identifier,
    pub context: TContext,

    pub return_type: Option<TSymbol::Type>,
    pub kind: FunctionKind,

    pub args: VarDecls<TSymbol, TContext>,

    // a function without a body is a forward declaration
    pub body: Option<FunctionDeclBody<TSymbol, TContext>>,
}

#[derive(Clone, Debug)]
pub struct Block<TSymbol, TContext> {
    pub context: TContext,
    pub statements: Vec<Expression<TSymbol, TContext>>,
}

#[derive(Clone, Debug)]
pub enum TypeDecl<TSymbol, TContext>
    where TSymbol: Symbol,
          TContext: Context
{
    Record(RecordDecl<TSymbol, TContext>),
    Alias {
        context: TContext,

        alias: String,
        of: TSymbol::Type,
    },
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub enum RecordKind {
    Record,
    Class,
}

#[derive(Clone, Debug)]
pub struct RecordDecl<TSymbol, TContext>
    where TSymbol: Symbol,
          TContext: Context
{
    pub name: Identifier,
    pub kind: RecordKind,

    pub context: TContext,
    pub members: Vec<VarDecl<TSymbol, TContext>>,
}

impl<TSymbol, TContext> RecordDecl<TSymbol, TContext>
    where TSymbol: Symbol,
          TContext: Context
{
    pub fn get_member(&self, name: &str) -> Option<&VarDecl<TSymbol, TContext>> {
        self.members.iter()
            .find(|member| member.name.to_string() == name)
    }
}

#[derive(Clone, Debug)]
pub struct VarDecl<TSymbol, TContext>
    where TSymbol: Symbol
{
    pub name: Identifier,
    pub context: TContext,

    pub decl_type: TSymbol::Type,
}

#[derive(Clone, Debug)]
pub struct VarDecls<TSymbol, TContext>
    where TSymbol: Symbol
{
    pub decls: Vec<VarDecl<TSymbol, TContext>>,
}

impl<TSymbol, TContext> Default for VarDecls<TSymbol, TContext>
    where TSymbol: Symbol
{
    fn default() -> Self {
        Self { decls: Vec::new() }
    }
}