pub mod identifier;
pub mod type_name;
pub mod expression;
pub mod to_source;
pub mod function_signature;

use std::fmt;

use source;
pub use self::type_name::TypeName;
pub use self::identifier::*;
pub use self::expression::*;
pub use self::to_source::*;
pub use self::function_signature::*;

pub trait Context {
    type Type: Clone + ToSource + fmt::Debug;

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
pub enum UnitDecl<TContext>
    where TContext: Context
{
    Function(FunctionDecl<TContext>),
    Type(TypeDecl<TContext>),
    Vars(VarDecls<TContext>),
    Consts(ConstDecls<TContext>),
}

#[derive(Clone, Debug)]
pub enum Implementation<TContext>
    where TContext: Context
{
    Function(Function<TContext>),
    Decl(UnitDecl<TContext>),
}

#[derive(Clone, Debug)]
pub struct Program<TContext>
    where TContext: Context
{
    pub name: String,

    pub uses: Vec<UnitReference<TContext>>,
    pub decls: Vec<Implementation<TContext>>,

    pub program_block: Block<TContext>,
}

#[derive(Clone, Debug)]
pub struct Unit<TContext>
    where TContext: Context
{
    pub name: String,

    pub uses: Vec<UnitReference<TContext>>,

    pub interface: Vec<UnitDecl<TContext>>,
    pub implementation: Vec<Implementation<TContext>>,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum FunctionKind {
    Function,
    Constructor,
    Destructor,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub enum FunctionArgModifier {
    Const,
    Var,
    Out,
}

impl ToSource for FunctionArgModifier {
    fn to_source(&self) -> String {
        match self {
            FunctionArgModifier::Const => "const".to_string(),
            FunctionArgModifier::Var => "var".to_string(),
            FunctionArgModifier::Out => "out".to_string(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct FunctionArg<TContext>
    where TContext: Context
{
    pub name: String,
    pub decl_type: TContext::Type,
    pub modifier: Option<FunctionArgModifier>,
    pub context: TContext,
    pub default_value: Option<Expression<TContext>>,
}

#[derive(Debug, Clone)]
pub struct FunctionDecl<TContext>
    where TContext: Context
{
    pub name: String,
    pub context: TContext,

    pub return_type: Option<TContext::Type>,
    pub kind: FunctionKind,
    pub modifiers: Vec<FunctionModifier>,

    pub args: Vec<FunctionArg<TContext>>,
}

#[derive(Debug, Clone)]
pub enum FunctionLocalDecl<TContext>
    where TContext: Context,
{
    Vars(VarDecls<TContext>),
    Consts(ConstDecls<TContext>),
    NestedFunction(Box<FunctionDecl<TContext>>),
}

#[derive(Debug, Clone)]
pub struct Function<TContext>
    where TContext: Context
{
    pub decl: FunctionDecl<TContext>,
    pub local_decls: Vec<FunctionLocalDecl<TContext>>,
    pub block: Block<TContext>,
}

impl<TContext> Function<TContext>
    where TContext: Context
{
    pub fn local_vars(&self) -> impl Iterator<Item=&VarDecl<TContext>> {
        self.local_decls.iter()
            .filter_map(|decl| match decl {
                FunctionLocalDecl::Vars(vars) => Some(vars.decls.iter()),

                FunctionLocalDecl::NestedFunction(_) |
                FunctionLocalDecl::Consts(_) => None,
            })
            .flat_map(|var_decls| var_decls)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Block<TContext> {
    pub context: TContext,
    pub statements: Vec<Expression<TContext>>,
}

#[derive(Clone, Debug)]
pub enum TypeDecl<TContext>
    where TContext: Context
{
    Record(RecordDecl<TContext>),
    Enumeration(EnumerationDecl<TContext>),
    Set(SetDecl<TContext>),
    Alias {
        context: TContext,

        alias: String,
        of: TContext::Type,
    },
}

#[derive(Clone, Debug)]
pub struct EnumerationDecl<TContext> {
    pub name: String,

    pub names: Vec<String>,
    pub context: TContext,
}

#[derive(Clone, Debug)]
pub struct SetDecl<TContext> {
    pub name: String,
    pub context: TContext,
    pub enumeration: SetEnumeration,
}

#[derive(Clone, Debug)]
pub enum SetEnumeration {
    Named(Identifier),
    Inline(Vec<String>),
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub enum RecordKind {
    Record,
    Class,
}

#[derive(Clone, Debug)]
pub struct RecordDecl<TContext>
    where TContext: Context
{
    pub name: String,
    pub kind: RecordKind,

    pub context: TContext,
    pub members: Vec<VarDecl<TContext>>,
}

impl<TContext> RecordDecl<TContext>
    where TContext: Context
{
    pub fn get_member(&self, name: &str) -> Option<&VarDecl<TContext>> {
        self.members.iter()
            .find(|member| member.name.to_string() == name)
    }
}

#[derive(Clone, Debug)]
pub struct ConstDecl<TContext>
    where TContext: Context
{
    pub name: String,
    pub value: Expression<TContext>,
    pub decl_type: Option<TContext::Type>,
    pub context: TContext,
}

#[derive(Clone, Debug)]
pub struct ConstDecls<TContext>
    where TContext: Context
{
    pub decls: Vec<ConstDecl<TContext>>,
}

impl<TContext> Default for ConstDecls<TContext>
    where TContext: Context
{
    fn default() -> Self {
        ConstDecls { decls: Vec::new() }
    }
}

#[derive(Clone, Debug)]
pub struct VarDecl<TContext>
    where TContext: Context
{
    pub name: String,
    pub context: TContext,

    pub decl_type: TContext::Type,

    pub default_value: Option<Expression<TContext>>,
}

#[derive(Clone, Debug)]
pub struct VarDecls<TContext>
    where TContext: Context
{
    pub decls: Vec<VarDecl<TContext>>,
}

impl<TContext> Default for VarDecls<TContext>
    where TContext: Context
{
    fn default() -> Self {
        VarDecls { decls: Vec::new() }
    }
}

