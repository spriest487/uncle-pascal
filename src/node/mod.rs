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
    type Type: Clone + ToSource + PartialEq + fmt::Debug;

    fn token(&self) -> &source::Token;
}

pub trait Symbol: ToSource {
    type Type: Clone + ToSource + fmt::Debug;
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum UnitReferenceKind {
    /**
        adds all symbols from the unit to the scope,
        under their original namespace

        `uses System`
    */
    Namespaced,

    /**
        adds all symbols from the unit to the scope,
        `uses System.*`
    */
    All,

    /**
        `uses System.String` reference a particular name
    */
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
    Var(VarDecl<TContext>),
    Const(ConstDecl<TContext>),
}

impl<TContext> UnitDecl<TContext>
    where TContext: Context
{
    pub fn as_class_decl(&self) -> Option<&RecordDecl<TContext>> {
        match self {
            UnitDecl::Type(TypeDecl::Record(record)) => {
                match record.kind {
                    RecordKind::Class => Some(record),
                    RecordKind::Record => None
                }
            }

            _ => None
        }
    }
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

impl<TContext> Program<TContext>
    where TContext: Context
{
    pub fn vars(&self) -> impl Iterator<Item=&VarDecl<TContext>> {
        self.decls.iter()
            .filter_map(|program_decl| match program_decl {
                Implementation::Decl(decl) => match decl {
                    UnitDecl::Var(var_decl) => Some(var_decl),
                    _ => None,
                }

                _ => None,
            })
    }
}

#[derive(Clone, Debug)]
pub struct Unit<TContext>
    where TContext: Context
{
    pub name: String,

    pub uses: Vec<UnitReference<TContext>>,

    pub interface: Vec<UnitDecl<TContext>>,
    pub implementation: Vec<Implementation<TContext>>,

    pub initialization: Option<Block<TContext>>,
    pub finalization: Option<Block<TContext>>,
}

impl<TContext> Unit<TContext>
    where TContext: Context
{
    pub fn vars(&self) -> impl Iterator<Item=&VarDecl<TContext>> {
        let interface_vars = self.interface.iter()
            .filter_map(|decl| match decl {
                UnitDecl::Var(var) => Some(var),
                _ => None,
            });

        let impl_vars = self.implementation.iter()
            .filter_map(|program_decl| match program_decl {
                Implementation::Decl(decl) => match decl {
                    UnitDecl::Var(var) => Some(var),
                    _ => None,
                }

                _ => None,
            });

        interface_vars.chain(impl_vars)
    }
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

impl<TContext> FunctionDecl<TContext>
    where TContext: Context
{
    pub fn external_name(&self) -> Option<&ExternalName> {
        self.modifiers.iter()
            .filter_map(|modifier| match modifier {
                FunctionModifier::External(name) => Some(name),
                _ => None,
            })
            .next()
    }
}

#[derive(Debug, Clone)]
pub enum FunctionLocalDecl<TContext>
    where TContext: Context,
{
    Var(VarDecl<TContext>),
    Const(ConstDecl<TContext>),
    NestedFunction(Box<Function<TContext>>),
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
                FunctionLocalDecl::Var(var) => Some(var),

                FunctionLocalDecl::NestedFunction(_) |
                FunctionLocalDecl::Const(_) => None,
            })
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Block<TContext>
    where TContext: Context
{
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

#[derive(Clone, Debug, PartialEq)]
pub struct RecordVariantCase<TContext>
    where TContext: Context
{
    pub tag_value: Expression<TContext>,
    pub members: Vec<RecordMember<TContext>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct RecordMember<TContext>
    where TContext: Context
{
    pub name: String,
    pub decl_type: TContext::Type,
    pub context: TContext
}

/*
    variable structure/union
    ```
    case tag: Int32 of
        1: (foo: String; bar: Int32)
        2: (baz: Boolean)
    ```
*/
#[derive(Clone, Debug, PartialEq)]
pub struct RecordVariantPart<TContext>
    where TContext: Context
{
    pub tag: RecordMember<TContext>,
    pub context: TContext,
    pub cases: Vec<RecordVariantCase<TContext>>,
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
    pub members: Vec<RecordMember<TContext>>,
    pub variant_part: Option<RecordVariantPart<TContext>>,
}

impl<TContext> RecordDecl<TContext>
    where TContext: Context
{
    pub fn get_member(&self, name: &str) -> Option<&RecordMember<TContext>> {
        self.members.iter()
            .find(|member| member.name.as_str() == name)
            .or_else(|| {
                let variant_part = self.variant_part.as_ref()?;

                let tag = self.variant_part.as_ref().map(|variant_part| &variant_part.tag)?;
                match name == tag.name.as_str() {
                    true => Some(tag),
                    false => {
                        variant_part.cases.iter()
                            .flat_map(|case| case.members.iter())
                            .find(|member| member.name.as_str() == name)
                    }
                }
            })
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
pub struct VarDecl<TContext>
    where TContext: Context
{
    pub name: String,
    pub context: TContext,

    pub decl_type: TContext::Type,

    pub default_value: Option<Expression<TContext>>,
}
