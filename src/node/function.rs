use std::fmt;
use node::*;

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub enum FunctionArgModifier {
    Const,
    Var,
    Out,
}

impl fmt::Display for FunctionArgModifier {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            FunctionArgModifier::Const => write!(f, "const"),
            FunctionArgModifier::Var => write!(f, "var"),
            FunctionArgModifier::Out => write!(f, "out"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionArg<TContext>
    where TContext: Context
{
    pub name: String,
    pub decl_type: TContext::Type,
    pub modifier: Option<FunctionArgModifier>,
    pub context: TContext,
    pub default_value: Option<Expression<TContext>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct InterfaceImplementation<TContext>
    where TContext: Context
{
    pub interface: Identifier,
    pub for_type: TContext::Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionDecl<TContext>
    where TContext: Context
{
    pub name: String,
    pub context: TContext,

    pub return_type: Option<TContext::Type>,
    pub implements: Option<InterfaceImplementation<TContext>>,
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

    pub fn signature(&self) -> FunctionSignature<TContext::Type> {
        FunctionSignature {
            args: self.args.iter()
                .map(|decl| FunctionArgSignature {
                    decl_type: decl.decl_type.clone(),
                    modifier: decl.modifier,
                })
                .collect(),
            return_type: self.return_type.clone(),
            modifiers: self.modifiers.clone(),
        }
    }
}

impl<C> fmt::Display for FunctionDecl<C>
    where C: Context
{
    // todo: this is incomplete
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "function {};", self.name)
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

impl<C> fmt::Display for FunctionLocalDecl<C>
    where C: Context
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            FunctionLocalDecl::Var(var) => write!(f, "{}", var),
            FunctionLocalDecl::NestedFunction(func) => write!(f, "{}", func),
            FunctionLocalDecl::Const(const_decl) => write!(f, "{}", const_decl),
        }
    }
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

impl<C> fmt::Display for Function<C>
    where C: Context
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "{}", self.decl)?;

        for decl in &self.local_decls {
            writeln!(f, "{}", decl)?;
        }

        writeln!(f, "{};", self.block)
    }
}
