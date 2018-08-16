use std::fmt;

use node;
use operators;

pub trait Symbol {
    type Type;
}

#[derive(Eq, PartialEq, Clone, Debug, Hash)]
pub struct Identifier {
    pub namespace: Vec<String>,
    pub name: String,
}

impl node::Symbol for Identifier {
    type Type = Self;
}

impl fmt::Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.namespace.len() > 0 {
            write!(f, "{}.", self.namespace.join("."))?;
        }
        write!(f, "{}", self.name)
    }
}

impl Identifier {
    pub fn parse(source: &str) -> Self {
        let mut parts: Vec<String> = source.split('.')
            .map(|part: &str| part.to_owned())
            .collect();

        let name = parts.pop().unwrap_or(String::new());

        Self { namespace: parts, name }
    }
}

#[derive(Clone, Debug)]
pub struct Program<TSymbol>
    where TSymbol: Symbol + Clone + fmt::Debug,
          TSymbol::Type: Clone + fmt::Debug
{
    pub name: String,

    pub uses: Vec<node::Identifier>,

    pub functions: Vec<Function<TSymbol>>,
    pub type_decls: Vec<RecordDecl<TSymbol>>,
    pub vars: Vars<TSymbol>,

    pub program_block: Block<TSymbol>,
}

#[derive(Clone, Debug)]
pub enum Expression<TSymbol>
    where TSymbol: Symbol + Clone + fmt::Debug,
          TSymbol::Type: Clone + fmt::Debug
{
    BinaryOperator {
        lhs: Box<Expression<TSymbol>>,
        op: operators::BinaryOperator,
        rhs: Box<Expression<TSymbol>>,
    },
    FunctionCall {
        target: TSymbol,
        args: Vec<Expression<TSymbol>>,
    },
    LiteralInteger(i64),
    LiteralString(String),
    Identifier(TSymbol),
    If {
        condition: Box<Expression<TSymbol>>,
        then_branch: Box<Expression<TSymbol>>,
        else_branch: Option<Box<Expression<TSymbol>>>,
    },
    Block(Block<TSymbol>),
}

#[allow(dead_code)]
impl<TSymbol> Expression<TSymbol>
    where TSymbol: Symbol + Clone + fmt::Debug,
          TSymbol::Type: Clone + fmt::Debug
{
    pub fn binary_op(lhs: Self, op: operators::BinaryOperator, rhs: Self) -> Self {
        Expression::BinaryOperator {
            lhs: Box::new(lhs),
            op,
            rhs: Box::new(rhs),
        }
    }

    pub fn function_call<TIter>(target: TSymbol, args: TIter) -> Self
        where TIter: IntoIterator<Item=Self>
    {
        Expression::FunctionCall {
            target,
            args: args.into_iter().collect(),
        }
    }

    pub fn literal_int(i: i64) -> Self {
        Expression::LiteralInteger(i)
    }

    pub fn literal_string(s: &str) -> Self {
        Expression::LiteralString(s.to_owned())
    }

    pub fn identifier(id: TSymbol) -> Self {
        Expression::Identifier(id)
    }

    pub fn if_then_else(condition: Self, then_branch: Self, else_branch: Option<Self>) -> Self {
        Expression::If {
            condition: Box::new(condition),
            then_branch: Box::new(then_branch),
            else_branch: else_branch.map(Box::new),
        }
    }

    pub fn block(block: Block<TSymbol>) -> Self {
        Expression::Block(block)
    }

    pub fn unwrap_function_call(self) -> (TSymbol, Vec<Self>) {
        match self {
            Expression::FunctionCall { target, args } => (target, args),
            _ => panic!("called unwrap_function_call on something other than a function call expr"),
        }
    }

    pub fn is_function_call(&self) -> bool {
        match self {
            &Expression::FunctionCall { .. } => true,
            _ => false,
        }
    }

    pub fn unwrap_literal_string(self) -> String {
        match self {
            Expression::LiteralString(s) => s,
            _ => panic!("called unwrap_literal_string on something other than a string literal expr")
        }
    }

    pub fn is_literal_string(&self) -> bool {
        match self {
            &Expression::LiteralString(_) => true,
            _ => false,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Function<TSymbol>
    where TSymbol: Symbol + Clone + fmt::Debug,
          TSymbol::Type: Clone + fmt::Debug
{
    pub name: String,
    pub return_type: TSymbol::Type,

    pub args: Vars<TSymbol>,
    pub local_vars: Vars<TSymbol>,

    pub body: Block<TSymbol>,
}

#[derive(Clone, Debug)]
pub struct Block<TSymbol>
    where TSymbol: Symbol + Clone + fmt::Debug,
          TSymbol::Type: Clone + fmt::Debug
{
    pub statements: Vec<Expression<TSymbol>>
}

#[derive(Clone, Debug)]
pub struct RecordDecl<TSymbol>
    where TSymbol: Symbol + Clone + fmt::Debug,
          TSymbol::Type: Clone + fmt::Debug
{
    pub name: String,
    pub members: Vec<VarDecl<TSymbol>>,
}

#[derive(Clone, Debug)]
pub struct VarDecl<TSymbol>
    where TSymbol: Symbol + Clone + fmt::Debug,
          TSymbol::Type: Clone + fmt::Debug
{
    pub name: String,
    pub decl_type: TSymbol::Type,
}

#[derive(Clone, Debug)]
pub struct Vars<TSymbol>
    where TSymbol: Symbol + Clone + fmt::Debug,
          TSymbol::Type: Clone + fmt::Debug
{
    pub decls: Vec<VarDecl<TSymbol>>,
}

impl<TSymbol> Default for Vars<TSymbol>
    where TSymbol: Symbol + Clone + fmt::Debug,
          TSymbol::Type: Clone + fmt::Debug
{
    fn default() -> Self {
        Self { decls: Vec::new() }
    }
}