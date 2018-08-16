pub mod identifier;
pub mod type_name;
pub mod expression;
pub mod function_signature;
mod unit;
mod function;
mod block;
mod type_decl;

pub use self::type_name::TypeName;
pub use self::identifier::*;
pub use self::expression::*;
pub use self::function_signature::*;
pub use self::unit::*;
pub use self::function::*;
pub use self::block::*;
pub use self::type_decl::*;

use std::{
    fmt,
};

use source;

pub trait Context: PartialEq {
    type Type: Clone + PartialEq + fmt::Debug + fmt::Display;

    fn token(&self) -> &source::Token;
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

impl<TContext> fmt::Display for ConstDecl<TContext>
    where TContext: Context
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "const {} = {};", self.name, self.value)
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

impl<C> fmt::Display for VarDecl<C>
    where C: Context
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "var {}: {};", self.name, self.decl_type)
    }
}