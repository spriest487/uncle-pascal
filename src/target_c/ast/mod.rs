use std::fmt;

pub mod function;
pub mod ctype;
pub mod translation_unit;
pub mod decl;
pub mod block;
pub mod class;
pub mod interface;
pub mod expression;
pub mod name;

pub use self::function::*;
pub use self::ctype::*;
pub use self::translation_unit::*;
pub use self::decl::*;
pub use self::block::*;
pub use self::class::*;
pub use self::interface::*;
pub use self::expression::*;
pub use self::name::*;

use node::Identifier;

pub type TranslationResult<T> = Result<T, TranslationError>;

#[derive(Debug)]
pub enum TranslationError {
    WriteFailed(fmt::Error),
}

impl From<fmt::Error> for TranslationError {
    fn from(err: fmt::Error) -> Self {
        TranslationError::WriteFailed(err)
    }
}

impl fmt::Display for TranslationError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TranslationError::WriteFailed(err) => write!(f, "{}", err),
        }
    }
}

/* runtime functions */
pub fn rc_getmem(struct_name: impl Into<Name>, class_name: &Identifier) -> Expression {
    Expression::function_call(Name::internal_symbol("Rc_GetMem"), vec![
        Expression::function_call(Name::internal_symbol("SizeOf"), vec![
            Expression::Name(struct_name.into())
        ]),
        Expression::string_literal(&class_name.to_string())
    ])
}

pub fn rc_retain(expr: impl Into<Expression>) -> Expression {
    Expression::function_call(
        Name::internal_symbol("Rc_Retain"),
        vec![expr.into()],
    )
}

pub fn rc_release(expr: impl Into<Expression>) -> Expression {
    Expression::function_call(
        Name::internal_symbol("Rc_Release"),
        vec![expr.into()],
    )
}

pub fn rc_retain_weak(expr: impl Into<Expression>) -> Expression {
    Expression::function_call(
        Name::internal_symbol("Rc_RetainWeak"),
        vec![expr.into()],
    )
}

pub fn rc_release_weak(expr: impl Into<Expression>) -> Expression {
    Expression::function_call(
        Name::internal_symbol("Rc_ReleaseWeak"),
        vec![expr.into()],
    )
}

pub fn rc_weak_value(expr: impl Into<Expression>) -> Expression {
    Expression::function_call(
        Name::internal_symbol("Rc_WeakValue"),
        vec![expr.into()],
    )
}