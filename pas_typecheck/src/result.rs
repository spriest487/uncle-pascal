use crate::annotation::TypeAnnotation;
use crate::{
    ast::{Call, Expression, Variant},
    context::NameError,
    Type, ValueKind,
};
use pas_common::{span::*, Backtrace, DiagnosticLabel, DiagnosticMessage, DiagnosticOutput};
use pas_syn::parse::InvalidStatement;
use pas_syn::{ast, Ident, Operator};
use std::fmt;

#[derive(Debug)]
pub enum TypecheckError {
    ScopeError(NameError),
    NotCallable(Box<Expression>),
    InvalidArgs {
        expected: Vec<Type>,
        actual: Vec<Type>,
        span: Span,
    },
    InvalidCallInExpression(Call),
    InvalidIndexer {
        base: Box<Expression>,
        index_ty: Type,
        span: Span,
    },
    TypeMismatch {
        expected: Type,
        actual: Type,
        span: Span,
    },
    NotMutable {
        expr: Box<Expression>,
        decl: Option<Span>,
    },
    NotAddressable {
        ty: Type,
        value_kind: Option<ValueKind>,
        span: Span,
    },
    NotDerefable {
        ty: Type,
        span: Span,
    },
    NotMatchable {
        ty: Type,
        span: Span,
    },
    InvalidUnaryOp {
        op: Operator,
        operand: Type,
        span: Span,
    },
    InvalidBinOp {
        lhs: Type,
        rhs: Type,
        op: Operator,
        span: Span,
    },
    InvalidBlockOutput(Box<Expression>),
    AmbiguousMethod {
        method: Ident,
        span: Span,
    },
    InvalidCtorType {
        ty: Type,
        span: Span,
    },
    UndefinedSymbols {
        unit: Ident,
        syms: Vec<Ident>,
    },
    UnableToInferType {
        expr: ast::Expression<Span>,
    },
    UninitBindingWithNoType {
        binding: ast::LocalBinding<Span>,
    },
    BindingWithNoType {
        binding: ast::LocalBinding<Span>,
    },
    NotInitialized {
        ident: Ident,
        usage: Span,
    },
    InvalidRefExpression {
        expr: Box<Expression>,
    },
    InvalidStatement(Box<InvalidStatement<TypeAnnotation>>),
    EmptyVariant(Box<ast::Variant<Span>>),
    EmptyVariantCaseBinding {
        variant: Box<Variant>,
        case_index: usize,
        span: Span,
    },
}

pub type TypecheckResult<T> = Result<T, TypecheckError>;

impl From<NameError> for TypecheckError {
    fn from(err: NameError) -> Self {
        TypecheckError::ScopeError(err)
    }
}

impl Spanned for TypecheckError {
    fn span(&self) -> &Span {
        match self {
            TypecheckError::ScopeError(err) => err.span(),
            TypecheckError::NotCallable(expr) => expr.annotation().span(),
            TypecheckError::InvalidArgs { span, .. } => span,
            TypecheckError::InvalidCallInExpression(call) => call.annotation().span(),
            TypecheckError::InvalidIndexer { span, .. } => span,
            TypecheckError::TypeMismatch { span, .. } => span,
            TypecheckError::NotMutable { expr, .. } => expr.annotation().span(),
            TypecheckError::NotAddressable { span, .. } => span,
            TypecheckError::NotDerefable { span, .. } => span,
            TypecheckError::NotMatchable { span, .. } => span,
            TypecheckError::InvalidBinOp { span, .. } => span,
            TypecheckError::InvalidUnaryOp { span, .. } => span,
            TypecheckError::InvalidBlockOutput(expr) => expr.annotation().span(),
            TypecheckError::AmbiguousMethod { span, .. } => span,
            TypecheckError::InvalidCtorType { span, .. } => span,
            TypecheckError::UndefinedSymbols { unit, .. } => unit.span(),
            TypecheckError::UnableToInferType { expr } => expr.annotation().span(),
            TypecheckError::UninitBindingWithNoType { binding } => binding.annotation.span(),
            TypecheckError::BindingWithNoType { binding } => binding.annotation.span(),
            TypecheckError::NotInitialized { usage, .. } => usage.span(),
            TypecheckError::InvalidRefExpression { expr } => expr.annotation().span(),
            TypecheckError::InvalidStatement(expr) => expr.0.annotation().span(),
            TypecheckError::EmptyVariant(variant) => variant.span(),
            TypecheckError::EmptyVariantCaseBinding { span, .. } => span,
        }
    }
}

impl DiagnosticOutput for TypecheckError {
    fn title(&self) -> String {
        match self {
            TypecheckError::ScopeError(err) => err.title(),
            TypecheckError::NotCallable(_) => "Not callable".to_string(),
            TypecheckError::InvalidArgs { .. } => "Invalid arguments".to_string(),
            TypecheckError::InvalidCallInExpression(_) => "Invalid call in expression".to_string(),
            TypecheckError::InvalidIndexer { .. } => "Invalid indexer expression".to_string(),
            TypecheckError::TypeMismatch { .. } => "Type mismatch".to_string(),
            TypecheckError::NotMutable { .. } => "Value not mutable".to_string(),
            TypecheckError::NotAddressable { .. } => "Value not addressable".to_string(),
            TypecheckError::NotDerefable { .. } => "Value cannot be dereferenced".to_string(),
            TypecheckError::NotMatchable { .. } => "Type is not matchable".to_string(),
            TypecheckError::InvalidBinOp { .. } => "Invalid binary operation".to_string(),
            TypecheckError::InvalidUnaryOp { .. } => "Invalid unary operation".to_string(),
            TypecheckError::InvalidBlockOutput(_) => "Invalid block output expression".to_string(),
            TypecheckError::AmbiguousMethod { .. } => "Method reference is ambiguous".to_string(),
            TypecheckError::InvalidCtorType { .. } => {
                "Invalid constructor expression type".to_string()
            }
            TypecheckError::UndefinedSymbols { .. } => "Undefined symbol(s)".to_string(),
            TypecheckError::UnableToInferType { .. } => {
                "Unable to infer type of expression".to_string()
            }
            TypecheckError::UninitBindingWithNoType { .. } => {
                "Uninitialized binding must have an explicit type".to_string()
            }
            TypecheckError::BindingWithNoType { .. } => {
                "Value bound to name must have a type".to_string()
            }
            TypecheckError::NotInitialized { .. } => "Use of uninitialized value".to_string(),
            TypecheckError::InvalidRefExpression { .. } => {
                "Invalid reference expression".to_string()
            }
            TypecheckError::InvalidStatement(invalid_stmt) => invalid_stmt.title(),
            TypecheckError::EmptyVariant(..) => "Empty variant".to_string(),
            TypecheckError::EmptyVariantCaseBinding { .. } => {
                "Empty variant case binding".to_string()
            }
        }
    }

    fn label(&self) -> Option<DiagnosticLabel> {
        match self {
            TypecheckError::ScopeError(err) => err.label(),
            TypecheckError::UndefinedSymbols { .. } => None,
            _ => Some(DiagnosticLabel {
                text: Some(self.to_string()),
                span: self.span().clone(),
            }),
        }
    }

    fn see_also(&self) -> Vec<DiagnosticMessage> {
        match self {
            TypecheckError::ScopeError(err) => err.see_also(),

            TypecheckError::UndefinedSymbols { syms, .. } => syms
                .iter()
                .map(|sym| DiagnosticMessage {
                    title: format!("`{}` is declared but not defined", sym),
                    label: Some(DiagnosticLabel {
                        text: Some("declared here".to_string()),
                        span: sym.span().clone(),
                    }),
                })
                .collect(),

            TypecheckError::NotInitialized { ident, .. } => vec![DiagnosticMessage {
                title: format!("uninitialized value `{}`", ident),
                label: Some(DiagnosticLabel {
                    text: Some("declared here".to_string()),
                    span: ident.span().clone(),
                }),
            }],

            TypecheckError::NotMutable {
                decl: Some(decl_span),
                ..
            } => vec![DiagnosticMessage {
                title: format!("modifying immutable value"),
                label: Some(DiagnosticLabel {
                    text: Some("declared as immutable here".to_string()),
                    span: decl_span.clone(),
                }),
            }],

            _ => Vec::new(),
        }
    }

    fn backtrace(&self) -> Option<&Backtrace> {
        None
    }
}

fn write_args<'a>(f: &mut fmt::Formatter, args: impl IntoIterator<Item = &'a Type>) -> fmt::Result {
    for (i, arg) in args.into_iter().enumerate() {
        if i > 0 {
            write!(f, ", ")?;
        }
        write!(f, "{}", arg)?;
    }
    Ok(())
}

impl fmt::Display for TypecheckError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TypecheckError::ScopeError(err) => write!(f, "{}", err),
            TypecheckError::NotCallable(expr) => {
                write!(f, "expression `{}` is not a callable function", expr)
            }

            TypecheckError::InvalidArgs { expected, actual, .. } => {
                if expected.len() != actual.len() {
                    write!(f, "expected {} args, got {}", expected.len(), actual.len())
                } else {
                    write!(f, "expected arguments (")?;
                    write_args(f, expected.iter())?;
                    write!(f, "), found (")?;
                    write_args(f, actual.iter())?;
                    write!(f, ")")
                }
            }

            TypecheckError::InvalidCallInExpression(call) => {
                write!(f, "function call `{}` returns no value and cannot be used as part of an expression", call)
            }

            TypecheckError::InvalidIndexer { base, index_ty, .. } => {
                write!(f, "expression `{}` cannot be indexed with an index of type `{}`", base, index_ty)
            }

            TypecheckError::TypeMismatch { expected, actual, .. } => {
                write!(f, "type mismatch: expected {}, found {}", expected, actual)
            }

            TypecheckError::NotMutable { expr, .. } => {
                write!(f, "`{}` does not refer to a mutable value", expr)
            }

            TypecheckError::NotAddressable { ty, value_kind, .. } => {
                match value_kind {
                    Some(value_kind) => write!(f, "{} of type {} cannot have its address taken", value_kind, ty),
                    None => write!(f, "expression without a value cannot have its address taken"),
                }
            }

            TypecheckError::NotDerefable { ty, .. } => {
                write!(f, "value of type {} cannot be dereferenced", ty)
            }

            TypecheckError::NotMatchable { ty, .. } => {
                write!(f, "type {} cannot be used in matching constructs", ty)
            }

            TypecheckError::InvalidBinOp { lhs, rhs, op, .. } => {
                write!(f, "operator {} cannot be applied to the operand types {} and {}", op, lhs, rhs)
            }

            TypecheckError::InvalidUnaryOp { operand, op, .. } => {
                write!(f, "operator {} cannot be applied to an operand of type {}", op, operand)
            }

            TypecheckError::InvalidBlockOutput(expr) => {
                write!(f, "expression `{}` is not valid as the final expression in a block because it is not a complete statement", expr)
            }

            TypecheckError::AmbiguousMethod { method, .. } => {
                // todo: show ambiguous options
                write!(f, "call to method `{}` was ambiguous", method)
            }

            TypecheckError::InvalidCtorType { ty, .. } => {
                write!(f, "type `{}` cannot be created with a constructor expression", ty)
            }

            TypecheckError::UndefinedSymbols { unit, .. } => {
                write!(f, "unit `{}` contains undefined symbols", unit)
            }

            TypecheckError::UnableToInferType { expr } => {
                write!(f, "unable to infer the type of `{}`", expr)
            }

            TypecheckError::UninitBindingWithNoType { binding } => {
                write!(f, "the type of `{}` cannot be inferred because it has no initial value", binding.name)
            }

            TypecheckError::BindingWithNoType { binding } => {
                write!(f, "the type of value bound to `{}` cannot be Nothing", binding.name)
            }

            TypecheckError::NotInitialized { ident, .. } => {
                write!(f, "`{}` is used before initialization", ident)
            }

            TypecheckError::InvalidRefExpression { expr } => {
                write!(f, "`{}` does not refer to a local variable that can be passed by reference", expr)
            }

            TypecheckError::InvalidStatement(invalid_stmt) => {
                write!(f, "{}", invalid_stmt)
            }

            TypecheckError::EmptyVariant(variant) => {
                write!(f, "variant `{}` has no cases", variant.ident)
            }

            TypecheckError::EmptyVariantCaseBinding { variant, case_index, .. } => {
                let case_ident = &variant.cases[*case_index].ident;
                write!(f, "cannot bind value of empty variant case `{}.{}`", variant.ident, case_ident)
            }
        }
    }
}
