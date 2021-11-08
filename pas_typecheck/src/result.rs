use crate::{
    annotation::TypeAnnotation,
    ast::{Call, Expression, Variant, OverloadCandidate},
    context::NameError,
    GenericError, Type, ValueKind,
};
use pas_common::{span::*, Backtrace, DiagnosticLabel, DiagnosticMessage, DiagnosticOutput};
use pas_syn::{ast, parse::InvalidStatement, Ident, IdentPath, Operator};
use std::fmt;
use crate::ast::Statement;

#[derive(Debug)]
pub enum TypecheckError {
    ScopeError(NameError),
    NotCallable(Box<Expression>),
    InvalidArgs {
        expected: Vec<Type>,
        actual: Vec<Type>,
        span: Span,
    },
    InvalidCallInExpression(Box<Call>),
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
    BlockOutputIsNotExpression {
        stmt: Box<Statement>,
        expected_expr_ty: Type,
    },
    InvalidBlockOutput(Box<Expression>),
    AmbiguousMethod {
        method: Ident,
        span: Span,
    },
    AmbiguousSelfType {
        method: Ident,
        iface: Type,
        span: Span,
    },
    AmbiguousFunction {
        candidates: Vec<OverloadCandidate>,
        span: Span,
    },
    ExternalGenericFunction {
        func: Ident,
        extern_modifier: Span,
        ty_args: Span,
    },
    InvalidCtorType {
        ty: Type,
        span: Span,
    },
    DuplicateNamedArg {
        name: Ident,
        previous: Span,
        span: Span,
    },
    UndefinedSymbols {
        unit: Ident,
        syms: Vec<Ident>,
    },
    UnableToInferType {
        expr: Box<ast::Expression<Span>>,
    },
    UninitBindingWithNoType {
        binding: Box<ast::LocalBinding<Span>>,
    },
    BindingWithNoType {
        binding: Box<ast::LocalBinding<Span>>,
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
    NoLoopContext {
        stmt: Box<ast::Statement<Span>>,
    },
    NoFunctionContext {
        stmt: Box<ast::Statement<Span>>,
    },

    GenericError(GenericError),

    UnsizedMember {
        decl: IdentPath,
        member: Ident,
        member_ty: Type,
    },

    InvalidMethodInterface {
        ty: Type,
        span: Span,
    },
    InterfaceNotImplemented {
        self_ty: Type,
        iface_ty: Type,
        span: Span,
    },

    Private {
        name: IdentPath,
        span: Span,
    },
    PrivateConstructor {
        ty: Type,
        span: Span,
    },

    UnsafeConversionNotAllowed {
        from: Type,
        to: Type,
        span: Span,
    },
    UnsafeAddressoOfNotAllowed {
        ty: Type,
        span: Span,
    }
}

pub type TypecheckResult<T> = Result<T, TypecheckError>;

impl From<NameError> for TypecheckError {
    fn from(err: NameError) -> Self {
        match err {
            NameError::GenericError(err) => TypecheckError::from(err),
            err => TypecheckError::ScopeError(err),
        }
    }
}

impl From<GenericError> for TypecheckError {
    fn from(err: GenericError) -> Self {
        TypecheckError::GenericError(err)
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
            TypecheckError::BlockOutputIsNotExpression { stmt, .. } => stmt.annotation().span(),
            TypecheckError::InvalidBlockOutput(expr) => expr.annotation().span(),
            TypecheckError::AmbiguousFunction { span, .. } => span,
            TypecheckError::AmbiguousMethod { span, .. } => span,
            TypecheckError::ExternalGenericFunction { func, .. } => func.span(),
            TypecheckError::AmbiguousSelfType { span, .. } => span,
            TypecheckError::InvalidCtorType { span, .. } => span,
            TypecheckError::DuplicateNamedArg { span, .. } => span,
            TypecheckError::UndefinedSymbols { unit, .. } => unit.span(),
            TypecheckError::UnableToInferType { expr } => expr.annotation().span(),
            TypecheckError::UninitBindingWithNoType { binding } => binding.annotation.span(),
            TypecheckError::BindingWithNoType { binding } => binding.annotation.span(),
            TypecheckError::NotInitialized { usage, .. } => usage.span(),
            TypecheckError::InvalidRefExpression { expr } => expr.annotation().span(),
            TypecheckError::InvalidStatement(expr) => expr.0.annotation().span(),
            TypecheckError::EmptyVariant(variant) => variant.span(),
            TypecheckError::EmptyVariantCaseBinding { span, .. } => span,
            TypecheckError::NoLoopContext { stmt, .. } => stmt.annotation().span(),
            TypecheckError::NoFunctionContext { stmt, .. } => stmt.annotation().span(),
            TypecheckError::UnsizedMember { member, .. } => member.span(),
            TypecheckError::GenericError(err) => err.span(),
            TypecheckError::InvalidMethodInterface { span, .. } => span,
            TypecheckError::InterfaceNotImplemented { span, .. } => span,
            TypecheckError::Private { span, .. } => span,
            TypecheckError::PrivateConstructor { span, .. } => span,
            TypecheckError::UnsafeConversionNotAllowed { span, .. } => span,
            TypecheckError::UnsafeAddressoOfNotAllowed { span, .. } => span,
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
            TypecheckError::BlockOutputIsNotExpression { .. } => "Expected block output expression".to_string(),
            TypecheckError::InvalidBlockOutput(_) => "Invalid block output expression".to_string(),
            TypecheckError::AmbiguousFunction { .. } => "Function reference is ambiguous".to_string(),
            TypecheckError::AmbiguousMethod { .. } => "Method reference is ambiguous".to_string(),
            TypecheckError::AmbiguousSelfType { .. } => "Self type of method is ambiguous".to_string(),
            TypecheckError::ExternalGenericFunction { .. } => "Function imported from external module may not have type parameters".to_string(),
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

            TypecheckError::NoLoopContext { .. } => "Statement requires loop context".to_string(),
            TypecheckError::NoFunctionContext { .. } => "Statement requires function context".to_string(),

            TypecheckError::UnsizedMember { .. } => "Unsized member".to_string(),

            TypecheckError::GenericError(err) => err.title(),

            TypecheckError::InvalidMethodInterface { .. } => {
                "Invalid interface type for method".to_string()
            }

            TypecheckError::InterfaceNotImplemented { .. } => {
                "Interface not implemented".to_string()
            }

            TypecheckError::Private { .. } => "Name not exported".to_string(),

            TypecheckError::PrivateConstructor { .. } => "Type has private constructor".to_string(),
            TypecheckError::DuplicateNamedArg { .. } => "Duplicate named argument".to_string(),
            TypecheckError::UnsafeConversionNotAllowed { .. } => "Conversion not allowed in a safe context".to_string(),
            TypecheckError::UnsafeAddressoOfNotAllowed { .. } => "Address operator not allowed on this type in a safe context".to_string(),
        }
    }

    fn label(&self) -> Option<DiagnosticLabel> {
        match self {
            TypecheckError::ScopeError(err) => err.label(),
            TypecheckError::GenericError(err) => err.label(),
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
            TypecheckError::GenericError(err) => err.see_also(),

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
                title: "modifying immutable value".to_string(),
                label: Some(DiagnosticLabel {
                    text: Some("declared as immutable here".to_string()),
                    span: decl_span.clone(),
                }),
            }],

            TypecheckError::DuplicateNamedArg { name, previous, .. } => vec![DiagnosticMessage {
                title: format!("previous occurrence of `{}`", name),
                label: Some(DiagnosticLabel {
                    text: None,
                    span: previous.clone(),
                }),
            }],

            TypecheckError::AmbiguousFunction { candidates, .. } => {
                candidates.iter()
                    .map(|c| {
                        DiagnosticMessage {
                            label: Some(DiagnosticLabel {
                                span: c.span().clone(),
                                text: None,
                            }),
                            title: format!("may refer to {}", c),
                        }
                    })
                    .collect()
            }

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
                write!(f, "expression `{}` of type `{}` is not a callable function", expr, expr.annotation().ty())
            }

            TypecheckError::InvalidArgs { expected, actual, .. } => {
                write!(f, "expected arguments (")?;
                write_args(f, expected.iter())?;
                write!(f, "), found (")?;
                write_args(f, actual.iter())?;
                write!(f, ")")
            }

            TypecheckError::InvalidCallInExpression(call) => {
                write!(f, "function call `{}` returns no value and cannot be used as part of an expression", call)
            }

            TypecheckError::InvalidIndexer { base, index_ty, .. } => {
                let base_ty = base.annotation().ty();
                write!(f, "`{}` cannot be used as an index for `{}` of type `{}`", index_ty, base, base_ty)
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
                match op {
                    Operator::Assignment => {
                        write!(f, "{} is not assignable to {}", rhs, lhs)
                    },
                    _ => {
                        write!(f, "operator {} cannot be applied to the operand types {} and {}", op, lhs, rhs)
                    }
                }
            }

            TypecheckError::InvalidUnaryOp { operand, op, .. } => {
                write!(f, "operator {} cannot be applied to an operand of type {}", op, operand)
            }

            TypecheckError::BlockOutputIsNotExpression { stmt, expected_expr_ty } => {
                write!(f, "expected expression of type `{}` but found statement `{}`", expected_expr_ty, stmt)
            }

            TypecheckError::InvalidBlockOutput(expr) => {
                write!(f, "expression `{}` is not valid as the final expression in a block because it is not a complete statement", expr)
            }

            TypecheckError::AmbiguousFunction { .. } => {
                write!(f, "call to function was ambiguous")
            }

            TypecheckError::AmbiguousMethod { method, .. } => {
                write!(f, "call to method `{}` was ambiguous", method)
            }

            TypecheckError::AmbiguousSelfType{  method, iface, .. } => {
                write!(f, "the type implementing `{}` could not be deduced for `{}` in this context", iface, method)
            }

            TypecheckError::ExternalGenericFunction { func, .. } => {
                write!(f, "`{}` is generic but is declared with the `{}` modifier", func, ast::DeclMod::EXTERNAL_WORD)
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
                write!(f, "variant `{}` has no cases", variant.name)
            }

            TypecheckError::EmptyVariantCaseBinding { variant, case_index, .. } => {
                let case_ident = &variant.cases[*case_index].ident;
                write!(f, "cannot bind value of empty variant case `{}.{}`", variant.name, case_ident)
            }

            TypecheckError::NoLoopContext { stmt } => {
                write!(f, "the statement `{}` can only appear inside a loop", stmt)
            }

            TypecheckError::NoFunctionContext { stmt } => {
                write!(f, "the statement `{}` can only appear inside a function", stmt)
            }

            TypecheckError::UnsizedMember { decl, member_ty, .. } => {
                write!(f, "`{}` cannot have member of type `{}` because its size is unknown in this context", decl, member_ty)
            }

            TypecheckError::GenericError(err) => {
                write!(f, "{}", err)
            }

            TypecheckError::InvalidMethodInterface { ty, .. } => {
                write!(f, "`{}` is not an interface type and cannot have methods", ty)
            }

            TypecheckError::InterfaceNotImplemented { self_ty, iface_ty, .. } => {
                write!(f, "`{}` does not implement interface `{}`", self_ty, iface_ty)
            }

            TypecheckError::Private { name, .. } => {
                write!(f, "`{}` is not exported and can only be referenced in the unit where it is declared", name)
            }

            TypecheckError::PrivateConstructor { ty, .. } => {
                write!(f, "`{}` is a private type constructor and can only be constructed in the unit where it is declared", ty)
            }

            TypecheckError::DuplicateNamedArg { name, .. } => {
                write!(f, "named argument `{}` already occurred in this argument list", name)
            }

            TypecheckError::UnsafeConversionNotAllowed { from, to, .. } => {
                write!(f, "conversion from `{}` to `{}` is only allowed in an unsafe context", from, to)
            }

            TypecheckError::UnsafeAddressoOfNotAllowed { ty, .. } => {
                write!(f, "value of type `{}` can only have its address taken in an unsafe context", ty)
            }
        }
    }
}
