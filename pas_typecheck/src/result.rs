use crate::{
    annotation::TypeAnnotation,
    ast::{Call, Expression, Variant, OverloadCandidate},
    context::NameError,
    GenericError, Type, ValueKind,
};
use pas_common::{span::*, Backtrace, DiagnosticLabel, DiagnosticMessage, DiagnosticOutput};
use pas_syn::{ast, parse::InvalidStatement, Ident, IdentPath, Operator, IntConstant};
use std::fmt;
use std::path::PathBuf;
use crate::ast::{DeclMod, Statement};

#[derive(Debug)]
pub enum TypecheckError {
    NameError {
        err: NameError,
        span: Span
    },
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
    IndexOutOfBounds {
        base_ty: Box<Type>,
        index: IntConstant,
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
        unit: IdentPath,
        syms: Vec<Ident>,
    },
    UnableToInferType {
        expr: Box<ast::Expression<Span>>,
    },
    UnableToInferSpecialization {
        generic_ty: Type,
        hint_ty: Type,
        span: Span,
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
    },
    InvalidConstExpr {
        expr: Box<Expression>,
    },

    InvalidCaseExprBlock {
        span: Span,
    },

    InvalidCast {
        from: Type,
        to: Type,
        span: Span,
    },

    EmptyMatchBlock {
        span: Span,
    },
    MatchExprNotExhaustive {
        span: Span,
        missing_cases: Vec<Ident>,
    }
}

impl TypecheckError {
    pub fn from_name_err(err: NameError, span: Span) -> Self {
        TypecheckError::NameError {
            err,
            span
        }
    }

    pub fn from_generic_err(err: GenericError, span: Span) -> Self {
        TypecheckError::NameError {
            err: NameError::GenericError(err),
            span,
        }
    }
}

pub type TypecheckResult<T> = Result<T, TypecheckError>;

impl Spanned for TypecheckError {
    fn span(&self) -> &Span {
        match self {
            TypecheckError::NameError { span, .. } => span,
            TypecheckError::NotCallable(expr) => expr.annotation().span(),
            TypecheckError::InvalidArgs { span, .. } => span,
            TypecheckError::InvalidCallInExpression(call) => call.annotation().span(),
            TypecheckError::InvalidIndexer { span, .. } => span,
            TypecheckError::IndexOutOfBounds { span, .. } => span,
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
            TypecheckError::UnableToInferSpecialization { span, .. } => span,
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
            TypecheckError::InvalidMethodInterface { span, .. } => span,
            TypecheckError::InterfaceNotImplemented { span, .. } => span,
            TypecheckError::Private { span, .. } => span,
            TypecheckError::PrivateConstructor { span, .. } => span,
            TypecheckError::UnsafeConversionNotAllowed { span, .. } => span,
            TypecheckError::UnsafeAddressoOfNotAllowed { span, .. } => span,
            TypecheckError::InvalidConstExpr { expr } => expr.span(),
            TypecheckError::InvalidCaseExprBlock { span } => span,
            TypecheckError::InvalidCast { span, .. } => span,
            TypecheckError::EmptyMatchBlock { span, .. } => span,
            TypecheckError::MatchExprNotExhaustive { span, .. } => span,
        }
    }
}

impl DiagnosticOutput for TypecheckError {
    fn title(&self) -> String {
        match self {
            TypecheckError::NameError { err, ..  } => match err {
                NameError::NotFound { .. } => "Name not found".to_string(),
                NameError::MemberNotFound { .. } => "Named member not found".to_string(),
                NameError::Unexpected { .. } => "Name had unexpected type".to_string(),
                NameError::AlreadyDeclared { .. } => "Name already declared".to_string(),
                NameError::AlreadyDefined { .. } => "Name already defined".to_string(),
                NameError::Ambiguous { .. } => "Name is ambiguous".to_string(),
                NameError::AlreadyImplemented { .. } => "Method already implemented".to_string(),
                NameError::DefDeclMismatch { .. } => {
                    "Definition does not match previous declaration".to_string()
                }
                NameError::GenericError(err) => match err {
                    GenericError::ArgsLenMismatch { .. } => "Wrong number of type arguments".to_string(),
                    GenericError::ArgConstraintNotSatisfied { .. } => "Type paramter constraint not satisfied by argument".to_string(),
                    GenericError::CannotInferArgs { .. } => "Cannot infer type arguments".to_string(),
                    GenericError::IllegalUnspecialized { .. } => "Illegal use of unspecialized type".to_string(),
                }
            }
            TypecheckError::NotCallable(_) => "Not callable".to_string(),
            TypecheckError::InvalidArgs { .. } => "Invalid arguments".to_string(),
            TypecheckError::InvalidCallInExpression(_) => "Invalid call in expression".to_string(),
            TypecheckError::InvalidIndexer { .. } => "Invalid indexer expression".to_string(),
            TypecheckError::IndexOutOfBounds { .. } => "Index out of bounds".to_string(),
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
            TypecheckError::UnableToInferSpecialization { .. } => {
                "Unable to infer type specialization".to_string()
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

            TypecheckError::InvalidConstExpr { .. } => "Invalid constant expression".to_string(),

            TypecheckError::InvalidCaseExprBlock { .. } => "Case block invalid as expression".to_string(),

            TypecheckError::InvalidCast { .. } => "Invalid cast".to_string(),
            TypecheckError::EmptyMatchBlock { .. } => "Empty match block".to_string(),
            TypecheckError::MatchExprNotExhaustive { .. } => "Match expression is not exhaustive".to_string(),
        }
    }

    fn label(&self) -> Option<DiagnosticLabel> {
        match self {
            TypecheckError::UndefinedSymbols { .. } => None,

            _ => Some(DiagnosticLabel {
                text: Some(self.to_string()),
                span: self.span().clone(),
            }),
        }
    }

    fn see_also(&self) -> Vec<DiagnosticMessage> {
        match self {
            TypecheckError::NameError { err, .. } => match err {
                NameError::AlreadyDeclared { new, existing, existing_kind } => {
                    if *existing.span().file.as_ref() == PathBuf::from("<builtin>") {
                        // don't show this message for conflicts with builtin identifiers
                        return Vec::new()
                    }

                    vec![DiagnosticMessage {
                        title: format!("{} `{}` previously declared here", existing_kind, new),
                        label: Some(DiagnosticLabel {
                            text: None,
                            span: existing.span().clone(),
                        }),
                    }]
                }

                NameError::AlreadyDefined { ident, existing } => vec![DiagnosticMessage {
                    title: format!("`{}` previously defined here", ident),
                    label: Some(DiagnosticLabel {
                        text: None,
                        span: existing.span().clone(),
                    }),
                }],

                NameError::AlreadyImplemented {
                    iface,
                    method,
                    existing,
                    ..
                } => vec![DiagnosticMessage {
                    title: format!("`{}.{}` previously implemented here", iface, method),
                    label: Some(DiagnosticLabel {
                        text: None,
                        span: existing.clone(),
                    }),
                }],

                NameError::Ambiguous { ident, options } => {
                    let mut see_also: Vec<_> = options
                        .iter()
                        .map(|option| DiagnosticMessage {
                            title: format!("`{}` could refer to `{}`", ident, option.join(".")),
                            label: Some(DiagnosticLabel {
                                text: None,
                                span: option.last().span().clone(),
                            }),
                        })
                        .collect();
                    see_also.sort();
                    see_also
                }

                NameError::DefDeclMismatch { def, decl, ident } => vec![
                    DiagnosticMessage {
                        title: format!("Previous declaration of `{}`", ident),
                        label: Some(DiagnosticLabel {
                            text: None,
                            span: decl.clone(),
                        }),
                    },
                    DiagnosticMessage {
                        title: format!("Conflicting definition of `{}`", ident),
                        label: Some(DiagnosticLabel {
                            text: None,
                            span: def.clone(),
                        }),
                    },
                ],

                _ => Vec::new(),
            },

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
            TypecheckError::NameError { err, .. } => write!(f, "{}", err),
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

            TypecheckError::IndexOutOfBounds { base_ty, index, .. } => {
                write!(f, "index value `{}` is out of range for type `{}`", index, base_ty)
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
                write!(f, "`{}` is generic but is declared with the `{}` modifier", func, DeclMod::EXTERNAL_WORD)
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

            TypecheckError::UnableToInferSpecialization { generic_ty, hint_ty, .. } => {
                write!(f, "unable to infer specialization of the generic type `{}` from expected type `{}`", generic_ty, hint_ty)
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

            TypecheckError::InvalidConstExpr { expr } => {
                write!(f, "expression `{}` is not a constant value", expr)
            }

            TypecheckError::InvalidCaseExprBlock { .. } => {
                write!(f, "case expression must contain at least one branch and an `else` branch")
            }

            TypecheckError::InvalidCast { from, to, .. } => {
                write!(f, "`{}` cannot be cast to `{}`", from, to)
            }

            TypecheckError::EmptyMatchBlock { .. } => {
                write!(f, "this match block must have at least one branch")
            }

            TypecheckError::MatchExprNotExhaustive { missing_cases, .. } => {
                write!(f, "this match expression must be exhaustive or have an `else` branch")?;
                if missing_cases.len() > 0 {
                    write!(f, " (unhandled cases: ")?;
                    for (i, missing_case) in missing_cases.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        write!(f, "{}", missing_case)?;
                    }
                    write!(f, ")")?;
                }
                Ok(())
            }
        }
    }
}
