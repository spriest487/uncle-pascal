use crate::ast;
use crate::ast::Ident;
use crate::ast::IdentPath;
use crate::ast::Operator;
use crate::parse::InvalidStatement;
use crate::typ::annotation::Typed;
use crate::typ::ast::Call;
use crate::typ::ast::DeclMod;
use crate::typ::ast::Expr;
use crate::typ::ast::OverloadCandidate;
use crate::typ::ast::Stmt;
use crate::typ::ast::VariantDef;
use crate::typ::context::NameError;
use crate::typ::GenericError;
use crate::typ::Type;
use crate::typ::ValueKind;
use crate::IntConstant;
use common::span::*;
use common::Backtrace;
use common::DiagnosticLabel;
use common::DiagnosticMessage;
use common::DiagnosticOutput;
use std::fmt;
use std::path::PathBuf;

#[derive(Debug)]
pub enum TypeError {
    NameError {
        err: NameError,
        span: Span
    },
    NotCallable(Box<Expr>),
    InvalidArgs {
        expected: Vec<Type>,
        actual: Vec<Type>,
        span: Span,
    },
    InvalidCallInExpression(Box<Call>),
    InvalidIndexer {
        base: Box<Expr>,
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
        expr: Box<Expr>,
        decl: Option<Ident>,
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
        stmt: Box<Stmt>,
        expected_expr_ty: Type,
    },
    InvalidBlockOutput(Box<Expr>),
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
    CtorMissingMembers {
        ctor_ty: Type,
        members: Vec<Ident>,
        span: Span,
    },
    DuplicateParamName {
        name: Ident,
        previous: Span,
        span: Span,
    },
    DuplicateNamedArg {
        name: Ident,
        previous: Span,
        span: Span,
    },
    UndefinedSymbols {
        unit: IdentPath,
        syms: Vec<IdentPath>,
    },
    UnableToInferType {
        expr: Box<ast::Expr<Span>>,
    },
    UnableToInferFunctionExprType {
        func: Box<ast::AnonymousFunctionDef<Span>>,
    },
    UnableToInferSpecialization {
        generic_ty: Type,
        hint_ty: Type,
        span: Span,
    },
    UninitGlobalBinding {
        ident: Ident,
        ty: Type,
    },
    UninitBindingWithNoType {
        binding: Box<ast::LocalBinding<Span>>,
    },
    ConstDeclWithNoValue {
        span: Span,
    },
    BindingWithNoType {
        binding_name: Ident,
        span: Span,
    },
    NotInitialized {
        ident: Ident,
        usage: Span,
    },
    InvalidRefExpression {
        expr: Box<Expr>,
    },
    InvalidStatement(InvalidStatement<Typed>),
    EmptyVariant(Box<ast::VariantDef<Span>>),
    EmptyVariantCaseBinding {
        variant: Box<VariantDef>,
        case_index: usize,
        span: Span,
    },
    NoLoopContext {
        stmt: Box<ast::Stmt<Span>>,
    },
    NoFunctionContext {
        stmt: Box<ast::Stmt<Span>>,
    },

    UnsizedMember {
        decl: IdentPath,
        member: Ident,
        member_ty: Type,
    },

    InvalidMethodModifiers {
        mods: Vec<DeclMod>,
        span: Span
    },
    InvalidMethodExplicitInterface {
        method_ident: Ident,
        span: Span,
    },
    NoMethodContext {
        method_ident: Ident,
        span: Span,
    },

    InvalidMethodInstanceType {
        ty: Type,
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
        expr: Box<Expr>,
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
    },

    InvalidExitWithValue {
        span: Span,
    },

    InvalidLoopCounterType {
        counter_ty: Type,
        span: Span,
    },

    EnumDeclWithTypeParams {
        span: Span,
    },
    EnumValuesMustBeAscending {
        span: Span,
        prev_ident: Ident,
        prev_val: i128,

        next_ident: Ident,
        next_val: i128,
    },
}

impl TypeError {
    pub fn from_name_err(err: NameError, span: Span) -> Self {
        TypeError::NameError {
            err,
            span
        }
    }

    pub fn from_generic_err(err: GenericError, span: Span) -> Self {
        TypeError::NameError {
            err: NameError::GenericError(err),
            span,
        }
    }
}

pub type TypecheckResult<T> = Result<T, TypeError>;

impl Spanned for TypeError {
    fn span(&self) -> &Span {
        match self {
            TypeError::NameError { span, .. } => span,
            TypeError::NotCallable(expr) => expr.annotation().span(),
            TypeError::InvalidArgs { span, .. } => span,
            TypeError::InvalidCallInExpression(call) => call.annotation().span(),
            TypeError::InvalidIndexer { span, .. } => span,
            TypeError::IndexOutOfBounds { span, .. } => span,
            TypeError::TypeMismatch { span, .. } => span,
            TypeError::NotMutable { expr, .. } => expr.annotation().span(),
            TypeError::NotAddressable { span, .. } => span,
            TypeError::NotDerefable { span, .. } => span,
            TypeError::NotMatchable { span, .. } => span,
            TypeError::InvalidBinOp { span, .. } => span,
            TypeError::InvalidUnaryOp { span, .. } => span,
            TypeError::BlockOutputIsNotExpression { stmt, .. } => stmt.annotation().span(),
            TypeError::InvalidBlockOutput(expr) => expr.annotation().span(),
            TypeError::AmbiguousFunction { span, .. } => span,
            TypeError::AmbiguousMethod { span, .. } => span,
            TypeError::ExternalGenericFunction { func, .. } => func.span(),
            TypeError::AmbiguousSelfType { span, .. } => span,
            TypeError::InvalidCtorType { span, .. } => span,
            TypeError::CtorMissingMembers { span, .. } => span,
            
            TypeError::DuplicateNamedArg { span, .. } => span,
            TypeError::DuplicateParamName { span, .. } => span,
            
            TypeError::UndefinedSymbols { unit, .. } => unit.span(),
            TypeError::UnableToInferType { expr } => expr.annotation().span(),
            TypeError::UnableToInferFunctionExprType { func } => func.span(),
            TypeError::UnableToInferSpecialization { span, .. } => span,
            TypeError::UninitGlobalBinding { ident, .. } => ident.span(),
            TypeError::UninitBindingWithNoType { binding } => binding.annotation.span(),
            TypeError::BindingWithNoType { span, .. } => span,
            TypeError::NotInitialized { usage, .. } => usage.span(),
            TypeError::InvalidRefExpression { expr } => expr.annotation().span(),
            TypeError::InvalidStatement(expr) => expr.0.annotation().span(),
            TypeError::EmptyVariant(variant) => variant.span(),
            TypeError::EmptyVariantCaseBinding { span, .. } => span,
            TypeError::NoLoopContext { stmt, .. } => stmt.annotation().span(),
            TypeError::NoFunctionContext { stmt, .. } => stmt.annotation().span(),
            TypeError::UnsizedMember { member, .. } => member.span(),
            
            TypeError::InvalidMethodModifiers { span, .. } => span,
            TypeError::InvalidMethodExplicitInterface { span, .. } => span,
            TypeError::InvalidMethodInstanceType { span, .. } => span,
            TypeError::NoMethodContext { span, .. } => span,

            TypeError::Private { span, .. } => span,
            TypeError::PrivateConstructor { span, .. } => span,
            TypeError::UnsafeConversionNotAllowed { span, .. } => span,
            TypeError::UnsafeAddressoOfNotAllowed { span, .. } => span,
            TypeError::InvalidConstExpr { expr } => expr.span(),
            TypeError::InvalidCaseExprBlock { span } => span,
            TypeError::InvalidCast { span, .. } => span,
            TypeError::EmptyMatchBlock { span, .. } => span,
            TypeError::MatchExprNotExhaustive { span, .. } => span,
            TypeError::InvalidExitWithValue { span, .. } => span,
            TypeError::ConstDeclWithNoValue { span, .. } => span,
            TypeError::InvalidLoopCounterType { span, .. } => span,
            TypeError::EnumDeclWithTypeParams { span, .. } => span,
            TypeError::EnumValuesMustBeAscending { span, .. } => span,
        }
    }
}

impl DiagnosticOutput for TypeError {
    fn title(&self) -> String {
        match self {
            TypeError::NameError { err, ..  } => match err {
                NameError::NotFound { .. } => "Name not found".to_string(),
                NameError::MemberNotFound { .. } => "Named member not found".to_string(),
                NameError::Unexpected { .. } => "Name had unexpected type".to_string(),
                NameError::AlreadyDeclared { .. } => "Name already declared".to_string(),
                NameError::AlreadyDefined { .. } => "Name already defined".to_string(),
                NameError::Ambiguous { .. } => "Name is ambiguous".to_string(),
                NameError::NoImplementationFound { .. } => "No implementation found".to_string(),
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
            TypeError::NotCallable(_) => "Not callable".to_string(),
            TypeError::InvalidArgs { .. } => "Invalid arguments".to_string(),
            TypeError::InvalidCallInExpression(_) => "Invalid call in expr".to_string(),
            TypeError::InvalidIndexer { .. } => "Invalid indexer expr".to_string(),
            TypeError::IndexOutOfBounds { .. } => "Index out of bounds".to_string(),
            TypeError::TypeMismatch { .. } => "Type mismatch".to_string(),
            TypeError::NotMutable { .. } => "Value not mutable".to_string(),
            TypeError::NotAddressable { .. } => "Value not addressable".to_string(),
            TypeError::NotDerefable { .. } => "Value cannot be dereferenced".to_string(),
            TypeError::NotMatchable { .. } => "Type is not matchable".to_string(),
            TypeError::InvalidBinOp { .. } => "Invalid binary operation".to_string(),
            TypeError::InvalidUnaryOp { .. } => "Invalid unary operation".to_string(),
            TypeError::BlockOutputIsNotExpression { .. } => "Expected block output expr".to_string(),
            TypeError::InvalidBlockOutput(_) => "Invalid block output expr".to_string(),
            TypeError::AmbiguousFunction { .. } => "Function reference is ambiguous".to_string(),
            TypeError::AmbiguousMethod { .. } => "Method reference is ambiguous".to_string(),
            TypeError::AmbiguousSelfType { .. } => "Self type of method is ambiguous".to_string(),
            TypeError::ExternalGenericFunction { .. } => "Function imported from external module may not have type parameters".to_string(),
            TypeError::InvalidCtorType { .. } => {
                "Invalid constructor expr type".to_string()
            }
            TypeError::CtorMissingMembers { .. } => {
                "Constructor is missing one or more named members".to_string()
            }
            TypeError::UndefinedSymbols { .. } => "Undefined symbol(s)".to_string(),
            TypeError::UnableToInferType { .. } => {
                "Unable to infer type of expr".to_string()
            }
            TypeError::UnableToInferFunctionExprType { .. } => {
                "Unable to infer function type of function expression".to_string()
            }
            TypeError::UnableToInferSpecialization { .. } => {
                "Unable to infer type specialization".to_string()
            }
            TypeError::UninitGlobalBinding { .. } => {
                "Uninitialized global variable".to_string()
            }
            TypeError::UninitBindingWithNoType { .. } => {
                "Uninitialized variable must have an explicit type".to_string()
            }
            TypeError::BindingWithNoType { .. } => {
                "Value bound to name must have a type".to_string()
            }
            TypeError::NotInitialized { .. } => "Use of uninitialized value".to_string(),
            TypeError::InvalidRefExpression { .. } => {
                "Invalid reference expr".to_string()
            }
            TypeError::InvalidStatement(invalid_stmt) => invalid_stmt.title(),
            TypeError::EmptyVariant(..) => "Empty variant".to_string(),
            TypeError::EmptyVariantCaseBinding { .. } => {
                "Empty variant case binding".to_string()
            }

            TypeError::NoLoopContext { .. } => "Statement requires loop context".to_string(),
            TypeError::NoFunctionContext { .. } => "Statement requires function context".to_string(),

            TypeError::UnsizedMember { .. } => "Unsized member".to_string(),
            
            TypeError::InvalidMethodExplicitInterface { .. } => "Explicit interface implementation for method is invalid".to_string(),
            TypeError::InvalidMethodModifiers { .. } => "Invalid method modifier(s)".to_string(),

            TypeError::InvalidMethodInstanceType { .. } => {
                "Invalid instance type for method".to_string()
            },

            TypeError::NoMethodContext { .. } => "Method requires enclosing type".to_string(),

            TypeError::Private { .. } => "Name not exported".to_string(),

            TypeError::PrivateConstructor { .. } => "Type has private constructor".to_string(),
            TypeError::DuplicateParamName { .. } => "Duplicate parameter name".to_string(),
            TypeError::DuplicateNamedArg { .. } => "Duplicate named argument".to_string(),
            TypeError::UnsafeConversionNotAllowed { .. } => "Conversion not allowed in a safe context".to_string(),
            TypeError::UnsafeAddressoOfNotAllowed { .. } => "Address operator not allowed on this type in a safe context".to_string(),

            TypeError::InvalidConstExpr { .. } => "Invalid constant expr".to_string(),

            TypeError::InvalidCaseExprBlock { .. } => "Case block invalid as expr".to_string(),

            TypeError::InvalidCast { .. } => "Invalid cast".to_string(),
            TypeError::EmptyMatchBlock { .. } => "Empty match block".to_string(),
            TypeError::MatchExprNotExhaustive { .. } => "Match expr is not exhaustive".to_string(),
            TypeError::InvalidExitWithValue { .. } => "Invalid exit with value".to_string(),
            TypeError::ConstDeclWithNoValue { .. } => "Constant declaration without a value".to_string(),
            TypeError::InvalidLoopCounterType { .. } => "Invalid loop counter type".to_string(),
            TypeError::EnumDeclWithTypeParams { .. } => "Enumeration type declared with type params".to_string(),
            TypeError::EnumValuesMustBeAscending { .. } => "Enumeration values must be ascending".to_string(),
        }
    }

    fn label(&self) -> Option<DiagnosticLabel> {
        match self {
            TypeError::UndefinedSymbols { .. } => None,

            _ => Some(DiagnosticLabel {
                text: Some(self.to_string()),
                span: self.span().clone(),
            }),
        }
    }

    fn see_also(&self) -> Vec<DiagnosticMessage> {
        match self {
            TypeError::NameError { err, .. } => match err {
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
                        notes: Vec::new(),
                    }]
                }

                NameError::AlreadyDefined { ident, existing } => vec![DiagnosticMessage {
                    title: format!("`{}` previously defined here", ident),
                    label: Some(DiagnosticLabel {
                        text: None,
                        span: existing.span().clone(),
                    }),
                    notes: Vec::new(),
                }],

                NameError::AlreadyImplemented {
                    owning_ty: iface,
                    method,
                    existing,
                    ..
                } => vec![DiagnosticMessage {
                    title: format!("`{}.{}` previously implemented here", iface, method),
                    label: Some(DiagnosticLabel {
                        text: None,
                        span: existing.clone(),
                    }),
                    notes: Vec::new(),
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
                            notes: Vec::new(),
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
                        notes: Vec::new(),
                    },
                    DiagnosticMessage {
                        title: format!("Conflicting definition of `{}`", ident),
                        label: Some(DiagnosticLabel {
                            text: None,
                            span: def.clone(),
                        }),
                        notes: Vec::new(),
                    },
                ],

                _ => Vec::new(),
            },

            TypeError::UndefinedSymbols { syms, .. } => syms
                .iter()
                .map(|sym| DiagnosticMessage {
                    title: format!("`{}` is declared but not defined", sym),
                    label: Some(DiagnosticLabel {
                        text: Some("declared here".to_string()),
                        span: sym.span().clone(),
                    }),
                    notes: Vec::new(),
                })
                .collect(),

            TypeError::NotInitialized { ident, .. } => vec![DiagnosticMessage {
                title: format!("uninitialized value `{}`", ident),
                label: Some(DiagnosticLabel {
                    text: Some("declared here".to_string()),
                    span: ident.span().clone(),
                }),
                notes: Vec::new(),
            }],

            TypeError::NotMutable {
                decl: Some(decl),
                ..
            } => vec![DiagnosticMessage {
                title: "modifying immutable value".to_string(),
                label: Some(DiagnosticLabel {
                    text: Some("declared as immutable here".to_string()),
                    span: decl.span().clone(),
                }),
                notes: Vec::new(),
            }],

            TypeError::DuplicateNamedArg { name, previous, .. } 
            | TypeError::DuplicateParamName { name, previous, .. } => 
                vec![DiagnosticMessage {
                    title: format!("previous occurrence of `{}`", name),
                    label: Some(DiagnosticLabel {
                        text: None,
                        span: previous.clone(),
                    }),
                    notes: Vec::new(),
                }],

            TypeError::AmbiguousFunction { candidates, .. } => {
                candidates.iter()
                    .map(|c| {
                        DiagnosticMessage {
                            label: Some(DiagnosticLabel {
                                span: c.span().clone(),
                                text: None,
                            }),
                            title: format!("may refer to {}", c),
                            notes: Vec::new(),
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

impl fmt::Display for TypeError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TypeError::NameError { err, .. } => write!(f, "{}", err),
            TypeError::NotCallable(expr) => {
                write!(f, "{} `{}` of type `{}` is not a callable function", expr.name(), expr, expr.annotation().ty())
            }

            TypeError::InvalidArgs { expected, actual, .. } => {
                write!(f, "expected arguments (")?;
                write_args(f, expected.iter())?;
                write!(f, "), found (")?;
                write_args(f, actual.iter())?;
                write!(f, ")")
            }

            TypeError::InvalidCallInExpression(call) => {
                write!(f, "function call `{}` returns no value and cannot be used as part of an expr", call)
            }

            TypeError::InvalidIndexer { base, index_ty, .. } => {
                let base_ty = base.annotation().ty();
                write!(f, "`{}` cannot be used as an index for `{}` of type `{}`", index_ty, base, base_ty)
            }

            TypeError::IndexOutOfBounds { base_ty, index, .. } => {
                write!(f, "index value `{}` is out of range for type `{}`", index, base_ty)
            }

            TypeError::TypeMismatch { expected, actual, .. } => {
                write!(f, "type mismatch: expected {}, found {}", expected, actual)
            }

            TypeError::NotMutable { expr, .. } => {
                write!(f, "`{}` does not refer to a mutable value", expr)
            }

            TypeError::NotAddressable { ty, value_kind, .. } => {
                match value_kind {
                    Some(value_kind) => write!(f, "{} of type {} cannot have its address taken", value_kind, ty),
                    None => write!(f, "expr without a value cannot have its address taken"),
                }
            }

            TypeError::NotDerefable { ty, .. } => {
                write!(f, "value of type {} cannot be dereferenced", ty)
            }

            TypeError::NotMatchable { ty, .. } => {
                write!(f, "type {} cannot be used in matching constructs", ty)
            }

            TypeError::InvalidBinOp { lhs, rhs, op, .. } => {
                match op {
                    Operator::Assignment => {
                        write!(f, "{} is not assignable to {}", rhs, lhs)
                    },
                    _ => {
                        write!(f, "operator {} cannot be applied to the operand types {} and {}", op, lhs, rhs)
                    }
                }
            }

            TypeError::InvalidUnaryOp { operand, op, .. } => {
                write!(f, "operator {} cannot be applied to an operand of type {}", op, operand)
            }

            TypeError::BlockOutputIsNotExpression { stmt, expected_expr_ty } => {
                write!(f, "expected expr of type `{}` but found stmt `{}`", expected_expr_ty, stmt)
            }

            TypeError::InvalidBlockOutput(expr) => {
                write!(f, "expr `{}` is not valid as the final expr in a block because it is not a complete stmt", expr)
            }

            TypeError::AmbiguousFunction { .. } => {
                write!(f, "call to function was ambiguous")
            }

            TypeError::AmbiguousMethod { method, .. } => {
                write!(f, "call to method `{}` was ambiguous", method)
            }

            TypeError::AmbiguousSelfType{  method, iface, .. } => {
                write!(f, "the type implementing `{}` could not be deduced for `{}` in this context", iface, method)
            }

            TypeError::ExternalGenericFunction { func, .. } => {
                write!(f, "`{}` is generic but is declared with the `{}` modifier", func, DeclMod::EXTERNAL_WORD)
            }

            TypeError::InvalidCtorType { ty, .. } => {
                write!(f, "type `{}` cannot be created with a constructor expr", ty)
            }

            TypeError::CtorMissingMembers { ctor_ty, members, .. } => {
                write!(f, "the following members are missing from the constructor for type `{}`: ", ctor_ty)?;
                for (i, member) in members.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", member)?;
                }
                Ok(())
            }

            TypeError::UndefinedSymbols { unit, .. } => {
                write!(f, "unit `{}` contains undefined symbols", unit)
            }

            TypeError::UnableToInferType { expr } => {
                write!(f, "unable to infer the type of `{}`", expr)
            }

            TypeError::UnableToInferFunctionExprType { .. } => {
                write!(f, "unable to infer the type of this function expression from the usage")
            }

            TypeError::UnableToInferSpecialization { generic_ty, hint_ty, .. } => {
                write!(f, "unable to infer specialization of the generic type `{}` from expected type `{}`", generic_ty, hint_ty)
            }

            TypeError::UninitGlobalBinding { ident, ty } => {
                write!(f, "global variable `{}` of type `{}` does not have a default value and must be initialized", ident, ty)
            }

            TypeError::UninitBindingWithNoType { binding } => {
                write!(f, "the type of `{}` cannot be inferred because it has no initial value", binding.name)
            }

            TypeError::BindingWithNoType { binding_name, .. } => {
                write!(f, "the type of value bound to `{}` cannot be Nothing", binding_name)
            }

            TypeError::NotInitialized { ident, .. } => {
                write!(f, "`{}` is used before initialization", ident)
            }

            TypeError::InvalidRefExpression { expr } => {
                write!(f, "`{}` does not refer to a value that can be passed by reference", expr)
            }

            TypeError::InvalidStatement(invalid_stmt) => {
                write!(f, "{}", invalid_stmt)
            }

            TypeError::EmptyVariant(variant) => {
                write!(f, "variant `{}` has no cases", variant.name)
            }

            TypeError::EmptyVariantCaseBinding { variant, case_index, .. } => {
                let case_ident = &variant.cases[*case_index].ident;
                write!(f, "cannot bind value of empty variant case `{}.{}`", variant.name, case_ident)
            }

            TypeError::NoLoopContext { stmt } => {
                write!(f, "the stmt `{}` can only appear inside a loop", stmt)
            }

            TypeError::NoFunctionContext { stmt } => {
                write!(f, "the stmt `{}` can only appear inside a function", stmt)
            }

            TypeError::UnsizedMember { decl, member_ty, .. } => {
                write!(f, "`{}` cannot have member of type `{}` because its size is unknown in this context", decl, member_ty)
            }

            TypeError::InvalidMethodModifiers { mods, .. } => {
                if mods.len() > 1 {
                    write!(f, "the following modifiers can not appear on a method declaration:")?;
                    write!(f, "{}", mods.iter()
                        .map(|m| format!("`{}`", m))
                        .collect::<Vec<_>>()
                        .join(", "))
                } else {
                    write!(f, "the modifier `{}` can not appear on a method declaration:", mods[0])
                }
            }

            TypeError::InvalidMethodExplicitInterface { method_ident, .. } => {
                write!(f, "method `{}` declared in this scope cannot explicitly implement an interface", method_ident)
            }
            
            TypeError::NoMethodContext { method_ident, .. } => {
                write!(f, "method `{}` is not declared within a type", method_ident)
            }

            TypeError::InvalidMethodInstanceType { ty, .. } => {
                write!(f, "`{}` is not a type which supports methods", ty)
            }

            TypeError::Private { name, .. } => {
                write!(f, "`{}` is not exported and can only be referenced in the unit where it is declared", name)
            }

            TypeError::PrivateConstructor { ty, .. } => {
                write!(f, "`{}` is a private type constructor and can only be constructed in the unit where it is declared", ty)
            }

            TypeError::DuplicateNamedArg { name, .. } => {
                write!(f, "named argument `{}` already occurred in this argument list", name)
            }
            
            TypeError::DuplicateParamName { name, .. } => {
                write!(f, "parameter named `{}` was already declared previously", name)
            }

            TypeError::UnsafeConversionNotAllowed { from, to, .. } => {
                write!(f, "conversion from `{}` to `{}` is only allowed in an unsafe context", from, to)
            }

            TypeError::UnsafeAddressoOfNotAllowed { ty, .. } => {
                write!(f, "value of type `{}` can only have its address taken in an unsafe context", ty)
            }

            TypeError::InvalidConstExpr { expr } => {
                write!(f, "expr `{}` is not a constant value", expr)
            }

            TypeError::InvalidCaseExprBlock { .. } => {
                write!(f, "case expr must contain at least one branch and an `else` branch")
            }

            TypeError::InvalidCast { from, to, .. } => {
                write!(f, "`{}` cannot be cast to `{}`", from, to)
            }

            TypeError::EmptyMatchBlock { .. } => {
                write!(f, "this match block must have at least one branch")
            }

            TypeError::MatchExprNotExhaustive { missing_cases, .. } => {
                write!(f, "this match expr must be exhaustive or have an `else` branch")?;
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

            TypeError::InvalidExitWithValue { .. } => {
                write!(f, "cannot exit with a value in this context")
            }

            TypeError::ConstDeclWithNoValue { .. } => {
                write!(f, "constant declaration must have a value")
            }

            TypeError::InvalidLoopCounterType { counter_ty, .. } => {
                write!(f, "type `{}` cannot be used as a loop counter", counter_ty)
            }

            TypeError::EnumDeclWithTypeParams { .. } => {
                write!(f, "enumeration types cannot have type parameters")
            }

            TypeError::EnumValuesMustBeAscending { prev_ident, prev_val, next_ident, next_val, .. } => {
                write!(f, "item `{}` has lower value ({}) than previous item `{}` ({})", next_ident, next_val, prev_ident, prev_val)
            }
        }
    }
}
