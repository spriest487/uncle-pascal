use std::{fmt, rc::Rc};

use pas_common::span::*;
use pas_syn::{
    ast::{Annotation, DeclNamed, TypeDeclName},
    ident::IdentPath,
    Ident,
};
use pas_syn::ast::TypeList;

use crate::{ast::{Expression, FunctionDecl}, GenericError, GenericResult, result::*, ty::*, ValueKind};
use crate::ast::{Literal, OverloadCandidate};

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct VariantCtorAnnotation {
    pub span: Span,

    // variant ctors don't know the type args of their variant, it must be inferred from context
    pub variant_name: IdentPath,

    pub case: Ident,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct OverloadAnnotation {
    pub span: Span,

    pub candidates: Vec<OverloadCandidate>,
    func_ty: Type,

    pub self_arg: Option<Box<Expression>>,

    pub type_args: Vec<Type>,
}

impl OverloadAnnotation {
    pub fn method(
        iface_ty: Type,
        self_arg: Expression,
        decl: FunctionDecl,
        span: Span
    ) -> Self {
        let sig = Rc::new(FunctionSig::of_decl(&decl));

        Self {
            span,
            type_args: Vec::new(), // NYI: methods can't have type args yet,
            self_arg: Some(Box::new(self_arg)),
            func_ty: Type::Function(sig.clone()),
            candidates: vec![
                OverloadCandidate::Method {
                    ident: decl.ident.last().clone(),
                    decl,
                    sig,
                    iface_ty,
                }
            ],
        }
    }

    pub fn new(
        candidates: Vec<OverloadCandidate>,
        self_arg: Option<Box<Expression>>,
        type_args: Vec<Type>,
        span: Span
    ) -> Self {
        let func_ty = if candidates.len() == 1 {
            Type::Function(candidates[0].sig().clone())
        } else {
            // undecided
            Type::Nothing
        };

        Self {
            candidates,
            func_ty,
            span,
            self_arg,
            type_args,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct InterfaceMethodAnnotation {
    pub iface_ty: Type,
    pub method_ident: Ident,
    pub span: Span,

    method_func_ty: Type,
}

impl InterfaceMethodAnnotation {
    pub fn new(decl: &FunctionDecl, iface_ty: Type, span: Span) -> Self {
        let sig = FunctionSig::of_decl(decl);

        Self {
            iface_ty,
            method_ident: decl.ident.last().clone(),
            span,
            method_func_ty: Type::Function(Rc::new(sig)),
        }
    }

    pub fn sig(&self) -> &FunctionSig {
        self.method_func_ty.as_func().as_ref().unwrap()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeAnnotation {
    Untyped(Span),
    TypedValue {
        span: Span,
        ty: Type,
        value_kind: ValueKind,
        decl: Option<Span>,
    },
    Function {
        span: Span,
        name: Ident,
        ns: IdentPath,
        func_ty: Type,
        type_args: Option<TypeList<Type>>,
    },
    // direct method reference e.g. `Interface.Method`
    InterfaceMethod(InterfaceMethodAnnotation),
    Type(Type, Span),
    Namespace(IdentPath, Span),
    UFCSCall {
        self_arg: Box<Expression>,
        function: IdentPath,
        func_ty: Type,
        span: Span,
    },
    VariantCtor(VariantCtorAnnotation),

    // as-yet unresolved function that may refer to 1+ functions (interface methods, ufcs functions,
    // or free functions)
    Overload(OverloadAnnotation),

    Const {
        span: Span,
        decl: Option<Span>,

        ty: Type,
        value: Literal,
    },
}

impl TypeAnnotation {
    pub fn expect_value(&self, expect_ty: &Type) -> TypecheckResult<()> {
        assert_ne!(Type::Nothing, *expect_ty);

        match self {
            TypeAnnotation::Function { func_ty: ty, .. }
            | TypeAnnotation::InterfaceMethod(InterfaceMethodAnnotation { method_func_ty: ty, .. })
            | TypeAnnotation::TypedValue { ty, .. }
            | TypeAnnotation::Const { ty, .. }
            | TypeAnnotation::Overload(OverloadAnnotation { func_ty: ty, .. })
                if ty == expect_ty =>
            {
                Ok(())
            }

            TypeAnnotation::Const { ty, span, .. }
            | TypeAnnotation::TypedValue { ty, span, .. } => Err(TypecheckError::TypeMismatch {
                span: span.clone(),
                expected: expect_ty.clone(),
                actual: ty.clone(),
            }),

            TypeAnnotation::UFCSCall { span, func_ty, .. }
            | TypeAnnotation::Function { span, func_ty, .. }
            | TypeAnnotation::InterfaceMethod(InterfaceMethodAnnotation { span, method_func_ty: func_ty, .. })
            | TypeAnnotation::Overload(OverloadAnnotation { span, func_ty, .. })
            => Err(TypecheckError::TypeMismatch {
                span: span.clone(),
                expected: expect_ty.clone(),
                actual: func_ty.clone(),
            }),

            TypeAnnotation::Untyped(span)
            | TypeAnnotation::Namespace(_, span)
            | TypeAnnotation::Type(_, span) => Err(TypecheckError::TypeMismatch {
                span: span.clone(),
                expected: expect_ty.clone(),
                actual: Type::Nothing,
            }),

            TypeAnnotation::VariantCtor(ctor) => {
                let variant_ty = Type::Variant(Box::new(Symbol {
                    qualified: ctor.variant_name.clone(),
                    decl_name: TypeDeclName::from(ctor.variant_name.last().clone()),
                    type_args: None,
                }));

                Err(TypecheckError::TypeMismatch {
                    span: ctor.span.clone(),
                    expected: expect_ty.clone(),
                    actual: variant_ty,
                })
            },
        }
    }

    pub fn ty(&self) -> &Type {
        match self {
            TypeAnnotation::Namespace(_, _) => &Type::Nothing,
            TypeAnnotation::Untyped(_) => &Type::Nothing,
            TypeAnnotation::Type(_, _) => &Type::Nothing,
            TypeAnnotation::VariantCtor(..) => &Type::Nothing,

            TypeAnnotation::UFCSCall { func_ty: ty, .. }
            | TypeAnnotation::Function { func_ty: ty, .. }
            | TypeAnnotation::InterfaceMethod(InterfaceMethodAnnotation { method_func_ty: ty, .. })
            | TypeAnnotation::Const { ty, .. }
            | TypeAnnotation::TypedValue { ty, .. } => ty,

            TypeAnnotation::Overload(overload) => &overload.func_ty,
        }
    }

    pub fn decl(&self) -> Option<&Span> {
        match self {
            TypeAnnotation::Type(..) => None,
            TypeAnnotation::Function { .. } => None, // TODO
            TypeAnnotation::InterfaceMethod(..) => None, // TODO
            TypeAnnotation::UFCSCall { .. } => None, // TODO
            TypeAnnotation::Overload { .. } => None, // TODO

            TypeAnnotation::TypedValue { decl, .. } => decl.as_ref(),
            TypeAnnotation::Untyped(..) => None,
            TypeAnnotation::Namespace(ident, ..) => Some(ident.last().span()),

            TypeAnnotation::Const { decl, .. } => decl.as_ref(),

            TypeAnnotation::VariantCtor(ctor) => Some(ctor.variant_name.span()),
        }
    }

    pub fn value_kind(&self) -> Option<ValueKind> {
        match self {
            TypeAnnotation::TypedValue { value_kind, .. } => Some(*value_kind),
            TypeAnnotation::Const { .. } => Some(ValueKind::Immutable),
            _ => None,
        }
    }

    pub fn is_namespace(&self) -> bool {
        match self {
            TypeAnnotation::Namespace(_, _) => true,
            _ => false,
        }
    }
}

impl fmt::Display for TypeAnnotation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.span())
    }
}

impl Spanned for TypeAnnotation {
    fn span(&self) -> &Span {
        match self {
            TypeAnnotation::Function { span, .. }
            | TypeAnnotation::InterfaceMethod(InterfaceMethodAnnotation { span, .. })
            | TypeAnnotation::UFCSCall { span, .. }
            | TypeAnnotation::Untyped(span)
            | TypeAnnotation::TypedValue { span, .. }
            | TypeAnnotation::VariantCtor(VariantCtorAnnotation { span, .. })
            | TypeAnnotation::Type(_, span)
            | TypeAnnotation::Namespace(_, span) => span,
            | TypeAnnotation::Overload(overload) => &overload.span,
            | TypeAnnotation::Const { span, .. } => span,
        }
    }
}

impl Annotation for TypeAnnotation {
    type Type = Type;
    type Name = Symbol;
    type Pattern = TypePattern;
}

#[derive(Eq, PartialEq, Hash, Clone, Debug)]
pub struct Symbol {
    pub decl_name: TypeDeclName,
    pub qualified: IdentPath,

    pub type_args: Option<TypeList<Type>>,
}

impl Symbol {
    pub fn expect_not_unspecialized(&self) -> GenericResult<()> {
        if !self.is_unspecialized_generic() {
            Ok(())
        } else {
            Err(GenericError::IllegalUnspecialized {
                ty: Type::Class(Box::new(self.clone())),
                span: self.span().clone(),
            })
        }
    }
}

impl Specializable for Symbol {
    type GenericID = IdentPath;

    /// is this either a type without type args, or does it already have all the type args it needs?
    fn is_unspecialized_generic(&self) -> bool {
        self.decl_name.type_params.is_some() && self.type_args.is_none()
    }

    fn name(&self) -> IdentPath {
        self.qualified.clone()
    }
}

impl DeclNamed for Symbol {
    fn as_local(&self) -> &TypeDeclName {
        &self.decl_name
    }

    fn decl_ty_params(&self) -> &[Ident] {
        match self.decl_name.type_params.as_ref() {
            Some(type_params) => &type_params.items,
            None => &[],
        }
    }
}

impl Spanned for Symbol {
    fn span(&self) -> &Span {
        self.decl_name.span()
    }
}

impl fmt::Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(type_args) = &self.type_args {
            write!(f, "{}{}", self.qualified, type_args)?;
        } else if let Some(type_params) = &self.decl_name.type_params {
            write!(f, "{}{}", self.qualified, type_params)?;
        } else {
            write!(f, "{}", self.qualified)?;
        }

        Ok(())
    }
}

impl From<OverloadAnnotation> for TypeAnnotation {
    fn from(a: OverloadAnnotation) -> Self {
        TypeAnnotation::Overload(a)
    }
}