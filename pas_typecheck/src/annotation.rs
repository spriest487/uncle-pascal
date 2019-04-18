use std::{fmt, rc::Rc};

use pas_common::span::*;
use pas_syn::{ast::Annotation, ident::IdentPath, Ident};

use crate::{
    ast::{Expression, FunctionDecl, Variant},
    result::*,
    FunctionSig, Type, ValueKind,
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MethodAnnotation {
    pub span: Span,
    pub iface_ty: Type,
    pub method: Box<FunctionDecl>,

    //    pub self_ty: Type,
    pub self_arg: Option<Box<Expression>>,

    method_ty: Type,
}

impl MethodAnnotation {
    pub fn ufcs(
        span: Span,
        iface_ty: Type,
        self_arg: Expression,
        method_decl: FunctionDecl,
    ) -> Self {
        Self {
            span,
            iface_ty,
            method_ty: Type::Function(Rc::new(FunctionSig::of_decl(&method_decl))),
            method: Box::new(method_decl),

            self_arg: Some(Box::new(self_arg)),
        }
    }

    pub fn explicit(span: Span, iface_ty: Type, method_decl: FunctionDecl) -> Self {
        Self {
            span,
            iface_ty,
            method_ty: Type::Function(Rc::new(FunctionSig::of_decl(&method_decl))),
            method: Box::new(method_decl),

            //            self_ty: Type::GenericSelf,
            self_arg: None,
        }
    }

    pub fn decl_sig(&self) -> &FunctionSig {
        match &self.method_ty {
            Type::Function(sig) => sig.as_ref(),
            _ => unreachable!(),
        }
    }

    pub fn ty(&self) -> &Type {
        &self.method_ty
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct VariantCtorAnnotation {
    pub span: Span,

    variant_ty: Type,
    pub case_index: usize,
}

impl VariantCtorAnnotation {
    pub fn new(variant: Rc<Variant>, case_index: usize, span: Span) -> Self {
        Self {
            span,
            case_index,
            variant_ty: Type::Variant(variant),
        }
    }

    pub fn ty(&self) -> &Type {
        &self.variant_ty
    }

    pub fn decl(&self) -> Rc<Variant> {
        match &self.variant_ty {
            Type::Variant(variant) => variant.clone(),
            _ => unreachable!("variant ctor must always have a variant type"),
        }
    }

    pub fn decl_span(&self) -> &Span {
        match &self.variant_ty {
            Type::Variant(variant) => variant.span(),
            _ => unreachable!("variant ctor must always have a variant type"),
        }
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
        ty: Type,
    },
    Type(Type, Span),
    Namespace(IdentPath, Span),
    Method(MethodAnnotation),
    VariantCtor(VariantCtorAnnotation),
}

impl TypeAnnotation {
    pub fn expect_value(&self, expect_ty: &Type) -> TypecheckResult<()> {
        assert_ne!(Type::Nothing, *expect_ty);

        match self {
            TypeAnnotation::Function { ty, .. } | TypeAnnotation::TypedValue { ty, .. }
                if ty == expect_ty =>
            {
                Ok(())
            }

            TypeAnnotation::Method(method_annotation) => Err(TypecheckError::TypeMismatch {
                span: method_annotation.span.clone(),
                expected: expect_ty.clone(),
                actual: method_annotation.method_ty.clone(),
            }),

            TypeAnnotation::TypedValue { ty, span, .. } => Err(TypecheckError::TypeMismatch {
                span: span.clone(),
                expected: expect_ty.clone(),
                actual: ty.clone(),
            }),

            TypeAnnotation::Function { span, .. }
            | TypeAnnotation::Untyped(span)
            | TypeAnnotation::Namespace(_, span)
            | TypeAnnotation::Type(_, span) => Err(TypecheckError::TypeMismatch {
                span: span.clone(),
                expected: expect_ty.clone(),
                actual: Type::Nothing,
            }),

            TypeAnnotation::VariantCtor(ctor) => Err(TypecheckError::TypeMismatch {
                span: ctor.span.clone(),
                expected: expect_ty.clone(),
                actual: ctor.ty().clone(),
            }),
        }
    }

    pub fn ty(&self) -> &Type {
        match self {
            TypeAnnotation::Namespace(_, _) => &Type::Nothing,
            TypeAnnotation::Untyped(_) => &Type::Nothing,
            TypeAnnotation::Type(_, _) => &Type::Nothing,
            TypeAnnotation::Method(method) => &method.method_ty,
            TypeAnnotation::VariantCtor(..) => &Type::Nothing,

            TypeAnnotation::Function { ty, .. } | TypeAnnotation::TypedValue { ty, .. } => ty,
        }
    }

    pub fn decl(&self) -> Option<&Span> {
        match self {
            TypeAnnotation::Type(..) => None,
            TypeAnnotation::Method(method) => Some(method.method.span()),
            TypeAnnotation::Function { .. } => None, //TODO
            TypeAnnotation::TypedValue { decl, .. } => decl.as_ref(),
            TypeAnnotation::Untyped(..) => None,
            TypeAnnotation::Namespace(ident, ..) => Some(ident.last().span()),

            TypeAnnotation::VariantCtor(ctor) => Some(ctor.decl_span()),
        }
    }

    pub fn value_kind(&self) -> Option<ValueKind> {
        match self {
            TypeAnnotation::TypedValue { value_kind, .. } => Some(*value_kind),
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
            | TypeAnnotation::Untyped(span)
            | TypeAnnotation::TypedValue { span, .. }
            | TypeAnnotation::Method(MethodAnnotation { span, .. })
            | TypeAnnotation::VariantCtor(VariantCtorAnnotation { span, .. })
            | TypeAnnotation::Type(_, span)
            | TypeAnnotation::Namespace(_, span) => span,
        }
    }
}

impl Annotation for TypeAnnotation {
    type Type = Type;
}
