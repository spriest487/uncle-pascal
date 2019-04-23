use crate::{
    ast::{
        ExpressionNode,
        FunctionDecl,
    },
    result::*,
    FunctionSig,
    Type,
    ValueKind,
};
use pas_common::span::*;
use pas_syn::{
    ast::Annotation,
    ident::IdentPath,
    Ident,
};
use std::{
    fmt,
    rc::Rc,
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MethodAnnotation {
    pub span: Span,
    pub iface_ty: Type,
    pub method: Box<FunctionDecl>,

    //    pub self_ty: Type,
    pub self_arg: Option<Box<ExpressionNode>>,

    method_ty: Type,
}

impl MethodAnnotation {
    pub fn ufcs(
        span: Span,
        iface_ty: Type,
        self_arg: ExpressionNode,
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeAnnotation {
    Untyped(Span),
    TypedValue {
        span: Span,
        ty: Type,
        value_kind: ValueKind,
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
}

impl TypeAnnotation {
    pub fn is_untyped(&self) -> bool {
        match self {
            TypeAnnotation::Untyped(_) => true,
            _ => false,
        }
    }

    pub fn expect_value(&self, expect_ty: &Type) -> TypecheckResult<()> {
        assert_ne!(Type::Nothing, *expect_ty);

        match self {
            TypeAnnotation::Function { ty, .. } | TypeAnnotation::TypedValue { ty, .. }
                if ty == expect_ty =>
            {
                Ok(())
            },

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
        }
    }

    pub fn value_ty(&self) -> &Type {
        match self {
            TypeAnnotation::Namespace(_, _) => &Type::Nothing,
            TypeAnnotation::Untyped(_) => &Type::Nothing,
            TypeAnnotation::Type(_, _) => &Type::Nothing,
            TypeAnnotation::Method(method) => &method.method_ty,

            TypeAnnotation::Function { ty, .. } | TypeAnnotation::TypedValue { ty, .. } => ty,
        }
    }

    pub fn value_kind(&self) -> Option<ValueKind> {
        match self {
            TypeAnnotation::TypedValue { value_kind, .. } => Some(*value_kind),
            //            TypeAnnotation::Function { .. } => Some(ValueKind::Immutable),
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
            | TypeAnnotation::Type(_, span)
            | TypeAnnotation::Namespace(_, span) => span,
        }
    }
}

impl Annotation for TypeAnnotation {
    type Type = Type;
}
