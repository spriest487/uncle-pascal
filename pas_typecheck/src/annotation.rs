use std::{fmt, rc::Rc};

use pas_common::span::*;
use pas_syn::{
    ast::{Annotation, DeclNamed, TypeDeclName},
    ident::IdentPath,
    Ident,
};

use crate::{
    ast::{Expression, FunctionDecl},
    result::*,
    ty::*,
    ValueKind,
};
use crate::ast::OverloadCandidate;

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
        type_args: Vec<Type>,
    },
    Type(Type, Span),
    Namespace(IdentPath, Span),
    UFCSCall {
        self_arg: Box<Expression>,
        function: IdentPath,
        func_ty: Type,
        span: Span,
    },
    VariantCtor(VariantCtorAnnotation),
    Overload(OverloadAnnotation),
}

impl TypeAnnotation {
    pub fn expect_value(&self, expect_ty: &Type) -> TypecheckResult<()> {
        assert_ne!(Type::Nothing, *expect_ty);

        match self {
            TypeAnnotation::Function { func_ty: ty, .. }
            | TypeAnnotation::TypedValue { ty, .. }
            | TypeAnnotation::Overload(OverloadAnnotation { func_ty: ty, .. })
                if ty == expect_ty =>
            {
                Ok(())
            }

            TypeAnnotation::TypedValue { ty, span, .. } => Err(TypecheckError::TypeMismatch {
                span: span.clone(),
                expected: expect_ty.clone(),
                actual: ty.clone(),
            }),

            TypeAnnotation::UFCSCall { span, func_ty, .. }
            | TypeAnnotation::Function { span, func_ty, .. }
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
                let variant_ty = Type::Variant(Box::new(QualifiedDeclName {
                    qualified: ctor.variant_name.clone(),
                    decl_name: TypeDeclName::from(ctor.variant_name.last().clone()),
                    type_args: Vec::new(),
                }));

                Err(TypecheckError::TypeMismatch {
                    span: ctor.span.clone(),
                    expected: expect_ty.clone(),
                    actual: variant_ty,
                })
            }
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
            | TypeAnnotation::TypedValue { ty, .. } => ty,

            TypeAnnotation::Overload(overload) => &overload.func_ty,
        }
    }

    pub fn decl(&self) -> Option<&Span> {
        match self {
            TypeAnnotation::Type(..) => None,
            TypeAnnotation::Function { .. } => None, // TODO
            TypeAnnotation::UFCSCall { .. } => None, // TODO
            TypeAnnotation::Overload { .. } => None, // TODO

            TypeAnnotation::TypedValue { decl, .. } => decl.as_ref(),
            TypeAnnotation::Untyped(..) => None,
            TypeAnnotation::Namespace(ident, ..) => Some(ident.last().span()),

            TypeAnnotation::VariantCtor(ctor) => Some(ctor.variant_name.span()),
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
            | TypeAnnotation::UFCSCall { span, .. }
            | TypeAnnotation::Untyped(span)
            | TypeAnnotation::TypedValue { span, .. }
            | TypeAnnotation::VariantCtor(VariantCtorAnnotation { span, .. })
            | TypeAnnotation::Type(_, span)
            | TypeAnnotation::Namespace(_, span) => span,
            | TypeAnnotation::Overload(overload) => &overload.span,
        }
    }
}

impl Annotation for TypeAnnotation {
    type Type = Type;
    type DeclName = QualifiedDeclName;
    type Pattern = TypePattern;
}

#[derive(Eq, PartialEq, Hash, Clone, Debug)]
pub struct QualifiedDeclName {
    pub decl_name: TypeDeclName,
    pub qualified: IdentPath,
    pub type_args: Vec<Type>,
}

impl Specializable for QualifiedDeclName {
    type GenericID = IdentPath;

    // is this either a type without type args, or does it already have all the type args it needs?
    fn is_generic(&self) -> bool {
        self.type_args.len() != self.decl_name.type_params.len()
    }

    fn name(&self) -> IdentPath {
        self.qualified.clone()
    }
}

impl DeclNamed for QualifiedDeclName {
    fn as_local(&self) -> &TypeDeclName {
        &self.decl_name
    }
}

impl Spanned for QualifiedDeclName {
    fn span(&self) -> &Span {
        self.decl_name.span()
    }
}

impl fmt::Display for QualifiedDeclName {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if !self.type_args.is_empty() {
            write!(f, "{} of ", self.qualified)?;

            for (arg_pos, arg_ty) in self.type_args.iter().enumerate() {
                if arg_pos > 0 {
                    write!(f, ", ")?;
                }

                write!(f, "{}", arg_ty)?;
            }
        } else if self.decl_name.type_params.len() > 0 {
            write!(f, "{} of ", self.qualified)?;

            for (arg_pos, arg_name) in self.decl_name.type_params.iter().enumerate() {
                if arg_pos > 0 {
                    write!(f, ", ")?;
                }

                write!(f, "{}", arg_name)?;
            }
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