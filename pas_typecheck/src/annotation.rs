use std::borrow::Cow;
use std::{fmt, rc::Rc};
use derivative::*;
use pas_common::span::*;
use pas_syn::ast::TypeList;
use pas_syn::{
    ast::{Annotation, DeclNamed, TypeDeclName},
    ident::IdentPath,
    Ident, IntConstant,
};

use crate::ast::{Literal, OverloadCandidate};
use crate::{
    ast::{Expr, FunctionDecl},
    result::*,
    ty::*,
    GenericError, GenericResult, ValueKind,
};

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct VariantCtorAnnotation {
    pub span: Span,

    // variant ctors don't know the type args of their variant, it must be inferred from context
    pub variant_name: IdentPath,

    pub case: Ident,
}

impl From<VariantCtorAnnotation> for TypeAnnotation {
    fn from(a: VariantCtorAnnotation) -> Self {
        TypeAnnotation::VariantCtor(Rc::new(a))
    }
}

#[derive(Eq, Clone, Derivative)]
#[derivative(Hash, Debug, PartialEq)]
pub struct OverloadAnnotation {
    #[derivative(Debug = "ignore")]
    #[derivative(Hash = "ignore")]
    #[derivative(PartialEq = "ignore")]
    pub span: Span,

    pub candidates: Vec<OverloadCandidate>,
    sig: Option<Rc<FunctionSig>>,

    pub self_arg: Option<Box<Expr>>,

    pub type_args: Vec<Type>,
}

impl OverloadAnnotation {
    pub fn method(iface_ty: Type, self_arg: Expr, decl: FunctionDecl, span: Span) -> Self {
        let sig = Rc::new(FunctionSig::of_decl(&decl));

        Self {
            span,
            type_args: Vec::new(), // NYI: methods can't have type args yet,
            self_arg: Some(Box::new(self_arg)),
            sig: Some(sig.clone()),
            candidates: vec![OverloadCandidate::Method {
                ident: decl.ident.last().clone(),
                decl,
                sig,
                iface_ty,
            }],
        }
    }

    pub fn new(
        candidates: Vec<OverloadCandidate>,
        self_arg: Option<Box<Expr>>,
        type_args: Vec<Type>,
        span: Span,
    ) -> Self {
        let sig = if candidates.len() == 1 {
            Some(candidates[0].sig().clone())
        } else {
            // undecided
            None
        };

        Self {
            candidates,
            sig,
            span,
            self_arg,
            type_args,
        }
    }

    pub fn func_ty(&self) -> Type {
        match &self.sig {
            Some(sig) => Type::Function(sig.clone()),
            None => Type::Nothing,
        }
    }
}

impl From<OverloadAnnotation> for TypeAnnotation {
    fn from(a: OverloadAnnotation) -> Self {
        TypeAnnotation::Overload(Rc::new(a))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct InterfaceMethodAnnotation {
    pub iface_ty: Type,
    pub method_ident: Ident,
    pub span: Span,

    pub method_sig: Rc<FunctionSig>,
}

impl InterfaceMethodAnnotation {
    pub fn new(decl: &FunctionDecl, iface_ty: Type, span: Span) -> Self {
        let sig = FunctionSig::of_decl(decl);

        Self {
            iface_ty,
            method_ident: decl.ident.last().clone(),
            span,
            method_sig: Rc::new(sig),
        }
    }

    pub fn func_ty(&self) -> Type {
        Type::Function(self.method_sig.clone())
    }

    pub fn should_call_noargs_in_expr(&self, expect_ty: &Type, self_arg: &Type) -> bool {
        self.method_sig.should_call_noargs_in_expr(expect_ty, Some(self_arg))
    }
}

impl From<InterfaceMethodAnnotation> for TypeAnnotation {
    fn from(a: InterfaceMethodAnnotation) -> Self {
        TypeAnnotation::InterfaceMethod(Rc::new(a))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FunctionAnnotation {
    pub ident: IdentPath,
    pub sig: Rc<FunctionSig>,
    pub type_args: Option<TypeList<Type>>,
    pub span: Span,
}

impl FunctionAnnotation {
    pub fn func_ty(&self) -> Type {
        Type::Function(self.sig.clone())
    }

    pub fn should_call_noargs_in_expr(&self, expect_ty: &Type) -> bool {
        self.sig.should_call_noargs_in_expr(expect_ty, None)
    }
}

impl From<FunctionAnnotation> for TypeAnnotation {
    fn from(a: FunctionAnnotation) -> Self {
        TypeAnnotation::Function(Rc::new(a))
    }
}

#[derive(Eq, Clone, Derivative)]
#[derivative(Hash, Debug, PartialEq)]
pub struct TypedValueAnnotation {
    pub ty: Type,
    pub value_kind: ValueKind,
    pub decl: Option<Ident>,

    #[derivative(Debug = "ignore")]
    #[derivative(Hash = "ignore")]
    #[derivative(PartialEq = "ignore")]
    pub span: Span,
}

impl From<TypedValueAnnotation> for TypeAnnotation {
    fn from(a: TypedValueAnnotation) -> Self {
        TypeAnnotation::TypedValue(Rc::new(a))
    }
}

#[derive(Eq, Clone, Derivative)]
#[derivative(Hash, Debug, PartialEq)]
pub struct ConstAnnotation {
    pub decl: Option<Ident>,
    pub ty: Type,

    pub value: Literal,

    #[derivative(Debug = "ignore")]
    #[derivative(Hash = "ignore")]
    #[derivative(PartialEq = "ignore")]
    pub span: Span,
}

impl From<ConstAnnotation> for TypeAnnotation {
    fn from(a: ConstAnnotation) -> Self {
        TypeAnnotation::Const(Rc::new(a))
    }
}

#[derive(Eq, Clone, Derivative)]
#[derivative(Hash, Debug, PartialEq)]
pub struct UfcsFunctionAnnotation {
    pub self_arg: Box<Expr>,
    pub function_name: IdentPath,
    pub sig: Rc<FunctionSig>,

    #[derivative(Debug = "ignore")]
    #[derivative(Hash = "ignore")]
    #[derivative(PartialEq = "ignore")]
    pub span: Span,
}

impl UfcsFunctionAnnotation {
    pub fn func_ty(&self) -> Type {
        Type::Function(self.sig.clone())
    }

    pub fn should_call_noargs_in_expr(&self, expect_ty: &Type) -> bool {
        let self_arg_ty = self.self_arg.annotation().ty();

        self.sig.should_call_noargs_in_expr(expect_ty, Some(self_arg_ty.as_ref()))
    }
}

impl From<UfcsFunctionAnnotation> for TypeAnnotation {
    fn from(a: UfcsFunctionAnnotation) -> Self {
        TypeAnnotation::UfcsFunction(Rc::new(a))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeAnnotation {
    Untyped(Span),
    TypedValue(Rc<TypedValueAnnotation>),

    Function(Rc<FunctionAnnotation>),
    UfcsFunction(Rc<UfcsFunctionAnnotation>),

    // direct method reference e.g. `Interface.Method`
    InterfaceMethod(Rc<InterfaceMethodAnnotation>),
    Type(Type, Span),
    Namespace(IdentPath, Span),
    VariantCtor(Rc<VariantCtorAnnotation>),

    // as-yet unresolved function that may refer to 1+ functions (interface methods, ufcs functions,
    // or free functions)
    Overload(Rc<OverloadAnnotation>),

    Const(Rc<ConstAnnotation>),
}

impl TypeAnnotation {
    pub fn expect_value(&self, expect_ty: &Type) -> TypecheckResult<()> {
        assert_ne!(Type::Nothing, *expect_ty);

        let (actual_ty, span) = match self {
            TypeAnnotation::InterfaceMethod(method) => (method.func_ty(), &method.span),
            TypeAnnotation::Overload(overload) => (overload.func_ty(), &overload.span),
            TypeAnnotation::Function(func) => (func.func_ty(), &func.span),
            TypeAnnotation::TypedValue(val) => (val.ty.clone(), &val.span),
            TypeAnnotation::Const(const_val) => (const_val.ty.clone(), &const_val.span),

            TypeAnnotation::UfcsFunction(call) => (call.func_ty(), &call.span),

            TypeAnnotation::Untyped(span)
            | TypeAnnotation::Namespace(_, span)
            | TypeAnnotation::Type(_, span) => (Type::Nothing, span),

            TypeAnnotation::VariantCtor(ctor) => {
                let variant_ty = Type::Variant(Box::new(Symbol {
                    qualified: ctor.variant_name.clone(),
                    decl_name: TypeDeclName::from(ctor.variant_name.last().clone()),
                    type_args: None,
                }));

                (variant_ty, &ctor.span)
            },
        };

        if actual_ty == *expect_ty {
            Ok(())
        } else {
            Err(TypecheckError::TypeMismatch {
                span: span.clone(),
                expected: expect_ty.clone(),
                actual: actual_ty,
            })
        }
    }

    pub fn new_temp_val(ty: Type, span: Span) -> Self {
        let typed_val = TypedValueAnnotation {
            decl: None,
            value_kind: ValueKind::Temporary,
            ty,
            span,
        };
        typed_val.into()
    }

    pub fn ty(&self) -> Cow<Type> {
        match self {
            TypeAnnotation::Namespace(_, _)
            | TypeAnnotation::Untyped(_)
            | TypeAnnotation::Type(_, _)
            | TypeAnnotation::VariantCtor(..) => Cow::Owned(Type::Nothing),

            TypeAnnotation::Function(func) => Cow::Owned(func.func_ty()),
            TypeAnnotation::UfcsFunction(call) => Cow::Owned(call.func_ty()),
            TypeAnnotation::InterfaceMethod(method) => Cow::Owned(method.func_ty()),
            TypeAnnotation::Overload(overload) => Cow::Owned(overload.func_ty()),

            TypeAnnotation::Const(const_val) => Cow::Borrowed(&const_val.ty),
            TypeAnnotation::TypedValue(val) => Cow::Borrowed(&val.ty),
        }
    }

    pub fn decl(&self) -> Option<&Ident> {
        match self {
            TypeAnnotation::Type(..) => None,
            TypeAnnotation::Function { .. } => None, // TODO
            TypeAnnotation::InterfaceMethod(..) => None, // TODO
            TypeAnnotation::UfcsFunction { .. } => None, // TODO
            TypeAnnotation::Overload { .. } => None, // TODO

            TypeAnnotation::TypedValue(val) => val.decl.as_ref(),
            TypeAnnotation::Untyped(..) => None,
            TypeAnnotation::Namespace(ident, ..) => Some(ident.last()),

            TypeAnnotation::Const(const_val) => const_val.decl.as_ref(),

            TypeAnnotation::VariantCtor(ctor) => Some(ctor.variant_name.last()),
        }
    }

    pub fn value_kind(&self) -> Option<ValueKind> {
        match self {
            TypeAnnotation::TypedValue(val) => Some(val.value_kind),
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
            TypeAnnotation::InterfaceMethod(method) => &method.span,
            TypeAnnotation::VariantCtor(ctor) => &ctor.span,
            TypeAnnotation::Overload(overload) => &overload.span,
            TypeAnnotation::TypedValue(val) => &val.span,
            TypeAnnotation::Const(const_val) => &const_val.span,
            TypeAnnotation::Function(func) => &func.span,
            TypeAnnotation::UfcsFunction(call) => &call.span,

            TypeAnnotation::Untyped(span)
            | TypeAnnotation::Type(_, span)
            | TypeAnnotation::Namespace(_, span) => span,
        }
    }
}

impl Annotation for TypeAnnotation {
    type Type = Type;
    type Name = Symbol;
    type Pattern = TypePattern;
    type ConstStringExpr = String;
    type ConstIntegerExpr = IntConstant;
    type ConstExpr = Literal;
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
            })
        }
    }
}

impl Specializable for Symbol {
    type GenericID = IdentPath;

    /// is this either a type without type args, or does it already have all the type args it needs?
    fn is_unspecialized_generic(&self) -> bool {
        if self.decl_name.type_params.is_none() {
            return false;
        }

        match &self.type_args {
            None => true,
            Some(type_args) => type_args
                .items
                .iter()
                .any(|arg| arg.is_unspecialized_generic()),
        }
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
