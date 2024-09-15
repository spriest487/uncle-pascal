mod symbol;

pub use symbol::*;

use crate::ast::Annotation;
use crate::ast::TypeDeclName;
use crate::ast::TypeList;
use crate::ast::IdentPath;
use crate::typecheck::ast::Expr;
use crate::typecheck::ast::FunctionDecl;
use crate::typecheck::ast::Literal;
use crate::typecheck::ast::OverloadCandidate;
use crate::typecheck::result::*;
use crate::typecheck::ty::*;
use crate::typecheck::ValueKind;
use crate::ast::Ident;
use crate::IntConstant;
use derivative::*;
use pas_common::span::*;
use std::borrow::Cow;
use std::fmt;
use std::rc::Rc;

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct VariantCtorTyped {
    pub span: Span,

    // variant ctors don't know the type args of their variant, it must be inferred from context
    pub variant_name: IdentPath,

    pub case: Ident,
}

impl From<VariantCtorTyped> for Typed {
    fn from(a: VariantCtorTyped) -> Self {
        Typed::VariantCtor(Rc::new(a))
    }
}

#[derive(Eq, Clone, Derivative)]
#[derivative(Hash, Debug, PartialEq)]
pub struct OverloadTyped {
    #[derivative(Debug = "ignore")]
    #[derivative(Hash = "ignore")]
    #[derivative(PartialEq = "ignore")]
    pub span: Span,

    pub candidates: Vec<OverloadCandidate>,
    sig: Option<Rc<FunctionSig>>,

    pub self_arg: Option<Box<Expr>>,

    pub type_args: Vec<Type>,
}

impl OverloadTyped {
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

impl From<OverloadTyped> for Typed {
    fn from(a: OverloadTyped) -> Self {
        Typed::Overload(Rc::new(a))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MethodTyped {
    pub iface_ty: Type,
    pub method_ident: Ident,
    pub span: Span,

    pub method_sig: Rc<FunctionSig>,
}

impl MethodTyped {
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
        self.method_sig.should_call_noargs_in_expr(expect_ty, self_arg)
    }
}

impl From<MethodTyped> for Typed {
    fn from(a: MethodTyped) -> Self {
        Typed::InterfaceMethod(Rc::new(a))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FunctionTyped {
    pub ident: IdentPath,
    pub sig: Rc<FunctionSig>,
    pub type_args: Option<TypeList<Type>>,
    pub span: Span,
}

impl FunctionTyped {
    pub fn func_ty(&self) -> Type {
        Type::Function(self.sig.clone())
    }

    pub fn should_call_noargs_in_expr(&self, expect_ty: &Type) -> bool {
        self.sig.should_call_noargs_in_expr(expect_ty, &Type::Nothing)
    }
}

impl From<FunctionTyped> for Typed {
    fn from(a: FunctionTyped) -> Self {
        Typed::Function(Rc::new(a))
    }
}

#[derive(Eq, Clone, Derivative)]
#[derivative(Hash, Debug, PartialEq)]
pub struct TypedValue {
    pub ty: Type,
    pub value_kind: ValueKind,
    pub decl: Option<Ident>,

    #[derivative(Debug = "ignore")]
    #[derivative(Hash = "ignore")]
    #[derivative(PartialEq = "ignore")]
    pub span: Span,
}

impl From<TypedValue> for Typed {
    fn from(a: TypedValue) -> Self {
        Typed::TypedValue(Rc::new(a))
    }
}

#[derive(Eq, Clone, Derivative)]
#[derivative(Hash, Debug, PartialEq)]
pub struct ConstTyped {
    pub decl: Option<Ident>,
    pub ty: Type,

    pub value: Literal,

    #[derivative(Debug = "ignore")]
    #[derivative(Hash = "ignore")]
    #[derivative(PartialEq = "ignore")]
    pub span: Span,
}

impl From<ConstTyped> for Typed {
    fn from(a: ConstTyped) -> Self {
        Typed::Const(Rc::new(a))
    }
}

#[derive(Eq, Clone, Derivative)]
#[derivative(Hash, Debug, PartialEq)]
pub struct UfcsTyped {
    pub self_arg: Box<Expr>,
    pub function_name: IdentPath,
    pub sig: Rc<FunctionSig>,

    #[derivative(Debug = "ignore")]
    #[derivative(Hash = "ignore")]
    #[derivative(PartialEq = "ignore")]
    pub span: Span,
}

impl UfcsTyped {
    pub fn func_ty(&self) -> Type {
        Type::Function(self.sig.clone())
    }

    pub fn should_call_noargs_in_expr(&self, expect_ty: &Type) -> bool {
        let self_arg_ty = self.self_arg.annotation().ty();

        self.sig.should_call_noargs_in_expr(expect_ty, self_arg_ty.as_ref())
    }
}

impl From<UfcsTyped> for Typed {
    fn from(a: UfcsTyped) -> Self {
        Typed::UfcsFunction(Rc::new(a))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Typed {
    Untyped(Span),
    TypedValue(Rc<TypedValue>),

    Function(Rc<FunctionTyped>),
    UfcsFunction(Rc<UfcsTyped>),

    // direct method reference e.g. `Interface.Method`
    InterfaceMethod(Rc<MethodTyped>),
    Type(Type, Span),
    Namespace(IdentPath, Span),
    VariantCtor(Rc<VariantCtorTyped>),

    // as-yet unresolved function that may refer to 1+ functions (interface methods, ufcs functions,
    // or free functions)
    Overload(Rc<OverloadTyped>),

    Const(Rc<ConstTyped>),
}

impl Typed {
    pub fn expect_value(&self, expect_ty: &Type) -> TypecheckResult<()> {
        assert_ne!(Type::Nothing, *expect_ty);

        let (actual_ty, span) = match self {
            Typed::InterfaceMethod(method) => (method.func_ty(), &method.span),
            Typed::Overload(overload) => (overload.func_ty(), &overload.span),
            Typed::Function(func) => (func.func_ty(), &func.span),
            Typed::TypedValue(val) => (val.ty.clone(), &val.span),
            Typed::Const(const_val) => (const_val.ty.clone(), &const_val.span),

            Typed::UfcsFunction(call) => (call.func_ty(), &call.span),

            Typed::Untyped(span)
            | Typed::Namespace(_, span)
            | Typed::Type(_, span) => (Type::Nothing, span),

            Typed::VariantCtor(ctor) => {
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
        let typed_val = TypedValue {
            decl: None,
            value_kind: ValueKind::Temporary,
            ty,
            span,
        };
        typed_val.into()
    }

    pub fn ty(&self) -> Cow<Type> {
        match self {
            Typed::Namespace(_, _)
            | Typed::Untyped(_)
            | Typed::Type(_, _)
            | Typed::VariantCtor(..) => Cow::Owned(Type::Nothing),

            Typed::Function(func) => Cow::Owned(func.func_ty()),
            Typed::UfcsFunction(call) => Cow::Owned(call.func_ty()),
            Typed::InterfaceMethod(method) => Cow::Owned(method.func_ty()),
            Typed::Overload(overload) => Cow::Owned(overload.func_ty()),

            Typed::Const(const_val) => Cow::Borrowed(&const_val.ty),
            Typed::TypedValue(val) => Cow::Borrowed(&val.ty),
        }
    }

    pub fn decl(&self) -> Option<&Ident> {
        match self {
            Typed::Type(..) => None,
            Typed::Function { .. } => None, // TODO
            Typed::InterfaceMethod(..) => None, // TODO
            Typed::UfcsFunction { .. } => None, // TODO
            Typed::Overload { .. } => None, // TODO

            Typed::TypedValue(val) => val.decl.as_ref(),
            Typed::Untyped(..) => None,
            Typed::Namespace(ident, ..) => Some(ident.last()),

            Typed::Const(const_val) => const_val.decl.as_ref(),

            Typed::VariantCtor(ctor) => Some(ctor.variant_name.last()),
        }
    }

    pub fn value_kind(&self) -> Option<ValueKind> {
        match self {
            Typed::TypedValue(val) => Some(val.value_kind),
            Typed::Const { .. } => Some(ValueKind::Immutable),
            _ => None,
        }
    }

    pub fn is_namespace(&self) -> bool {
        match self {
            Typed::Namespace(_, _) => true,
            _ => false,
        }
    }
}

impl fmt::Display for Typed {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.span())
    }
}

impl Spanned for Typed {
    fn span(&self) -> &Span {
        match self {
            Typed::InterfaceMethod(method) => &method.span,
            Typed::VariantCtor(ctor) => &ctor.span,
            Typed::Overload(overload) => &overload.span,
            Typed::TypedValue(val) => &val.span,
            Typed::Const(const_val) => &const_val.span,
            Typed::Function(func) => &func.span,
            Typed::UfcsFunction(call) => &call.span,

            Typed::Untyped(span)
            | Typed::Type(_, span)
            | Typed::Namespace(_, span) => span,
        }
    }
}

impl Annotation for Typed {
    type Type = Type;
    type Name = Symbol;
    type Pattern = TypePattern;
    type ConstStringExpr = String;
    type ConstIntegerExpr = IntConstant;
    type ConstExpr = Literal;
}
