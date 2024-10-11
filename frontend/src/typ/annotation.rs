mod symbol;

pub use symbol::*;

use crate::ast::Access;
use crate::ast::Annotation;
use crate::ast::Ident;
use crate::ast::IdentPath;
use crate::typ::ast::Expr;
use crate::typ::ast::Literal;
use crate::typ::ast::Method;
use crate::typ::ast::OverloadCandidate;
use crate::typ::ast::TypedFunctionName;
use crate::typ::result::*;
use crate::typ::ty::*;
use crate::typ::ValueKind;
use crate::IntConstant;
use common::span::*;
use derivative::*;
use std::borrow::Cow;
use std::fmt;
use std::rc::Rc;

#[derive(Clone, Eq, Derivative)]
#[derivative(Debug, PartialEq, Hash)]
pub struct VariantCtorTyped {
    #[derivative(Debug = "ignore")]
    #[derivative(PartialEq = "ignore")]
    #[derivative(Hash = "ignore")]
    pub span: Span,

    // variant ctors don't know the type args of their variant, it must be inferred from context
    pub variant_name: Rc<Symbol>,

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
}

impl OverloadTyped {
    pub fn method(
        iface_ty: Type,
        self_arg: Expr,
        method: Method,
        span: Span
    ) -> Self {
        let sig = Rc::new(FunctionSig::of_decl(&method.decl));

        Self {
            span,
            self_arg: Some(Box::new(self_arg)),
            sig: Some(sig.clone()),
            candidates: vec![OverloadCandidate::Method {
                ident: method.decl.name.ident.clone(),
                method,
                sig,
                owning_ty: iface_ty,
            }],
        }
    }

    pub fn new(
        candidates: Vec<OverloadCandidate>,
        self_arg: Option<Box<Expr>>,
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
    pub owning_ty: Type,

    pub method_ident: Ident,
    pub method_access: Access,

    pub span: Span,

    pub method_sig: Rc<FunctionSig>,
}

impl MethodTyped {
    pub fn new(method: &Method, iface_ty: Type, span: Span) -> Self {
        let sig = FunctionSig::of_decl(&method.decl);

        Self {
            owning_ty: iface_ty,
            method_ident: method.decl.name.ident.clone(),
            method_access: method.access,
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
        Typed::Method(Rc::new(a))
    }
}

#[derive(Eq, Clone, Derivative)]
#[derivative(Hash, Debug, PartialEq)]
pub struct FunctionTyped {
    pub name: Symbol,
    pub sig: Rc<FunctionSig>,

    #[derivative(Debug = "ignore")]
    #[derivative(PartialEq = "ignore")]
    #[derivative(Hash = "ignore")]
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
    pub function_name: Symbol,
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

#[derive(Clone, Eq, Derivative)]
#[derivative(Debug, Hash, PartialEq)]
pub enum Typed {
    Untyped(
        #[derivative(Debug = "ignore")]
        #[derivative(PartialEq = "ignore")]
        #[derivative(Hash = "ignore")]
        Span
    ),
    TypedValue(Rc<TypedValue>),

    Function(Rc<FunctionTyped>),
    UfcsFunction(Rc<UfcsTyped>),

    // direct method reference e.g. `Interface.Method`
    Method(Rc<MethodTyped>),
    Type(
        Type,
        #[derivative(Debug = "ignore")]
        #[derivative(PartialEq = "ignore")]
        #[derivative(Hash = "ignore")]
        Span
    ),
    Namespace(
        IdentPath,
        #[derivative(Debug = "ignore")]
        #[derivative(PartialEq = "ignore")]
        #[derivative(Hash = "ignore")]
        Span
    ),
    VariantCtor(Rc<VariantCtorTyped>),

    // as-yet unresolved function that may refer to 1+ functions (interface methods, ufcs functions,
    // or free functions)
    Overload(Rc<OverloadTyped>),

    Const(Rc<ConstTyped>),
}

impl Typed {
    pub fn expect_value(&self, expect_ty: &Type) -> TypeResult<()> {
        assert_ne!(Type::Nothing, *expect_ty);

        let (actual_ty, span) = match self {
            Typed::Method(method) => (method.func_ty(), &method.span),
            Typed::Overload(overload) => (overload.func_ty(), &overload.span),
            Typed::Function(func) => (func.func_ty(), &func.span),
            Typed::TypedValue(val) => (val.ty.clone(), &val.span),
            Typed::Const(const_val) => (const_val.ty.clone(), &const_val.span),

            Typed::UfcsFunction(call) => (call.func_ty(), &call.span),

            Typed::Untyped(span)
            | Typed::Namespace(_, span)
            | Typed::Type(_, span) => (Type::Nothing, span),

            Typed::VariantCtor(ctor) => {
                let variant_ty = Type::variant((*ctor.variant_name).clone());

                (variant_ty, &ctor.span)
            },
        };

        if actual_ty == *expect_ty {
            Ok(())
        } else {
            Err(TypeError::TypeMismatch {
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
            Typed::Method(method) => Cow::Owned(method.func_ty()),
            Typed::Overload(overload) => Cow::Owned(overload.func_ty()),

            Typed::Const(const_val) => Cow::Borrowed(&const_val.ty),
            Typed::TypedValue(val) => Cow::Borrowed(&val.ty),
        }
    }

    pub fn decl(&self) -> Option<&Ident> {
        match self {
            Typed::Type(..) => None,
            Typed::Function { .. } => None, // TODO
            Typed::Method(..) => None, // TODO
            Typed::UfcsFunction { .. } => None, // TODO
            Typed::Overload { .. } => None, // TODO

            Typed::TypedValue(val) => val.decl.as_ref(),
            Typed::Untyped(..) => None,
            Typed::Namespace(ident, ..) => Some(ident.last()),

            Typed::Const(const_val) => const_val.decl.as_ref(),

            Typed::VariantCtor(ctor) => Some(ctor.variant_name.ident()),
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
            Typed::Method(method) => &method.span,
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
    type FunctionName = TypedFunctionName;
    
    type ConstStringExpr = String;
    type ConstIntegerExpr = IntConstant;
    type ConstExpr = Literal;
}
