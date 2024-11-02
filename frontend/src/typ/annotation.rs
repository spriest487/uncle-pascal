mod symbol;

pub use symbol::*;

use crate::ast::Annotation;
use crate::ast::Ident;
use crate::ast::IdentPath;
use crate::ast::Visibility;
use crate::typ::ast::Literal;
use crate::typ::ast::MethodDecl;
use crate::typ::ast::OverloadCandidate;
use crate::typ::ast::TypedFunctionName;
use crate::typ::ast::Expr;
use crate::typ::ast::FunctionDecl;
use crate::typ::result::*;
use crate::typ::ty::*;
use crate::typ::Context;
use crate::typ::ValueKind;
use crate::IntConstant;
use common::span::*;
use derivative::*;
use std::borrow::Cow;
use std::fmt;
use std::rc::Rc;

#[derive(Clone, Eq, Derivative)]
#[derivative(Debug, PartialEq, Hash)]
pub struct VariantCaseTyped {
    #[derivative(Debug = "ignore")]
    #[derivative(PartialEq = "ignore")]
    #[derivative(Hash = "ignore")]
    pub span: Span,

    // variant ctors don't know the type args of their variant, it must be inferred from context
    pub variant_name: Rc<Symbol>,

    pub case: Ident,
}

impl From<VariantCaseTyped> for Typed {
    fn from(a: VariantCaseTyped) -> Self {
        Typed::VariantCase(Rc::new(a))
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
    pub sig: Option<Rc<FunctionSig>>,

    pub self_arg: Option<Box<Expr>>,
}

impl OverloadTyped {
    pub fn method(
        self_ty: Type,
        iface_ty: Type,
        index: usize,
        self_arg: Expr,
        method: MethodDecl,
        span: Span
    ) -> Self {
        let sig = Rc::new(method.func_decl.sig());

        Self {
            span,
            self_arg: Some(Box::new(self_arg)),
            sig: Some(sig.clone()),
            candidates: vec![OverloadCandidate::Method {
                iface_ty,
                self_ty,
                index,
                decl: method.clone(),
            }],
        }
    }

    pub fn new(
        candidates: Vec<OverloadCandidate>,
        self_arg: Option<Box<Expr>>,
        span: Span,
    ) -> Self {
        let sig = if candidates.len() == 1 {
            Some(Rc::new(candidates[0].decl().sig()))
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

#[derive(Clone, Eq, Derivative)]
#[derivative(Debug, PartialEq, Hash)]
pub struct MethodTyped {
    /// the type via which this method is being referred to. we don't distinguish here between
    /// an interface method (implemented on a type other than the self type) and a direct method
    /// call (known to be implemented on the self type used here)
    pub self_ty: Type,
    pub index: usize,
    
    // members below this point are just cached for convenience, all of these can be
    // fetched from the type by the index

    /// span of this reference to the method (not the method's own decl)
    #[derivative(PartialEq = "ignore")]
    #[derivative(Hash = "ignore")]
    #[derivative(Debug = "ignore")]
    pub span: Span,

    #[derivative(PartialEq = "ignore")]
    #[derivative(Hash = "ignore")]
    #[derivative(Debug = "ignore")]
    pub decl: MethodDecl,
}

impl MethodTyped {
    pub fn new(self_ty: Type, index: usize, decl: MethodDecl, span: Span) -> Self {
        Self {
            self_ty,
            index,
            span,
            decl,
        }
    }

    pub fn func_ty(&self) -> Type {
        Type::Function(Rc::new(self.decl.func_decl.sig()))
    }

    pub fn should_call_noargs_in_expr(&self, expect_ty: &Type, self_arg: &Type) -> bool {
        self.decl.func_decl.sig().should_call_noargs_in_expr(expect_ty, self_arg)
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
    pub visibility: Visibility,

    pub decl: Rc<FunctionDecl>,
    pub sig: Rc<FunctionSig>,

    #[derivative(Debug = "ignore")]
    #[derivative(PartialEq = "ignore")]
    #[derivative(Hash = "ignore")]
    pub span: Span,
}

impl FunctionTyped {
    pub fn new(name: Symbol, visibility: Visibility, decl: Rc<FunctionDecl>, span: Span) -> Self {
        Self {
            name,
            visibility,
            sig: Rc::new(decl.sig()),
            decl,
            span,
        }
    }
    
    pub fn func_ty(&self) -> Type {
        Type::Function(self.sig.clone())
    }

    pub fn should_call_noargs_in_expr(&self, expect_ty: &Type) -> bool {
        self.sig.should_call_noargs_in_expr(expect_ty, &Type::Nothing)
    }
    
    pub fn check_visible(&self, at: &Span, ctx: &Context) -> TypeResult<()> {
        if self.visibility < Visibility::Interface
            && !ctx.is_current_namespace_child(&self.name.full_path) {
            return Err(TypeError::NameNotVisible {
                name: self.name.full_path.clone(),
                span: at.clone(),
            });
        }
        
        Ok(())
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

impl TypedValue {
    pub fn temp(ty: Type, span: Span) -> Self {
        TypedValue {
            ty,
            span,
            value_kind: ValueKind::Temporary,
            decl: None,
        }
    }
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
    pub function_name: Symbol,
    pub visibility: Visibility,
    
    pub self_arg: Box<Expr>,
    
    pub decl: Rc<FunctionDecl>,
    pub sig: Rc<FunctionSig>,

    #[derivative(Debug = "ignore")]
    #[derivative(Hash = "ignore")]
    #[derivative(PartialEq = "ignore")]
    pub span: Span,
}

impl UfcsTyped {
    pub fn new(
        function_name: Symbol,
        visibility: Visibility,
        self_arg: Expr,
        decl: Rc<FunctionDecl>,
        span: Span
    ) -> Self {
        Self {
            self_arg: Box::new(self_arg),
            function_name,
            sig: Rc::new(decl.sig()),
            decl,
            visibility,
            span,
        }
    }
    
    pub fn func_ty(&self) -> Type {
        Type::Function(self.sig.clone())
    }

    pub fn should_call_noargs_in_expr(&self, expect_ty: &Type) -> bool {
        let self_arg_ty = self.self_arg.annotation().ty();

        self.decl.sig().should_call_noargs_in_expr(expect_ty, self_arg_ty.as_ref())
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
    VariantCase(Rc<VariantCaseTyped>),

    // as-yet unresolved function that may refer to 1+ functions (interface methods, ufcs functions,
    // or free functions)
    Overload(Rc<OverloadTyped>),

    Const(Rc<ConstTyped>),
}

impl Typed {
    pub fn expect_any_value(&self) -> TypeResult<()> {
        self.expect_value(&Type::Nothing)
    }
    
    pub fn expect_value(&self, expect_ty: &Type) -> TypeResult<()> {
        let (actual_ty, span) = match self {
            Typed::Method(method) => (method.func_ty(), &method.span),
            Typed::TypedValue(val) => (val.ty.clone(), &val.span),
            Typed::Const(const_val) => (const_val.ty.clone(), &const_val.span),
            Typed::Function(func) => (func.func_ty(), &func.span),

            Typed::Overload(..)
            | Typed::UfcsFunction(..)
            | Typed::Untyped(..)
            | Typed::Namespace(..)
            | Typed::Type(..)
            | Typed::VariantCase(..) => {
                return Err(TypeError::NotValueExpr { 
                    expected: expect_ty.clone(),
                    actual: self.clone(),
                });
            }
        };

        if actual_ty == Type::Nothing {
            return Err(TypeError::NotValueExpr {
                expected: expect_ty.clone(),
                actual: self.clone(),
            });
        }
        
        if actual_ty != *expect_ty && *expect_ty != Type::Nothing {
            return Err(TypeError::TypeMismatch {
                span: span.clone(),
                expected: expect_ty.clone(),
                actual: actual_ty,
            });
        }
        
        Ok(())
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
            | Typed::VariantCase(..) => Cow::Owned(Type::Nothing),

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

            Typed::VariantCase(ctor) => Some(ctor.variant_name.ident()),
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
        match self {
            Typed::Untyped(_) => { 
                write!(f, "untyped value") 
            },
            Typed::TypedValue(val) => { 
                write!(f, "{} of type {}", val.value_kind, val.ty) 
            },
            Typed::Function(func) => { 
                write!(f, "function {}", func.name) 
            },
            Typed::UfcsFunction(func) => { 
                write!(f, "function {}", func.function_name) 
            },
            Typed::Method(method) => { 
                write!(f, "method {}.{}", method.self_ty, method.decl.func_decl.ident()) 
            },
            Typed::Type(ty, ..) => { 
                write!(f, "type {}", ty) 
            },
            Typed::Namespace(ns, ..) => { 
                write!(f, "namespace {}", ns) 
            },
            Typed::VariantCase(case) => { 
                write!(f, "variant case {}.{}", case.variant_name, case.case) 
            },
            Typed::Overload(overload) => { 
                write!(f, "overloaded function")?;
                if let Some(sig) = &overload.sig {
                    write!(f, " with signature {}", sig)?;
                }
                Ok(())
            },
            Typed::Const(const_val) => { 
                write!(f, "constant")?;
                if let Some(decl) = &const_val.decl {
                    write!(f, " {}", decl)?;
                }
                write!(f, "({})", const_val.value)
            },
        }
    }
}

impl Spanned for Typed {
    fn span(&self) -> &Span {
        match self {
            Typed::Method(method) => &method.span,
            Typed::VariantCase(ctor) => &ctor.span,
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
