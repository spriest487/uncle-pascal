pub use self::pattern::*;
pub use self::primitive::*;
pub use self::sig::*;
pub use self::ty_param::*;
use crate::ast;
use crate::ast::ArrayTypeName;
use crate::ast::Ident;
use crate::ast::IdentPath;
use crate::ast::IdentTypeName;
use crate::ast::StructKind;
use crate::ast::TypeAnnotation;
use crate::typ::ast::const_eval_integer;
use crate::typ::ast::typecheck_expr;
use crate::typ::ast::FunctionDecl;
use crate::typ::ast::Literal;
use crate::typ::ast::Member;
use crate::typ::ast::StructDef;
use crate::typ::ast::VariantDef;
use crate::typ::context;
use crate::typ::result::*;
use crate::typ::Context;
use crate::typ::GenericError;
use crate::typ::GenericResult;
use crate::typ::GenericTarget;
use crate::typ::NameResult;
use crate::typ::Symbol;
use crate::typ::TypeArgsResult::NotGeneric;
use crate::typ::Typed;
use crate::Operator;
use common::span::*;
use std::borrow::Cow;
use std::fmt;
use std::rc::Rc;

#[cfg(test)]
mod test;

pub mod layout;
pub mod pattern;
pub mod primitive;
pub mod sig;
pub mod ty_param;

#[derive(Eq, PartialEq, Hash, Clone, Debug)]
pub enum Type {
    Nothing,
    Nil,
    Primitive(Primitive),
    Pointer(Box<Type>),
    Function(Rc<FunctionSig>),
    Record(Box<Symbol>),
    Class(Box<Symbol>),
    Interface(Box<IdentPath>),
    Variant(Box<Symbol>),
    Array(Rc<ArrayType>),
    DynArray { element: Box<Type> },
    MethodSelf,
    GenericParam(Box<TypeParamType>),
    Any,
    Enum(Box<Symbol>),
}

impl From<Primitive> for Type {
    fn from(primitive: Primitive) -> Self {
        Type::Primitive(primitive)
    }
}

impl From<ArrayType> for Type {
    fn from(array_ty: ArrayType) -> Self {
        Type::Array(Rc::new(array_ty))
    }
}

impl Type {
    pub fn full_path(&self) -> Option<IdentPath> {
        fn builtin_path(name: &str) -> IdentPath {
            let builtin_span = Span::zero("<builtin>");
            IdentPath::new(Ident::new(name, builtin_span), vec![])
        }

        match self {
            Type::Nothing => Some(builtin_path("Nothing")),
            Type::Any => Some(builtin_path("Any")),
            Type::MethodSelf => Some(builtin_path("Self")),
            Type::Primitive(p) => Some(builtin_path(p.name())),
            Type::Interface(iface) => Some((**iface).clone()),
            Type::Record(class) | Type::Class(class) => Some(class.qualified.clone()),
            Type::Variant(variant) => Some(variant.qualified.clone()),
            _ => None,
        }
    }

    pub fn is_by_ref(&self) -> bool {
        match self {
            Type::Nothing => false,
            Type::Nil => false,
            Type::Primitive(_) => false,
            Type::Pointer(_) => false,
            Type::Function(_) => false,
            Type::Record(_) => false,
            Type::Variant(_) => false,
            Type::Array { .. } => false,
            Type::GenericParam(_) => false,
            Type::Enum(..) => false,

            Type::Class(_) => true,
            Type::Interface(_) => true,
            Type::DynArray { .. } => true,
            Type::MethodSelf => true,
            Type::Any => true,
        }
    }

    pub fn default_val(&self) -> Option<Literal> {
        match self {
            Type::Function(_)
            | Type::Nothing
            | Type::Record(_)
            | Type::Class(_)
            | Type::Interface(_)
            | Type::Variant(_)
            | Type::Array(_)
            | Type::DynArray { .. }
            | Type::MethodSelf
            | Type::GenericParam(_)
            | Type::Any
            | Type::Enum(_) => None,

            Type::Nil => Some(Literal::Nil),
            Type::Primitive(p) => Some(p.default_val()),
            Type::Pointer(..) => Some(Literal::Nil),
        }
    }

    pub fn of_decl(type_decl: &ast::TypeDeclItem<Typed>) -> Self {
        match type_decl {
            ast::TypeDeclItem::Struct(class) if class.kind == StructKind::Record => {
                Type::Record(Box::new(class.name.clone()))
            },

            ast::TypeDeclItem::Struct(class) => Type::Class(Box::new(class.name.clone())),

            ast::TypeDeclItem::Variant(variant) => Type::Variant(Box::new(variant.name.clone())),

            ast::TypeDeclItem::Interface(iface) => {
                Type::Interface(Box::new(iface.name.qualified.clone()))
            },

            ast::TypeDeclItem::Enum(enum_decl) => Type::Enum(Box::new(enum_decl.name.clone())),

            ast::TypeDeclItem::Alias(alias) => (*alias.ty).clone(),
        }
    }

    pub fn find_data_member(&self, member: &Ident, ctx: &Context) -> NameResult<Option<Member>> {
        match self {
            Type::Class(class_name) | Type::Record(class_name) => {
                let def = ctx.instantiate_struct_def(class_name)?;

                Ok(def.find_member(member).cloned())
            },

            _ => Ok(None),
        }
    }

    pub fn get_member(&self, index: usize, ctx: &Context) -> NameResult<Option<Member>> {
        match self {
            Type::Record(class) | Type::Class(class) => {
                let class = ctx.instantiate_struct_def(class)?;

                Ok(class.members.get(index).cloned())
            },

            _ => Ok(None),
        }
    }

    pub fn members_len(&self, ctx: &Context) -> NameResult<usize> {
        match self {
            Type::Record(class) | Type::Class(class) => {
                let class = ctx.instantiate_struct_def(class)?;
                Ok(class.members.len())
            },

            _ => Ok(0),
        }
    }

    pub fn members(&self, ctx: &Context) -> NameResult<Vec<Member>> {
        let mut members = Vec::new();
        for i in 0..self.members_len(ctx)? {
            let member = self.get_member(i, ctx)?.unwrap();

            members.push(member);
        }

        Ok(members)
    }

    pub fn is_generic_param(&self) -> bool {
        match self {
            Type::GenericParam(..) => true,
            _ => false,
        }
    }

    /// is this type, or any of the type parameters that it contains, a generic param type?
    /// e.g. in the sig `X[T](a: Box[T])`, the type of param `a` is "Box of type param 0".
    /// if this type appears in a context where the params already refer to specific types, 
    /// for example in the body of a function where this type refers to one of the function's type
    /// params, we ignore those types since they'll be real types when actually used
    pub fn contains_generic_params(&self, ctx: &Context) -> bool {
        if let Type::GenericParam(ty_param_ty) = self {
            let current_func_ty_params = ctx.current_function_env()
                .and_then(|env| env.ty_params.as_ref());

            return if let Some(ctx_ty_params) = current_func_ty_params {
                let is_param_defined_in_scope = ctx_ty_params.iter()
                    .find(|p| ty_param_ty.name == p.name)
                    .is_some();

                !is_param_defined_in_scope
            } else {
                true
            }
        }

        if let TypeArgsResult::Specialized(type_args) = &self.type_args() {
            return type_args.items.iter().any(|a| a.contains_generic_params(ctx));
        }

        if let Some(array_el) = self.array_element_ty() {
            return array_el.contains_generic_params(ctx);
        }

        false
    }

    pub fn same_array_dim(&self, other: &Self) -> bool {
        match (self, other) {
            (Type::Array(a), Type::Array(b)) => a.dim == b.dim,

            _ => false,
        }
    }

    pub fn same_decl_type(&self, other: &Self) -> bool {
        match (self.full_path(), other.full_path()) {
            (Some(path), Some(other_path)) => path == other_path,
            _ => false,
        }
    }

    /// get the type args this type is specialized with
    /// e.g. for the type `Box[Integer]`, the type list contains `Integer`
    /// returns `None` for non-generic types and unspecialized generic types
    pub fn type_args(&self) -> TypeArgsResult {
        match self {
            Type::Variant(name) | Type::Class(name) | Type::Record(name) => {
                match (&name.decl_name.type_params, &name.type_args) {
                    (Some(type_params), None) => TypeArgsResult::Unspecialized(type_params),
                    (Some(..), Some(type_args)) => TypeArgsResult::Specialized(type_args),
                    (None, None) => TypeArgsResult::NotGeneric,
                    (None, Some(..)) => unreachable!(),
                }
            },

            _ => NotGeneric,
        }
    }

    pub fn type_params(&self) -> Option<&ast::TypeList<Ident>> {
        match self {
            Type::Variant(name) | Type::Class(name) | Type::Record(name) => {
                name.decl_name.type_params.as_ref()
            },

            _ => None,
        }
    }

    pub fn array_element_ty(&self) -> Option<&Type> {
        match self {
            Type::DynArray { element } => Some(element),
            Type::Array(array_ty) => Some(&array_ty.element_ty),

            _ => None,
        }
    }

    pub fn is_rc_reference(&self) -> bool {
        match self {
            Type::Class(..) => true,
            Type::Interface(..) => true,
            Type::Any => true,
            Type::DynArray { .. } => true,

            _ => false,
        }
    }

    /// Is this a value pointer type, not including primitive pointer types and RC pointers?
    pub fn is_pointer(&self) -> bool {
        match self {
            Type::Pointer(..) => true,
            _ => false,
        }
    }

    pub fn deref_ty(&self) -> Option<&Type> {
        match self {
            Type::Pointer(ty) => Some(ty.as_ref()),
            _ => None,
        }
    }

    pub fn ptr(self) -> Self {
        Type::Pointer(Box::new(self))
    }

    pub fn indirect_by(self, indirection: usize) -> Self {
        (0..indirection).fold(self, |ty, _| ty.ptr())
    }

    pub fn self_equatable(&self) -> bool {
        match self {
            Type::Nothing
            | Type::Interface(..)
            | Type::Class(..)
            | Type::Record(..)
            | Type::Function(..)
            | Type::GenericParam(..)
            | Type::MethodSelf => false,

            _ => true,
        }
    }

    pub fn equatable(&self, other: &Self) -> bool {
        match (self, other) {
            (Type::Nil, Type::Pointer(..) | Type::Primitive(Primitive::Pointer)) => true,
            (Type::Pointer(..) | Type::Primitive(Primitive::Pointer), Type::Nil) => true,
            (a, b) if a == b => a.self_equatable(),
            _ => false,
        }
    }

    pub fn self_orderable(&self) -> bool {
        match self {
            Type::Primitive(primitive) if primitive.is_numeric() => true,

            _ => false,
        }
    }

    pub fn valid_math_op(&self, op: Operator, rhs: &Self) -> bool {
        match (self, op, rhs) {
            // pointer arithmetic:
            // - lhs is any pointer
            // - operator is +, - or any bitwise operator
            // - rhs is any integer type lte than the system pointer size
            (
                Type::Pointer(_) | Type::Primitive(Primitive::Pointer),
                Operator::Add
                | Operator::Sub
                | Operator::BitAnd
                | Operator::BitOr
                | Operator::Caret,
                Type::Primitive(rhs_primitive),
            ) => {
                let rhs_valid = rhs_primitive.is_integer() || rhs_primitive.is_pointer();
                rhs_valid && rhs_primitive.native_size() <= Primitive::Pointer.native_size()
            },

            // (typed) pointer arithmetic:
            // - lhs is any pointer
            // - rhs is +, - or any bitwise operator
            // - rhs is another pointer of the same type
            (
                Type::Pointer(_),
                Operator::Add
                | Operator::Sub
                | Operator::BitAnd
                | Operator::BitOr
                | Operator::Caret,
                Type::Pointer(..),
            ) => *self == *rhs,

            // integer division is valid for two of the same primitive integer type
            (Type::Primitive(a), Operator::IDiv, Type::Primitive(b)) => {
                let a_valid = a.is_integer() || a.is_pointer();
                a_valid && *a == *b
            },

            // real division is valid for two of the same primitive real type
            (Type::Primitive(a), Operator::FDiv, Type::Primitive(b)) => a.is_real() && *a == *b,

            // all maths ops except division are valid for primitives of the same type
            (
                Type::Primitive(a),
                Operator::Add | Operator::Sub | Operator::Mul | Operator::Mod,
                Type::Primitive(b),
            ) => a.is_numeric() && *a == *b,

            // bitwise ops are valid for two identical unsigned primitive types
            (
                Type::Primitive(lhs),
                Operator::Shl
                | Operator::Shr
                | Operator::BitAnd
                | Operator::BitOr
                | Operator::Caret,
                Type::Primitive(rhs),
            ) if *lhs == *rhs => lhs.is_integer() && !lhs.is_signed(),

            _ => false,
        }
    }

    pub fn get_method(&self, method: &Ident, ctx: &Context) -> NameResult<Option<FunctionDecl>> {
        match self {
            Type::Interface(iface) => {
                let iface_def = ctx.find_iface_def(iface)?;
                let method_decl = iface_def
                    .methods
                    .iter()
                    .find(|m| *m.ident() == *method)
                    .map(|m| m.decl.clone());

                Ok(method_decl)
            },

            _ => Ok(None),
        }
    }

    pub fn as_something(&self) -> Option<&Self> {
        match self {
            Type::Nothing => None,
            ty => Some(ty),
        }
    }

    pub fn expect_something(self, msg: &str) -> Self {
        match self {
            Type::Nothing => panic!("expected a type: {}", msg),
            x => x,
        }
    }

    pub fn element_ty(&self) -> Option<&Type> {
        match self {
            Type::Array(array_ty) => Some(&array_ty.element_ty),
            Type::DynArray { element } => Some(element.as_ref()),
            Type::Pointer(deref_ty) => Some(deref_ty.as_ref()),
            _ => None,
        }
    }

    pub fn is_matchable(&self) -> bool {
        match self {
            Type::Nothing | Type::Nil => false,
            _ => true,
        }
    }

    pub fn as_primitive(&self) -> Option<Primitive> {
        match self {
            Type::Primitive(p) => Some(*p),
            _ => None,
        }
    }

    pub fn as_iface(&self) -> Result<&IdentPath, &Self> {
        match self {
            Type::Interface(iface) => Ok(iface),
            other => Err(other),
        }
    }

    pub fn as_record(&self) -> Result<&Symbol, &Self> {
        match self {
            Type::Record(class) => Ok(&*class),
            other => Err(other),
        }
    }

    pub fn as_class(&self) -> Result<&Symbol, &Self> {
        match self {
            Type::Class(class) => Ok(&*class),
            other => Err(other),
        }
    }

    pub fn as_variant(&self) -> Result<&Symbol, &Self> {
        match self {
            Type::Variant(name) => Ok(&*name),
            other => Err(other),
        }
    }

    pub fn as_func(&self) -> Result<&Rc<FunctionSig>, &Self> {
        match self {
            Type::Function(sig) => Ok(sig),
            other => Err(other),
        }
    }

    fn new_class_name(name: &Symbol, args: &impl TypeArgsResolver) -> Symbol {
        if name.is_unspecialized_generic() {
            panic!("can't substitute types args into unspecialized name")
        }

        let new_args = name.type_args.as_ref().and_then(|name_type_args| {
            let items = name_type_args
                .items
                .iter()
                .cloned()
                .map(|arg| arg.substitute_type_args(args));

            Some(TypeList::new(items, name_type_args.span().clone()))
        });

        Symbol {
            type_args: new_args,
            ..name.clone()
        }
    }

    pub fn match_constraint(&self, constraint_ty: &Type, ctx: &Context) -> bool {
        match constraint_ty {
            Type::Interface(constraint_iface_sym) => {
                ctx.is_iface_impl(self, constraint_iface_sym)
            }

            // "Any" used as a constraint means all types, nothing to validate
            Type::Any => true,

            // nothing else is a valid constraint
            _ => false,
        }
    }

    // todo: error handling
    pub fn substitute_type_args(self, args: &impl TypeArgsResolver) -> Self {
        if args.len() == 0 {
            return self;
        }

        match self {
            Type::GenericParam(param) => args.resolve(&param).into_owned(),

            Type::Class(name) if name.decl_name.type_params.is_some() => {
                Type::Class(Box::new(Self::new_class_name(&name, args)))
            },

            Type::Record(name) if name.decl_name.type_params.is_some() => {
                Type::Record(Box::new(Self::new_class_name(&name, args)))
            },

            Type::Variant(name) if name.decl_name.type_params.is_some() => {
                Type::Variant(Box::new(Self::new_class_name(&name, args)))
            },

            Type::DynArray { element } => Type::DynArray {
                element: element.substitute_type_args(args).into(),
            },

            Type::Array(array_ty) => {
                let array_ty = (*array_ty).clone();
                ArrayType {
                    element_ty: array_ty.element_ty.substitute_type_args(args).into(),
                    dim: array_ty.dim,
                }
                .into()
            },

            Type::Pointer(base_ty) => base_ty.substitute_type_args(args).ptr(),

            Type::Function(sig) => {
                let sig = sig.substitute_type_args(args);
                Type::Function(Rc::new(sig))
            },

            other => other,
        }
    }

    pub fn specialize_generic<'s, 'a, 'res>(
        &'s self,
        args: &'a impl TypeArgsResolver,
        ctx: &Context,
    ) -> GenericResult<Cow<'res, Self>>
    where
        's: 'res,
        'a: 'res,
    {
        let specialized = match self {
            Type::GenericParam(type_param) => args.resolve(type_param),

            Type::Record(sym) => {
                let sym = specialize_generic_name(&sym, args)?;
                let record_ty = Type::Record(Box::new(sym.into_owned()));

                Cow::Owned(record_ty)
            },

            Type::Class(sym) => {
                let sym = specialize_generic_name(&sym, args)?;
                let class_ty = Type::Class(Box::new(sym.into_owned()));

                Cow::Owned(class_ty)
            },

            Type::Variant(variant) => {
                let sym = specialize_generic_name(&variant, args)?;
                let variant_ty = Type::Variant(Box::new(sym.into_owned()));

                Cow::Owned(variant_ty)
            },

            Type::Array(array_ty) => {
                let element_ty = array_ty.element_ty.specialize_generic(args, ctx)?;
                let arr_ty = ArrayType {
                    element_ty: element_ty.into_owned(),
                    dim: array_ty.dim,
                };
                Cow::Owned(Type::from(arr_ty))
            },

            Type::DynArray { element } => {
                let element_ty = element.specialize_generic(args, ctx)?;

                Cow::Owned(Type::DynArray {
                    element: Box::new(element_ty.into_owned()),
                })
            },

            Type::Function(sig) => {
                let specialized_sig = sig.specialize_generic(args, ctx)?;
                Cow::Owned(Type::Function(Rc::new(specialized_sig)))
            },

            not_generic => Cow::Borrowed(not_generic),
        };

        Ok(specialized)
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Type::Nil => write!(f, "nil"),
            Type::Class(name) | Type::Record(name) | Type::Enum(name) => write!(f, "{}", name),
            Type::Interface(iface) => write!(f, "{}", iface),
            Type::Pointer(target_ty) => write!(f, "^{}", target_ty),
            Type::Array(array_ty) => write!(f, "{}", array_ty),
            Type::DynArray { element } => write!(f, "array of {}", element),
            Type::GenericParam(ident) => write!(f, "{}", ident),
            Type::Nothing => write!(f, "Nothing"),
            Type::Primitive(p) => write!(f, "{}", p.name()),
            Type::Variant(variant) => write!(f, "{}", variant),
            Type::MethodSelf => write!(f, "Self"),
            Type::Any => write!(f, "Any"),
            Type::Function(sig) => write!(f, "{}", sig),
        }
    }
}

impl TypeAnnotation for Type {
    fn is_known(&self) -> bool {
        true
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct ArrayType {
    pub element_ty: Type,
    pub dim: usize,
}

impl ArrayType {
    pub fn new(element_ty: Type, dim: usize) -> Self {
        Self { element_ty, dim }
    }
}

impl fmt::Display for ArrayType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "array[{}] of {}", self.dim, self.element_ty)
    }
}

#[derive(Debug, Copy, Clone)]
pub struct TypeMemberRef<'ty> {
    pub ident: &'ty Ident,
    pub ty: &'ty Type,
}

pub fn typecheck_type(ty: &ast::TypeName, ctx: &mut Context) -> TypecheckResult<Type> {
    match ty {
        ast::TypeName::Ident(IdentTypeName {
            ident,
            indirection,
            type_args,
            span,
        }) => {
            let (_, raw_ty) = ctx
                .find_type(ident)
                .map_err(|err| TypecheckError::NameError {
                    err,
                    span: ty.span().clone(),
                })?;

            let raw_ty = raw_ty.clone();

            let ty = match type_args {
                Some(type_args) => {
                    let mut checked_type_arg_items = Vec::new();
                    for arg in &type_args.items {
                        let arg_ty = typecheck_type(arg, ctx)?;
                        checked_type_arg_items.push(arg_ty);
                    }

                    let type_args_span = type_args.span().clone();
                    let checked_type_args = TypeList::new(checked_type_arg_items, type_args_span);

                    Type::specialize_generic(&raw_ty, &checked_type_args, ctx)
                        .map_err(|err| TypecheckError::from_generic_err(err, span.clone()))?
                        .into_owned()
                },

                None => raw_ty.clone(),
            };

            Ok(ty.indirect_by(*indirection))
        },

        ast::TypeName::Array(ArrayTypeName { element, dim, .. }) => {
            let element_ty = typecheck_type(element.as_ref(), ctx)?;

            match dim {
                Some(dim_expr) => {
                    let dim_expr =
                        typecheck_expr(dim_expr, &Type::Primitive(Primitive::Int32), ctx)?;
                    let dim_val = const_eval_integer(&dim_expr, ctx)?;

                    let dim = dim_val
                        .as_usize()
                        .ok_or_else(|| TypecheckError::TypeMismatch {
                            span: dim_expr.span().clone(),
                            actual: dim_expr.annotation().ty().into_owned(),
                            expected: Type::Primitive(Primitive::Int32),
                        })?;

                    Ok(ArrayType { element_ty, dim }.into())
                },

                None => Ok(Type::DynArray {
                    element: Box::new(element_ty),
                }),
            }
        },

        ast::TypeName::Function(func_ty_name) => {
            let return_ty = match &func_ty_name.return_ty {
                Some(return_ty) => typecheck_type(return_ty, ctx)?,
                None => Type::Nothing,
            };

            let mut params = Vec::new();
            for param in &func_ty_name.params {
                let param_ty = typecheck_type(&param.ty, ctx)?;

                params.push(FunctionParamSig {
                    ty: param_ty,
                    modifier: param.modifier.clone(),
                });
            }

            let sig = FunctionSig::new(return_ty, params, None);
            let mut ty = Type::Function(Rc::new(sig));
            for _ in 0..func_ty_name.indirection {
                ty = ty.ptr();
            }

            Ok(ty)
        },

        ast::TypeName::Unknown(_) => unreachable!("trying to resolve unknown type"),
    }
}

pub fn specialize_generic_name<'a>(
    name: &'a Symbol,
    args: &impl TypeArgsResolver,
) -> GenericResult<Cow<'a, Symbol>> {
    let type_params = match name.decl_name.type_params.as_ref() {
        None => return Ok(Cow::Borrowed(name)),
        Some(type_params) => type_params,
    };

    if args.len() != type_params.items.len() {
        return Err(GenericError::ArgsLenMismatch {
            target: GenericTarget::Name(name.qualified.clone()),
            expected: type_params.items.len(),
            actual: type_params.len(),
        });
    }

    let type_args = if let Some(existing_args) = &name.type_args {
        let specialized_args = existing_args
            .items
            .iter()
            .cloned()
            .map(|arg| arg.substitute_type_args(args));

        TypeList::new(specialized_args, existing_args.span().clone())
    } else {
        let mut resolved_args = Vec::with_capacity(type_params.len());
        for (i, param) in type_params.items.iter().enumerate() {
            let arg = args.resolve(&TypeParamType {
                name: param.clone(),
                is_iface: None, //TODO? can't have constraints on name decls
                pos: i,
            });
            resolved_args.push(arg.into_owned());
        }
        TypeList::new(resolved_args, name.span().clone())
    };

    let name = Symbol {
        type_args: Some(type_args),
        ..name.clone()
    };

    Ok(Cow::Owned(name))
}

pub fn specialize_struct_def(class: &StructDef, ty_args: &TypeList) -> GenericResult<StructDef> {
    let parameterized_name = specialize_generic_name(&class.name, ty_args)?;

    let members: Vec<_> = class
        .members
        .iter()
        .map(|member| {
            //            let ty = specialize_member(&member.ty, &args, span)?;
            let ty = member.ty.clone().substitute_type_args(ty_args);

            Ok(ast::StructMember {
                ty,
                ..member.clone()
            })
        })
        .collect::<GenericResult<_>>()?;

    Ok(StructDef {
        name: parameterized_name.into_owned(),
        members,
        span: class.span.clone(),
        kind: class.kind,
    })
}

pub fn specialize_generic_variant(
    variant: &VariantDef,
    args: &TypeList,
) -> GenericResult<VariantDef> {
    let parameterized_name = specialize_generic_name(&variant.name, args)?;

    let cases: Vec<_> = variant
        .cases
        .iter()
        .map(|case| {
            let data_ty = match &case.data_ty {
                None => None,
                Some(ty) => {
                    let ty = ty.clone().substitute_type_args(args);
                    Some(ty)
                },
            };

            Ok(ast::VariantCase {
                data_ty,
                ..case.clone()
            })
        })
        .collect::<GenericResult<_>>()?;

    Ok(VariantDef {
        name: parameterized_name.into_owned(),
        span: variant.span().clone(),
        cases,
    })
}

pub trait Specializable {
    type GenericID: PartialEq;

    fn is_unspecialized_generic(&self) -> bool;
    fn name(&self) -> Self::GenericID;

    fn is_specialization_of(&self, generic: &Self) -> bool {
        generic.is_unspecialized_generic()
            && !self.is_unspecialized_generic()
            && self.name() == generic.name()
    }

    fn infer_specialized_from_hint<'out, 'a: 'out, 'b: 'out>(
        &'a self,
        hint: &'b Self,
    ) -> Option<&'out Self> {
        if self.is_unspecialized_generic() {
            if hint.is_specialization_of(self) {
                Some(hint)
            } else {
                None
            }
        } else {
            Some(self)
        }
    }
}

impl Specializable for Type {
    type GenericID = IdentPath;

    fn is_unspecialized_generic(&self) -> bool {
        match self {
            Type::Class(sym) | Type::Record(sym) | Type::Variant(sym) => {
                sym.is_unspecialized_generic()
            },

            Type::Array(array_ty) => array_ty.element_ty.is_unspecialized_generic(),

            Type::DynArray { element } => element.is_unspecialized_generic(),

            Type::Function(sig) => {
                sig.return_ty.is_unspecialized_generic()
                    || sig.params.iter().any(|p| p.ty.is_unspecialized_generic())
            },

            _ => false,
        }
    }

    fn name(&self) -> IdentPath {
        self.full_path()
            .expect("only types with full paths can be specialized")
    }
}

pub fn string_type(ctx: &mut Context) -> TypecheckResult<Type> {
    let span = context::builtin_span();
    let ns = IdentPath::from(Ident::new("System", span.clone()));
    let str_class_name = ast::TypeName::Ident(IdentTypeName {
        ident: ns.child(Ident::new("String", span.clone())),
        indirection: 0,
        type_args: None,
        span,
    });

    typecheck_type(&str_class_name, ctx)
}
