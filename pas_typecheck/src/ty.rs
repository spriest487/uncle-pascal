pub use self::{pattern::*, primitive::*, sig::*, ty_param::*};
use crate::ast::{const_eval_integer, typecheck_expr};
use crate::TypeArgsResult::NotGeneric;
use crate::{
    ast::{Class, FunctionDecl, Member, Variant},
    context,
    result::*,
    Context, GenericError, GenericResult, GenericTarget, NamingResult, Symbol, TypeAnnotation,
};
use pas_common::span::*;
use pas_syn::{
    ast::{self, ClassKind, Typed},
    ident::*,
    Operator,
};
use std::{fmt, rc::Rc};
use pas_syn::ast::{ArrayTypeName, IdentTypeName};

#[cfg(test)]
mod test;

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
    Interface(IdentPath),
    Variant(Box<Symbol>),
    Array { element: Box<Type>, dim: usize },
    DynArray { element: Box<Type> },
    MethodSelf,
    GenericParam(Box<TypeParamType>),
    Any,
}

impl From<Primitive> for Type {
    fn from(primitive: Primitive) -> Self {
        Type::Primitive(primitive)
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
            Type::Interface(iface) => Some(iface.clone()),
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

            Type::Class(_) => true,
            Type::Interface(_) => true,
            Type::DynArray { .. } => true,
            Type::MethodSelf => true,
            Type::Any => true,
        }
    }

    pub fn of_decl(type_decl: &ast::TypeDecl<TypeAnnotation>) -> Self {
        match type_decl {
            ast::TypeDecl::Class(class) if class.kind == ClassKind::Record => {
                Type::Record(Box::new(class.name.clone()))
            }

            ast::TypeDecl::Class(class) => Type::Class(Box::new(class.name.clone())),

            ast::TypeDecl::Variant(variant) => Type::Variant(Box::new(variant.name.clone())),

            ast::TypeDecl::Interface(iface) => Type::Interface(iface.name.qualified.clone()),
        }
    }

    pub fn find_data_member(&self, member: &Ident, ctx: &Context) -> NamingResult<Option<Member>> {
        match self {
            Type::Class(class_name) | Type::Record(class_name) => {
                let def = ctx.instantiate_class(class_name)?;

                Ok(def.find_member(member).cloned())
            }

            _ => Ok(None),
        }
    }

    pub fn get_member(&self, index: usize, ctx: &Context) -> NamingResult<Option<Member>> {
        match self {
            Type::Record(class) | Type::Class(class) => {
                let class = ctx.instantiate_class(class)?;

                Ok(class.members.get(index).cloned())
            }

            _ => Ok(None),
        }
    }

    pub fn members_len(&self, ctx: &Context) -> NamingResult<usize> {
        match self {
            Type::Record(class) | Type::Class(class) => {
                let class = ctx.instantiate_class(class)?;
                Ok(class.members.len())
            }

            _ => Ok(0),
        }
    }

    pub fn members(&self, ctx: &Context) -> NamingResult<Vec<Member>> {
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
    pub fn contains_generic_params(&self) -> bool {
        if self.is_generic_param() {
            return true;
        }

        if let TypeArgsResult::Specialized(type_args) = &self.type_args() {
            return type_args.items.iter().any(|a| a.contains_generic_params());
        }

        if let Some(array_el) = self.array_element_ty() {
            return array_el.contains_generic_params();
        }

        false
    }

    pub fn same_array_dim(&self, other: &Self) -> bool {
        match (self, other) {
            (Type::Array { dim, .. }, Type::Array { dim: other_dim, .. }) => *dim == *other_dim,

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
            }

            _ => NotGeneric,
        }
    }

    pub fn type_params(&self) -> Option<&ast::TypeList<Ident>> {
        match self {
            Type::Variant(name) | Type::Class(name) | Type::Record(name) => {
                name.decl_name.type_params.as_ref()
            }

            _ => None,
        }
    }

    pub fn array_element_ty(&self) -> Option<&Type> {
        match self {
            Type::DynArray { element } | Type::Array { element, .. } => Some(element),

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

    pub fn implicit_conversion_from(
        &self,
        from: &Self,
        span: &Span,
        ctx: &Context,
    ) -> TypecheckResult<()> {
        if *self == *from {
            return Ok(());
        }

        enum Conversion {
            Blittable,
            UnsafeBlittable,
            Illegal,
        }

        let conversion = match self {
            Type::Primitive(primitive_ty) => {
                match primitive_ty {
                    Primitive::Pointer if *from == Type::Nil => Conversion::Blittable,

                    Primitive::Pointer if from.is_rc_reference() => {
                        Conversion::UnsafeBlittable
                    }
                    Primitive::Pointer if from.is_pointer() => Conversion::UnsafeBlittable,

                    _ => Conversion::Illegal,
                }
            }

            Type::Pointer(..) | Type::Class(..) | Type::Interface(..) | Type::DynArray { .. }
                if *from == Type::Primitive(Primitive::Pointer) =>
            {
                Conversion::UnsafeBlittable
            }

            Type::Pointer(_) if *self == *from || *from == Type::Nil => Conversion::Blittable,

            Type::Interface(iface) => match from {
                Type::Class(..) if ctx.is_iface_impl(from, &iface) => Conversion::Blittable,
                Type::Interface(from_iface) if iface == from_iface => Conversion::Blittable,
                _ => Conversion::Illegal,
            },

            Type::Any => match from {
                Type::DynArray { .. } | Type::Class(..) | Type::Interface(..) => {
                    Conversion::Blittable
                }
                _ => Conversion::Illegal,
            },

            _ => Conversion::Illegal,
        };

        match conversion {
            Conversion::Blittable => Ok(()),
            Conversion::UnsafeBlittable if ctx.allow_unsafe() => Ok(()),

            Conversion::UnsafeBlittable => Err(TypecheckError::UnsafeConversionNotAllowed {
                from: from.clone(),
                to: self.clone(),
                span: span.clone(),
            }),

            Conversion::Illegal => Err(TypecheckError::TypeMismatch {
                expected: self.clone(),
                actual: from.clone(),
                span: span.clone(),
            }),
        }
    }

    pub fn valid_math_op(&self, op: Operator, rhs: &Self) -> bool {
        match (self, op, rhs) {
            // pointer arithmetic
            (Type::Pointer(_), Operator::Plus, Type::Pointer(_))
            | (Type::Pointer(_), Operator::Minus, Type::Pointer(_)) => true,
            | (Type::Pointer(_), Operator::Plus, Type::Primitive(Primitive::NativeInt))
            | (Type::Pointer(_), Operator::Minus, Type::Primitive(Primitive::NativeInt))
            | (Type::Pointer(_), Operator::Plus, Type::Primitive(Primitive::Int8))
            | (Type::Pointer(_), Operator::Minus, Type::Primitive(Primitive::Int8))
            | (Type::Pointer(_), Operator::Plus, Type::Primitive(Primitive::Int16))
            | (Type::Pointer(_), Operator::Minus, Type::Primitive(Primitive::Int16))
            | (Type::Pointer(_), Operator::Plus, Type::Primitive(Primitive::Int32))
            | (Type::Pointer(_), Operator::Minus, Type::Primitive(Primitive::Int32)) => true,

            // all maths ops are valid for primitives of the same type
            (Type::Primitive(a), Operator::Plus, Type::Primitive(b))
            | (Type::Primitive(a), Operator::Minus, Type::Primitive(b))
            | (Type::Primitive(a), Operator::IntegerDivide, Type::Primitive(b))
            | (Type::Primitive(a), Operator::Multiply, Type::Primitive(b)) => {
                if a.is_numeric() && b.is_numeric() {
                    *a == *b
                } else {
                    false
                }
            },

            (_, Operator::Shl, _) | (_, Operator::Shr, _) if *self == *rhs => match self {
                Type::Primitive(p) if p.is_integer() && !p.is_signed() => true,
                _ => false,
            },

            _ => false,
        }
    }

    pub fn get_method(&self, method: &Ident, ctx: &Context) -> NamingResult<Option<FunctionDecl>> {
        match self {
            Type::Interface(iface) => {
                let iface_def = ctx.find_iface_def(iface)?;
                let method_decl = iface_def
                    .methods
                    .iter()
                    .find(|m| *m.ident.single() == *method)
                    .cloned();

                Ok(method_decl)
            }

            _ => Ok(None),
        }
    }

    pub fn expect_something(self, msg: &str) -> Self {
        match self {
            Type::Nothing => panic!("expected a type: {}", msg),
            x => x,
        }
    }

    pub fn collection_element_ty(&self) -> Option<&Type> {
        match self {
            Type::Array { element, .. } => Some(element.as_ref()),
            Type::DynArray { element } => Some(element.as_ref()),
            _ => None,
        }
    }

    pub fn is_matchable(&self) -> bool {
        match self {
            Type::Nothing | Type::Nil => false,
            _ => true,
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

    fn new_class_name(name: &Symbol, args: &TypeList) -> Symbol {
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

    // todo: error handling
    pub fn substitute_type_args(self, args: &TypeList) -> Self {
        if args.len() == 0 {
            return self;
        }

        match self {
            Type::GenericParam(param) => args.items[param.pos].clone(),

            Type::Class(name) if name.decl_name.type_params.is_some() => {
                Type::Class(Box::new(Self::new_class_name(&name, args)))
            }

            Type::Record(name) if name.decl_name.type_params.is_some() => {
                Type::Record(Box::new(Self::new_class_name(&name, args)))
            }

            Type::Variant(name) if name.decl_name.type_params.is_some() => {
                Type::Variant(Box::new(Self::new_class_name(&name, args)))
            }

            Type::DynArray { element } => Type::DynArray {
                element: element.substitute_type_args(args).into(),
            },

            Type::Array { element, dim } => Type::Array {
                element: element.substitute_type_args(args).into(),
                dim,
            },

            Type::Pointer(base_ty) => base_ty.substitute_type_args(args).ptr(),

            other => other,
        }
    }

    pub fn specialize_generic(&self, args: &TypeList, span: &Span) -> GenericResult<Self> {
        match self {
            Type::GenericParam(type_param) => {
                let arg = args.items.get(type_param.pos).unwrap_or_else(|| {
                    panic!(
                        "missing arg {} in type arg list {:?} for type {}",
                        type_param, args, self
                    )
                });

                Ok(arg.clone())
            }

            Type::Record(class) => specialize_generic_name(&class, args, span)
                .map(Box::new)
                .map(Type::Record),

            Type::Class(class) => specialize_generic_name(&class, args, span)
                .map(Box::new)
                .map(Type::Class),

            Type::Variant(variant) => specialize_generic_name(&variant, args, span)
                .map(Box::new)
                .map(Type::Variant),

            Type::Array { element, dim } => element
                .specialize_generic(args, span)
                .map(Box::new)
                .map(|element| Type::Array { element, dim: *dim }),

            Type::DynArray { element } => element
                .specialize_generic(args, span)
                .map(Box::new)
                .map(|element| Type::DynArray { element }),

            not_generic => Ok(not_generic.clone()),
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Type::Nil => write!(f, "nil"),
            Type::Class(name) | Type::Record(name) => write!(f, "{}", name),
            Type::Interface(iface) => write!(f, "{}", iface),
            Type::Pointer(target_ty) => write!(f, "^{}", target_ty),
            Type::Array { element, dim } => write!(f, "array[{}] of {}", dim, element),
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

impl Typed for Type {
    fn is_known(&self) -> bool {
        true
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
            let (_, raw_ty) = ctx.find_type(ident)?;
            let raw_ty = raw_ty.clone();

            let ty = match type_args {
                Some(type_args) => {
                    let mut checked_type_arg_items = Vec::new();
                    for arg in &type_args.items {
                        let arg_ty = typecheck_type(arg, ctx)?;
                        checked_type_arg_items.push(arg_ty);
                    }

                    let checked_type_args =
                        TypeList::new(checked_type_arg_items, type_args.span().clone());

                    Type::specialize_generic(&raw_ty, &checked_type_args, span)?
                }

                None => raw_ty.clone(),
            };

            Ok(ty.indirect_by(*indirection))
        }

        ast::TypeName::Array(ArrayTypeName { element, dim, .. }) => {
            let element = typecheck_type(element.as_ref(), ctx)?;

            match dim {
                Some(dim_expr) => {
                    let dim_expr =
                        typecheck_expr(dim_expr, &Type::Primitive(Primitive::Int32), ctx)?;
                    let dim_val = const_eval_integer(&dim_expr, ctx)?;

                    let dim = dim_val.as_usize().ok_or_else(|| TypecheckError::TypeMismatch {
                        span: dim_expr.span().clone(),
                        actual: dim_expr.annotation().ty().clone(),
                        expected: Type::Primitive(Primitive::Int32),
                    })?;

                    Ok(Type::Array {
                        element: Box::new(element),
                        dim,
                    })
                }

                None => Ok(Type::DynArray {
                    element: Box::new(element),
                }),
            }
        }

        ast::TypeName::Unknown(_) => unreachable!("trying to resolve unknown type"),
    }
}

pub fn specialize_generic_name(
    name: &Symbol,
    args: &TypeList,
    span: &Span,
) -> GenericResult<Symbol> {
    if !name.is_unspecialized_generic() {
        return Ok(name.clone());
    }

    let type_params = match name.decl_name.type_params.as_ref() {
        None => unreachable!("is_unspecialized_generic should have returned false"),

        Some(type_params) => type_params,
    };

    if args.len() != type_params.items.len() {
        return Err(GenericError::ArgsLenMismatch {
            target: GenericTarget::Name(name.qualified.clone()),
            expected: type_params.items.len(),
            actual: args.len(),
            span: span.clone(),
        });
    }

    let name = Symbol {
        type_args: Some(args.clone()),
        ..name.clone()
    };

    Ok(name)
}

pub fn specialize_class_def(
    class: &Class,
    ty_args: &TypeList,
    span: &Span,
) -> GenericResult<Class> {
    let parameterized_name = specialize_generic_name(&class.name, &ty_args, span)?;

    let members: Vec<_> = class
        .members
        .iter()
        .map(|member| {
            //            let ty = specialize_member(&member.ty, &args, span)?;
            let ty = member.ty.clone().substitute_type_args(&ty_args);

            Ok(ast::Member {
                ty,
                ..member.clone()
            })
        })
        .collect::<GenericResult<_>>()?;

    Ok(Class {
        name: parameterized_name,
        members,
        span: class.span.clone(),
        kind: class.kind,
    })
}

pub fn specialize_generic_variant(
    variant: &Variant,
    args: &TypeList,
    span: &Span,
) -> GenericResult<Variant> {
    let parameterized_name = specialize_generic_name(&variant.name, &args, span)?;

    let cases: Vec<_> = variant
        .cases
        .iter()
        .map(|case| {
            let data_ty = match &case.data_ty {
                None => None,
                Some(ty) => {
                    let ty = ty.clone().substitute_type_args(&args);
                    Some(ty)
                }
            };

            Ok(ast::VariantCase {
                data_ty,
                ..case.clone()
            })
        })
        .collect::<GenericResult<_>>()?;

    Ok(Variant {
        name: parameterized_name,
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
            Type::Class(class) | Type::Record(class) => class.is_unspecialized_generic(),

            Type::Variant(variant) => variant.is_unspecialized_generic(),

            Type::Array { element, .. } => element.is_unspecialized_generic(),

            Type::DynArray { element } => element.is_unspecialized_generic(),

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
