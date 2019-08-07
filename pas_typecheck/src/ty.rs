use std::{fmt, rc::Rc};

use pas_common::span::*;
use pas_syn::{
    ast::{self, ClassKind, FunctionParamMod, Typed},
    ident::*,
    Operator,
};

use crate::{
    ast::{Class, FunctionDecl, Interface, Variant},
    context::{
        self,
        ns::{self, Namespace as _},
    },
    result::*,
    Context, Decl, NameError, QualifiedDeclName, TypeAnnotation,
};

#[cfg(test)]
mod test;

#[derive(Eq, PartialEq, Hash, Clone, Debug)]
pub struct FunctionParamSig {
    pub modifier: Option<FunctionParamMod>,
    pub ty: Type,
}

impl FunctionParamSig {
    pub fn by_val(ty: Type) -> Self {
        Self { ty, modifier: None }
    }

    pub fn inout(ty: Type) -> Self {
        Self {
            ty,
            modifier: Some(FunctionParamMod::Var),
        }
    }

    pub fn out(ty: Type) -> Self {
        Self {
            ty,
            modifier: Some(FunctionParamMod::Out),
        }
    }

    pub fn is_by_ref(&self) -> bool {
        match &self.modifier {
            Some(FunctionParamMod::Out) | Some(FunctionParamMod::Var) => true,
            _ => false,
        }
    }
}

#[derive(Eq, PartialEq, Hash, Clone, Debug)]
pub struct FunctionSig {
    pub return_ty: Type,
    pub params: Vec<FunctionParamSig>,
    pub type_params_len: usize,
}

impl FunctionSig {
    pub fn of_decl(decl: &ast::FunctionDecl<TypeAnnotation>) -> Self {
        Self {
            params: decl
                .params
                .iter()
                .map(|p| FunctionParamSig {
                    ty: p.ty.clone(),
                    modifier: p.modifier.clone(),
                })
                .collect(),
            return_ty: decl.return_ty.clone().unwrap_or(Type::Nothing),
            type_params_len: decl.type_params.len(),
        }
    }

    pub fn specialize_generic(&self, type_args: Vec<Type>, span: &Span) -> TypecheckResult<Self> {
        if type_args.len() != self.type_params_len {
            return Err(TypecheckError::InvalidTypeArgs {
                expected: self.type_params_len,
                actual: type_args.len(),
                ty: Type::Function(Rc::new(self.clone())),
                span: span.clone(),
            });
        }

        let specialized_params = self
            .params
            .iter()
            .map(|sig_param| {
                let ty = Type::specialize_generic(&sig_param.ty, &type_args, span)?;
                Ok(FunctionParamSig {
                    ty,
                    ..sig_param.clone()
                })
            })
            .collect::<TypecheckResult<_>>()?;

        let specialized_sig = FunctionSig {
            return_ty: Type::specialize_generic(&self.return_ty, &type_args, span)?,
            params: specialized_params,
            type_params_len: self.type_params_len,
        };

        Ok(specialized_sig)
    }

    /// replace all `Self`-typed args with `self_ty`
    pub fn with_self(&self, self_ty: &Type) -> Self {
        let mut result = self.clone();
        for param in &mut result.params {
            if param.ty == Type::MethodSelf {
                param.ty = self_ty.clone();
            }
        }

        result
    }

    /// given that `self` is the sig of an interface method with one
    /// or more `Self`-typed arguments, find the expected self-type for
    pub fn impl_ty_from_args<'arg>(&self, args: &'arg [Type]) -> Option<&'arg Type> {
        if args.len() != self.params.len() {
            return None;
        }

        let self_arg_pos = self
            .params
            .iter()
            .position(|arg| arg.ty == Type::MethodSelf)?;

        Some(&args[self_arg_pos])
    }

    /// given that `self` is the sig of an interface method,
    /// for what type does `impl_func` implement this method, if any?
    pub fn impl_ty<'func>(&self, impl_func: &'func Self) -> Option<&'func Type> {
        if self.params.len() != impl_func.params.len() {
            return None;
        }

        let self_type = if self.return_ty == Type::MethodSelf {
            &impl_func.return_ty
        } else {
            self.params
                .iter()
                .position(|param| param.ty == Type::MethodSelf)
                .map(|pos| &impl_func.params[pos].ty)?
        };

        // `Nothing` can't have interface impls
        if *self_type == Type::Nothing {
            return None;
        }

        let self_positions: Vec<_> = self
            .params
            .iter()
            .enumerate()
            .filter_map(|(pos, param)| {
                if param.ty == Type::MethodSelf {
                    Some(pos)
                } else {
                    None
                }
            })
            .collect();

        for pos in 0..self.params.len() {
            if self_positions.contains(&pos) {
                // self-typed params must all be the same type as either the
                // first such param, or the return type if it's self-typed too
                if impl_func.params[pos].ty != *self_type {
                    return None;
                }
            } else if impl_func.params[pos] != self.params[pos] {
                // non-self params must match exactly
                return None;
            }
        }

        Some(self_type)
    }
}

impl fmt::Display for FunctionSig {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "function(")?;

        for (i, param) in self.params.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            if let Some(modifier) = &param.modifier {
                write!(f, "{} ", modifier)?;
            }

            write!(f, "{}", param.ty)?;
        }
        write!(f, ")")?;

        match &self.return_ty {
            Type::Nothing => Ok(()),
            ty => write!(f, ": {}", ty),
        }
    }
}

#[derive(Eq, PartialEq, Hash, Copy, Clone, Debug)]
pub enum Primitive {
    Boolean,
    Byte,
    Int32,
    Real32,
}

impl Primitive {
    pub fn name(&self) -> &str {
        match self {
            Primitive::Boolean => "Boolean",
            Primitive::Byte => "Byte",
            Primitive::Int32 => "Integer",
            Primitive::Real32 => "Real32",
        }
    }
}

#[derive(Eq, PartialEq, Hash, Clone, Debug)]
pub struct TypeParam {
    pub name: Ident,
    pub pos: usize,
}

impl fmt::Display for TypeParam {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

#[derive(Eq, PartialEq, Hash, Clone, Debug)]
pub enum Type {
    Nothing,
    Nil,
    Primitive(Primitive),
    Pointer(Box<Type>),
    Function(Rc<FunctionSig>),
    Record(Rc<Class>),
    Class(Rc<Class>),
    Interface(Rc<Interface>),
    Variant(Rc<Variant>),
    Array { element: Box<Type>, dim: usize },
    MethodSelf,
    GenericParam(TypeParam),
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
            Type::Interface(iface) => Some(iface.name.qualified.clone()),
            Type::Record(class) | Type::Class(class) => Some(class.name.qualified.clone()),
            Type::Variant(variant) => Some(variant.name.qualified.clone()),
            _ => None,
        }
    }

    pub fn of_decl(type_decl: &ast::TypeDecl<TypeAnnotation>) -> Self {
        match type_decl {
            ast::TypeDecl::Class(
                class @ ast::Class {
                    kind: ClassKind::Record,
                    ..
                },
            ) => Type::Record(Rc::new(class.clone())),

            ast::TypeDecl::Class(
                class @ ast::Class {
                    kind: ClassKind::Object,
                    ..
                },
            ) => Type::Class(Rc::new(class.clone())),

            ast::TypeDecl::Variant(variant) => Type::Variant(Rc::new(variant.clone())),

            ast::TypeDecl::Interface(iface) => Type::Interface(Rc::new(iface.clone())),
        }
    }

    pub fn find_member(&self, ident: &Ident) -> Option<MemberRef> {
        match self {
            Type::Record(class) | Type::Class(class) => {
                class.find_member(ident).map(|m| MemberRef {
                    ident: &m.ident,
                    ty: &m.ty,
                })
            }

            _ => None,
        }
    }

    pub fn get_member(&self, index: usize) -> Option<MemberRef> {
        match self {
            Type::Record(class) | Type::Class(class) => {
                class.members.get(index).map(|m| MemberRef {
                    ty: &m.ty,
                    ident: &m.ident,
                })
            }
            _ => None,
        }
    }

    pub fn members_len(&self) -> usize {
        match self {
            Type::Record(class) | Type::Class(class) => class.members.len(),
            _ => 0,
        }
    }

    pub fn members(&self) -> impl Iterator<Item = MemberRef> {
        (0..self.members_len()).map(move |m| self.get_member(m).unwrap())
    }

    pub fn is_rc(&self) -> bool {
        match self {
            Type::Class(..) => true,
            Type::Interface(..) => true,
            Type::Any => true,

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

    pub fn self_comparable(&self) -> bool {
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

    pub fn self_orderable(&self) -> bool {
        match self {
            Type::Primitive(Primitive::Real32) => true,
            Type::Primitive(Primitive::Int32) => true,
            Type::Primitive(Primitive::Byte) => true,
            _ => false,
        }
    }

    pub fn assignable_from(&self, from: &Self, ctx: &Context) -> bool {
        match self {
            Type::Pointer(_) => *self == *from || *from == Type::Nil,
            Type::Function(_) => false,
            Type::Interface(iface) => match from {
                Type::Class(..) => ctx.is_iface_impl(from, &iface.name.qualified),
                Type::Interface(from_iface) => iface.name == from_iface.name,
                _ => false,
            },
            Type::Any => match from {
                Type::Class(..) | Type::Interface(..) => true,
                _ => false,
            },
            _ => *self == *from,
        }
    }

    pub fn valid_math_op(&self, op: Operator, rhs: &Self) -> bool {
        match (self, op, rhs) {
            // pointer arithmetic
            (Type::Pointer(_), Operator::Plus, Type::Pointer(_))
            | (Type::Pointer(_), Operator::Minus, Type::Pointer(_))
            | (Type::Pointer(_), Operator::Plus, Type::Primitive(Primitive::Int32))
            | (Type::Pointer(_), Operator::Minus, Type::Primitive(Primitive::Int32)) => true,

            // all maths ops are valid for primitives of the same type
            (Type::Primitive(a), Operator::Plus, Type::Primitive(b))
            | (Type::Primitive(a), Operator::Minus, Type::Primitive(b))
            | (Type::Primitive(a), Operator::Divide, Type::Primitive(b))
            | (Type::Primitive(a), Operator::Multiply, Type::Primitive(b)) => *a == *b,

            _ => false,
        }
    }

    pub fn get_method(&self, method: &Ident) -> Option<&FunctionDecl> {
        match self {
            Type::Interface(iface) => iface.methods.iter().find(|m| *m.ident.single() == *method),
            _ => None,
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
            _ => None,
        }
    }

    pub fn is_matchable(&self) -> bool {
        match self {
            Type::Class(..) | Type::Interface(..) | Type::Any => true,
            _ => false,
        }
    }

    pub fn as_iface(&self) -> Result<Rc<Interface>, &Self> {
        match self {
            Type::Interface(iface) => Ok(iface.clone()),
            other => Err(other),
        }
    }

    pub fn as_record(&self) -> Result<Rc<Class>, &Self> {
        match self {
            Type::Record(class) => Ok(class.clone()),
            other => Err(other),
        }
    }

    pub fn as_class(&self) -> Result<Rc<Class>, &Self> {
        match self {
            Type::Class(class) => Ok(class.clone()),
            other => Err(other),
        }
    }

    pub fn specialize_generic(ty: &Self, args: &[Self], span: &Span) -> TypecheckResult<Self> {
        match ty {
            Type::GenericParam(TypeParam { pos, .. }) => {
                let arg = args.get(*pos).unwrap_or_else(|| {
                    panic!(
                        "missing arg {} in type arg list {:?} for type {}",
                        pos, args, ty
                    )
                });

                Ok(arg.clone())
            }

            Type::Record(class) => specialize_generic_class(&class, args.to_vec(), span)
                .map(Rc::new)
                .map(Type::Record),

            Type::Class(class) => specialize_generic_class(&class, args.to_vec(), span)
                .map(Rc::new)
                .map(Type::Class),

            Type::Variant(variant) => specialize_generic_variant(&variant, args.to_vec(), span)
                .map(Rc::new)
                .map(Type::Variant),

            Type::Array { element, dim } => Self::specialize_generic(element, args, span)
                .map(Box::new)
                .map(|element| Type::Array { element, dim: *dim }),

            not_generic => Ok(not_generic.clone()),
        }
    }

    pub fn type_args(&self) -> &[Self] {
        match self {
            Type::Variant(variant) => &variant.name.type_args,
            Type::Record(class) | Type::Class(class) => &class.name.type_args,
            Type::Interface(iface) => &iface.name.type_args,
            _ => &[],
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Type::Nil => write!(f, "nil"),
            Type::Class(class) | Type::Record(class) => write!(f, "{}", class.name),
            Type::Interface(iface) => write!(f, "{}", iface.name),
            Type::Pointer(target_ty) => write!(f, "^{}", target_ty),
            Type::Array { element, dim } => write!(f, "array[{}] of {}", dim, element),
            Type::GenericParam(ident) => write!(f, "{}", ident),
            Type::Nothing => write!(f, "Nothing"),
            Type::Primitive(p) => write!(f, "{}", p.name()),
            Type::Variant(variant) => write!(f, "{}", variant.name),
            Type::MethodSelf => write!(f, "Self"),
            Type::Any => write!(f, "Any"),
            Type::Function(_) => unimplemented!("function types cannot be represented"),
        }
    }
}

impl Typed for Type {
    fn is_known(&self) -> bool {
        true
    }
}

#[derive(Debug, Copy, Clone)]
pub struct MemberRef<'ty> {
    pub ident: &'ty Ident,
    pub ty: &'ty Type,
}

fn specialize_generic_type_args(
    type_args: &[Type],
    outer_args: &[Type],
    span: &Span,
) -> TypecheckResult<Vec<Type>> {
    type_args
        .iter()
        .map(|arg| Type::specialize_generic(arg, outer_args, span))
        .collect()
}

pub fn find_type<'c>(name: &IdentPath, ctx: &'c Context) -> context::NamingResult<&'c Type> {
    match ctx.resolve(name) {
        Some(context::MemberRef::Value {
            value: Decl::Type { ty, .. },
            ..
        }) => Ok(ty),

        Some(context::MemberRef::Value {
            value: unexpected, ..
        }) => Err(NameError::ExpectedType(
            name.clone(),
            unexpected.clone().into(),
        )),

        Some(context::MemberRef::Namespace { path }) => {
            let ns_ident = path.top().key().unwrap().clone();
            let unexpected = context::UnexpectedValue::Namespace(ns_ident);
            Err(NameError::ExpectedType(name.clone(), unexpected))
        }

        None => Err(NameError::NotFound(name.last().clone())),
    }
}

pub fn typecheck_type(ty: &ast::TypeName, ctx: &Context) -> TypecheckResult<Type> {
    match ty {
        ast::TypeName::Ident {
            ident,
            indirection,
            type_args,
            span,
        } => {
            let raw_ty = find_type(ident, ctx)?;

            let ty = if !type_args.is_empty() {
                let type_args: Vec<_> = type_args
                    .iter()
                    .map(|arg| typecheck_type(arg, ctx))
                    .collect::<TypecheckResult<_>>()?;

                Type::specialize_generic(raw_ty, &type_args, span)?
            } else {
                raw_ty.clone()
            };

            Ok(ty.indirect_by(*indirection))
        }

        ast::TypeName::Array { element, dim, .. } => {
            let element = typecheck_type(element.as_ref(), ctx)?;

            Ok(Type::Array {
                element: Box::new(element),
                dim: *dim,
            })
        }

        ast::TypeName::Unknown(_) => unreachable!("trying to resolve unknown type"),
    }
}

fn specialize_member(
    member_ty: &Type,
    args: &[Type],
    span: &Span
) -> TypecheckResult<Type> {
    match member_ty {
        Type::GenericParam(class_param) => {
            let ty = args.get(class_param.pos).unwrap_or_else(|| {
                panic!(
                    "missing type arg {} in arg list {:?} for {}",
                    class_param.pos, args, member_ty,
                )
            });

            Ok(ty.clone())
        }

        Type::Class(class) => {
            let class_args = specialize_generic_type_args(&class.name.type_args, &args, span)?;
            let class = specialize_generic_class(&class, class_args, span)?;
            Ok(Type::Class(Rc::new(class)))
        },

        Type::Record(class) => {
            let class_args = specialize_generic_type_args(&class.name.type_args, &args, span)?;
            let class = specialize_generic_class(&class, class_args, span)?;
            Ok(Type::Record(Rc::new(class)))
        },

        Type::Variant(variant) => {
            let variant_args = specialize_generic_type_args(&variant.name.type_args, &args, span)?;
            let variant = specialize_generic_variant(variant, variant_args, span)?;
            Ok(Type::Variant(Rc::new(variant)))
        }

        Type::Array { element, dim } => {
            let ty = specialize_member(element, args, span)?;
            Ok(Type::Array { element: Box::new(ty), dim: *dim })
        }

        _ => Ok(member_ty.clone()),
    }
}

pub fn specialize_generic_class(
    class: &Class,
    args: Vec<Type>,
    span: &Span,
) -> TypecheckResult<Class> {
    let type_params: &[Ident] = &class.name.decl_name.type_params.as_slice();
    if args.len() != type_params.len() {
        return Err(TypecheckError::InvalidTypeArgs {
            ty: Type::Class(class.clone().into()),
            expected: type_params.len(),
            actual: args.len(),
            span: span.clone(),
        });
    }

    let members: Vec<_> = class
        .members
        .iter()
        .map(|member| {
            let ty = specialize_member(&member.ty, &args, span)?;
            Ok(ast::Member {
                ty,
                ..member.clone()
            })
        })
        .collect::<TypecheckResult<_>>()?;

    let parameterized_name = QualifiedDeclName {
        type_args: args,
        ..class.name.clone()
    };

    Ok(Class {
        name: parameterized_name,
        members,
        span: class.span.clone(),
        kind: class.kind,
    })
}

pub fn specialize_generic_variant(
    variant: &Variant,
    args: Vec<Type>,
    span: &Span,
) -> TypecheckResult<Variant> {
    let type_params: &[Ident] = &variant.name.decl_name.type_params.as_slice();
    if args.len() != type_params.len() {
        return Err(TypecheckError::InvalidTypeArgs {
            ty: Type::Variant(variant.clone().into()),
            expected: type_params.len(),
            actual: args.len(),
            span: span.clone(),
        });
    }

    let cases: Vec<_> = variant
        .cases
        .iter()
        .map(|case| {
            let data_ty = match &case.data_ty {
                None => None,
                Some(ty) => {
                    let ty = specialize_member(ty, &args, span)?;
                    Some(ty)
                },
            };

            Ok(ast::VariantCase {
                data_ty,
                ..case.clone()
            })
        })
        .collect::<TypecheckResult<_>>()?;

    let parameterized_name = QualifiedDeclName {
        type_args: args,
        ..variant.name.clone()
    };

    Ok(Variant {
        name: parameterized_name,
        span: variant.span().clone(),
        cases,
    })
}

pub trait Specializable {
    type GenericID: PartialEq;

    fn is_generic(&self) -> bool;
    fn name(&self) -> Self::GenericID;

    fn is_specialization_of(&self, generic: &Self) -> bool {
        generic.is_generic() && !self.is_generic() && self.name() == generic.name()
    }

    fn infer_specialized_from_hint<'out, 'a: 'out, 'b: 'out>(
        &'a self,
        hint: &'b Self,
    ) -> Option<&'out Self> {
        if self.is_generic() {
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

    fn is_generic(&self) -> bool {
        match self {
            Type::Class(class) | Type::Record(class) => class.name.is_generic(),

            Type::Variant(variant) => variant.name.is_generic(),

            _ => false,
        }
    }

    fn name(&self) -> IdentPath {
        self.full_path()
            .expect("only types with full paths can be specialized")
    }
}

pub fn string_type(ctx: &Context) -> TypecheckResult<Type> {
    let span = context::builtin_span();
    let ns = IdentPath::from(Ident::new("System", span.clone()));
    let str_class_name = ast::TypeName::Ident {
        ident: ns.child(Ident::new("String", span.clone())),
        indirection: 0,
        type_args: Vec::new(),
        span,
    };

    typecheck_type(&str_class_name, ctx)
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum TypePattern {
    VariantCase {
        variant: Rc<Variant>,
        case_index: usize,
        data_binding: Option<Ident>,
        span: Span,
    },
    NegatedVariantCase {
        variant: Rc<Variant>,
        case_index: usize,
        span: Span,
    },
    Type {
        ty: Type,
        binding: Option<Ident>,
        span: Span,
    },
    NegatedType {
        ty: Type,
        span: Span,
    },
}

impl Spanned for TypePattern {
    fn span(&self) -> &Span {
        match self {
            TypePattern::VariantCase { span, .. } => span,
            TypePattern::NegatedVariantCase { span, .. } => span,
            TypePattern::Type { span, .. } => span,
            TypePattern::NegatedType { span, .. } => span,
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct PatternBinding<'pat> {
    pub ident: &'pat Ident,
    pub ty: &'pat Type,
}

impl TypePattern {
    fn find_variant_case(
        path: &IdentPath,
        ctx: &Context,
    ) -> TypecheckResult<Option<(Rc<Variant>, usize)>> {
        let variant_parts = path.iter().cloned().take(path.as_slice().len() - 1);
        let stem_name = IdentPath::from_parts(variant_parts);

        match ctx.resolve(&stem_name) {
            Some(ns::MemberRef::Value {
                value:
                    Decl::Type {
                        ty: Type::Variant(variant),
                        ..
                    },
                ..
            }) => {
                let case_ident = path.last();

                let case_index = variant.case_position(case_ident).ok_or_else(|| {
                    let err_span = path.first().span.to(&case_ident.span);

                    TypecheckError::from(NameError::MemberNotFound {
                        base: Type::Variant(variant.clone()),
                        member: case_ident.clone(),
                        span: err_span,
                    })
                })?;

                let case = (variant.clone(), case_index);
                Ok(Some(case))
            }

            _ => Ok(None),
        }
    }

    // patterns referring to a typename can use a generic typename as the pattern, in which
    // case we infer the specialized type (or throw NotMatchable for generics we can't infer a
    // proper specialization for)
    fn find_pattern_ty(
        name: &IdentPath,
        span: &Span,
        expect_ty: &Type,
        ctx: &Context,
    ) -> TypecheckResult<Type> {
        let raw_ty = find_type(name, ctx)?;

        let matchable_ty = if raw_ty.is_matchable() {
            raw_ty.infer_specialized_from_hint(expect_ty).cloned()
        } else {
            None
        };

        matchable_ty.ok_or_else(|| TypecheckError::NotMatchable {
            ty: raw_ty.clone(),
            span: span.clone(),
        })
    }

    // find_pattern_ty, but for variant type patterns
    fn find_pattern_variant(
        variant: Rc<Variant>,
        span: &Span,
        expect_ty: &Type,
    ) -> TypecheckResult<Rc<Variant>> {
        match expect_ty {
            expect_var @ Type::Variant(..) => {
                let var_ty = Type::Variant(variant);

                var_ty.infer_specialized_from_hint(expect_var)
                    .map(|ty| match ty {
                        Type::Variant(v) => v.clone(),
                        _ => unreachable!("should never infer a non-variant specialized type for a generic variant"),
                    })
                    .ok_or_else(|| TypecheckError::NotMatchable {
                        ty: var_ty.clone(),
                        span: span.clone(),
                    })
            }

            _ => {
                // can never match on raw variants
                if variant.name.is_generic() {
                    Err(TypecheckError::NotMatchable {
                        ty: Type::Variant(variant),
                        span: span.clone(),
                    })
                } else {
                    // expect_ty is probably Nothing and we have to assume the type we find from
                    // just the typename is right (if not, we'll get a type mismatch later)
                    // todo: make sure we get a type mismatch later
                    Ok(variant)
                }
            }
        }
    }

    pub fn find(
        pattern: &ast::TypeNamePattern,
        expect_ty: &Type,
        ctx: &Context,
    ) -> TypecheckResult<TypePattern> {
        match pattern {
            ast::TypeNamePattern::TypeName {
                name,
                binding,
                span,
            } if name.as_slice().len() > 1 => match Self::find_variant_case(name, ctx)? {
                Some((variant, case_index)) => {
                    let variant = Self::find_pattern_variant(variant, span, expect_ty)?;

                    Ok(TypePattern::VariantCase {
                        variant: variant.clone(),
                        case_index,
                        data_binding: binding.clone(),
                        span: span.clone(),
                    })
                }

                None => {
                    let ty = Self::find_pattern_ty(name, pattern.span(), expect_ty, ctx)?;

                    Ok(TypePattern::Type {
                        ty,
                        binding: binding.clone(),
                        span: span.clone(),
                    })
                }
            },

            ast::TypeNamePattern::NegatedTypeName { name, span } if name.as_slice().len() > 1 => {
                match Self::find_variant_case(name, ctx)? {
                    Some((variant, case_index)) => {
                        let variant = Self::find_pattern_variant(variant, span, expect_ty)?;

                        Ok(TypePattern::NegatedVariantCase {
                            variant: variant.clone(),
                            case_index,
                            span: span.clone(),
                        })
                    }

                    None => {
                        let ty = Self::find_pattern_ty(name, pattern.span(), expect_ty, ctx)?;
                        Ok(TypePattern::NegatedType {
                            ty,
                            span: span.clone(),
                        })
                    }
                }
            }

            ast::TypeNamePattern::TypeName {
                name,
                binding,
                span,
            } => {
                let ty = Self::find_pattern_ty(name, pattern.span(), expect_ty, ctx)?;
                Ok(TypePattern::Type {
                    ty,
                    binding: binding.clone(),
                    span: span.clone(),
                })
            }

            ast::TypeNamePattern::NegatedTypeName { name, span } => {
                let ty = Self::find_pattern_ty(name, pattern.span(), expect_ty, ctx)?;
                Ok(TypePattern::NegatedType {
                    ty,
                    span: span.clone(),
                })
            }
        }
    }

    pub fn bindings(&self) -> Vec<PatternBinding> {
        match self {
            TypePattern::Type {
                ty,
                binding: Some(ident),
                ..
            } => vec![PatternBinding { ident: ident, ty }],

            TypePattern::VariantCase {
                variant,
                case_index,
                data_binding: Some(ident),
                ..
            } => {
                let case_ty = variant.cases[*case_index].data_ty.as_ref()
                    .expect("variant case pattern with a binding must always reference a case which has a data member");
                vec![PatternBinding { ty: case_ty, ident }]
            }

            _ => Vec::new(),
        }
    }
}

impl fmt::Display for TypePattern {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TypePattern::Type { ty, binding, .. } => {
                write!(f, "{}", ty)?;
                if let Some(binding) = binding {
                    write!(f, " {}", binding)?;
                }
                Ok(())
            }

            TypePattern::NegatedType { ty, .. } => write!(f, "not {}", ty),

            TypePattern::VariantCase {
                variant,
                case_index,
                data_binding,
                ..
            } => {
                let case_ident = &variant.cases[*case_index].ident;
                write!(f, "{}.{}", variant.name, case_ident)?;
                if let Some(binding) = data_binding {
                    write!(f, " {}", binding)?;
                }
                Ok(())
            }

            TypePattern::NegatedVariantCase {
                variant,
                case_index,
                ..
            } => {
                let case_ident = &variant.cases[*case_index].ident;
                write!(f, "not {}.{}", variant.name, case_ident)
            }
        }
    }
}