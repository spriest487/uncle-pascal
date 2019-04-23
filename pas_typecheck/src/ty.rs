use std::{fmt, rc::Rc};

use pas_common::span::*;
use pas_syn::{
    ast::{self, ClassKind, FunctionParamMod, Typed},
    ident::*,
    Operator,
};

use crate::{
    ast::{Class, FunctionDecl, Interface, Variant},
    context::{self, ns, ns::Namespace as _},
    result::*,
    Context, Decl, NameError, TypeAnnotation,
    QualifiedDeclName,
};

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
        }
    }

    /// replace all `Self`-typed args with `self_ty`
    pub fn with_self(&self, self_ty: &Type) -> Self {
        let mut result = self.clone();
        for param in &mut result.params {
            if param.ty == Type::GenericSelf {
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
            .position(|arg| arg.ty == Type::GenericSelf)?;

        Some(&args[self_arg_pos])
    }

    /// given that `self` is the sig of an interface method,
    /// for what type does `impl_func` implement this method, if any?
    pub fn impl_ty<'func>(&self, impl_func: &'func Self) -> Option<&'func Type> {
        if self.params.len() != impl_func.params.len() {
            return None;
        }

        let self_type = if self.return_ty == Type::GenericSelf {
            &impl_func.return_ty
        } else {
            self.params
                .iter()
                .position(|param| param.ty == Type::GenericSelf)
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
                if param.ty == Type::GenericSelf {
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
    GenericSelf,
    GenericParam(Ident),
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
            Type::GenericSelf => Some(builtin_path("Self")),
            Type::Primitive(p) => Some(builtin_path(p.name())),
            Type::Interface(iface) => Some(iface.name.qualified.clone()),
            Type::Record(class) | Type::Class(class) => Some(class.name.qualified.clone()),
            Type::Variant(variant) => Some(variant.name.qualified.clone()),
            _ => None,
        }
    }

    pub fn full_name(&self) -> Option<String> {
        match self {
            Type::Nothing => Some("Nothing".to_string()),
            Type::Any => Some("Any".to_string()),
            Type::GenericSelf => Some("Self".to_string()),
            Type::Primitive(p) => Some(p.name().to_string()),
            Type::Interface(iface) => Some(iface.name.qualified.to_string()),
            Type::Record(class) | Type::Class(class) => Some(class.name.qualified.to_string()),
            Type::Variant(variant) => Some(variant.name.qualified.to_string()),
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

    pub fn members<'ty>(&'ty self) -> impl Iterator<Item = MemberRef<'ty>> {
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
            | Type::GenericSelf
            => false,

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

    pub fn into_iface(self) -> Result<Rc<Interface>, Self> {
        match self {
            Type::Interface(iface) => Ok(iface),
            other => Err(other)
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.full_name() {
            Some(name) => write!(f, "{}", name),
            None => match self {
                Type::Nil => write!(f, "nil"),
                Type::Class(class) => write!(f, "{}", class.name),
                Type::Record(class) => write!(f, "{}", class.name),
                Type::Interface(iface) => write!(f, "{}", iface.name),
                Type::Pointer(target_ty) => write!(f, "^{}", target_ty),
                Type::Array { element, dim } => write!(f, "array[{}] of {}", dim, element),
                Type::GenericParam(ident) => write!(f, "{}", ident),
                _ => unimplemented!("type with no Display impl: {:?}", self),
            },
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

fn parameterize_type(ty: Type, args: &[ast::TypeName], span: &Span, ctx: &Context) -> TypecheckResult<Type> {
    let wrong_args = |expected| TypecheckError::InvalidTypeArgs {
        expected,
        actual: args.len(),
        ty: ty.clone(),
        span: span.clone(),
    };

    let args: Vec<_> = args.iter().map(|arg| typecheck_type(arg, ctx))
        .collect::<TypecheckResult<_>>()?;

    match &ty {
        Type::Record(class) => {
            parameterize_class(class, args, span).map(Rc::new).map(Type::Record)
        }
        Type::Class(class) => {
            parameterize_class(class, args, span).map(Rc::new).map(Type::Class)
        }

        //todo: should be parameterizable
        Type::Variant(variant) => {
            parameterize_variant(variant, args, span).map(Rc::new).map(Type::Variant)
        }
        Type::Interface(_) |
        Type::GenericParam(_) |

        // will never be parameterizable
        Type::GenericSelf | Type::Primitive(_) | Type::Any | Type::Nothing | Type::Pointer(_)
        | Type::Function(_) | Type::Nil | Type::Array { .. } => {
            if !args.is_empty() {
                Err(wrong_args(0))
            } else {
                Ok(ty.clone())
            }
        }
    }
}

pub fn find_type<'c>(name: &IdentPath, ctx: &'c Context) -> context::NamingResult<&'c Type> {
    match ctx.resolve(name) {
        Some(context::MemberRef::Value { value: Decl::Type(ty), .. }) => {
            Ok(ty)
        }

        Some(context::MemberRef::Value { value: unexpected, .. }) => {
            Err(NameError::ExpectedType(
                name.clone(),
                unexpected.clone().into(),
            ))
        },

        Some(context::MemberRef::Namespace { path }) => {
            let ns_ident = path.top().key().unwrap().clone();
            let unexpected = context::UnexpectedValue::Namespace(ns_ident);
            Err(NameError::ExpectedType(name.clone(), unexpected))
        }

        None => {
            Err(NameError::NotFound(name.last().clone()))
        },
    }
}

pub fn typecheck_type(ty: &ast::TypeName, ctx: &Context) -> TypecheckResult<Type> {
    match ty {
        ast::TypeName::Ident { ident, indirection, type_args, span } => {
            let raw_ty = find_type(ident, ctx)?.clone();
            let base_ty = parameterize_type(raw_ty, type_args, span, ctx)?;

            Ok(base_ty.indirect_by(*indirection))
        },

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

fn substitute_type_param<'a, 't: 'a>(ty: &'t Type, params: &[Ident], args: &'a [Type]) -> &'a Type {
    match ty {
        Type::GenericParam(ident) => {
            let index = params.iter().position(|arg| *arg == *ident)
                .expect("member with generic param type must match one of the generic params");

            &args[index]
        },

        not_generic => not_generic,
    }
}

pub fn parameterize_class(
    class: &Class,
    args: Vec<Type>,
    span: &Span
) -> TypecheckResult<Class> {
    let type_params: &[Ident] = &class.name.decl_name.type_params.as_slice();
    if args.len() != type_params.len() {
        return Err(TypecheckError::InvalidTypeArgs {
            ty: Type::Class(class.clone().into()),
            expected: type_params.len(),
            actual: args.len(),
            span: span.clone(),
        })
    }

    let members: Vec<_> = class.members.iter()
        .map(|member| ast::Member {
            ty: substitute_type_param(&member.ty, type_params, &args).clone(),
            ..member.clone()
        })
        .collect();

    let parameterized_name = QualifiedDeclName {
        type_args: args,
        ..class.name.clone()
    };

    Ok(Class {
        name: parameterized_name,
        members,
        span: class.span.clone(),
        kind: class.kind.clone(),
    })
}

pub fn parameterize_variant(
    variant: &Variant,
    args: Vec<Type>,
    span: &Span
) -> TypecheckResult<Variant> {
    let type_params: &[Ident] = &variant.name.decl_name.type_params.as_slice();
    if args.len() != type_params.len() {
        return Err(TypecheckError::InvalidTypeArgs {
            ty: Type::Variant(variant.clone().into()),
            expected: type_params.len(),
            actual: args.len(),
            span: span.clone(),
        })
    }

    let cases: Vec<_> = variant.cases.iter()
        .map(|case| ast::VariantCase {
            data_ty: case.data_ty.as_ref()
                .map(|ty| substitute_type_param(ty, type_params, &args))
                .cloned(),
            ..case.clone()
        })
        .collect();

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
        generic.is_generic()
            && !self.is_generic()
            && self.name() == generic.name()
    }

    fn infer_specialized_from_hint<'out, 'a: 'out, 'b: 'out>(
        &'a self,
        hint: &'b Self
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
            Type::Class(class)
            | Type::Record(class) => class.name.is_generic(),

            Type::Variant(variant) => variant.name.is_generic(),

            _ => false,
        }
    }

    fn name(&self) -> IdentPath {
        self.full_path().expect("only types with full paths can be specialized")
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
                value: Decl::Type(Type::Variant(variant)),
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

    fn find_pattern_ty(
        name: &IdentPath,
        span: &Span,
        ctx: &Context,
    ) -> TypecheckResult<Type> {
        let raw_ty = find_type(name, ctx)?;
        if raw_ty.is_matchable() {
            Ok(raw_ty.clone())
        } else {
            Err(TypecheckError::NotMatchable {
                ty: raw_ty.clone(),
                span: span.clone(),
            })
        }
    }

    pub fn find(pattern: &ast::TypeNamePattern, ctx: &Context) -> TypecheckResult<TypePattern> {
        match pattern {
            ast::TypeNamePattern::TypeName {
                name,
                binding,
                span,
            } if name.as_slice().len() > 1 => match Self::find_variant_case(name, ctx)? {
                Some((variant, case_index)) => Ok(TypePattern::VariantCase {
                    variant: variant.clone(),
                    case_index,
                    data_binding: binding.clone(),
                    span: span.clone(),
                }),

                None => {
                    let ty = Self::find_pattern_ty(name, pattern.span(), ctx)?;
                    Ok(TypePattern::Type {
                        ty,
                        binding: binding.clone(),
                        span: span.clone(),
                    })
                }
            },

            ast::TypeNamePattern::NegatedTypeName {
                name,
                span,
            } if name.as_slice().len() > 1 => match Self::find_variant_case(name, ctx)? {
                Some((variant, case_index)) => Ok(TypePattern::NegatedVariantCase {
                    variant: variant.clone(),
                    case_index,
                    span: span.clone(),
                }),

                None => {
                    let ty = Self::find_pattern_ty(name, pattern.span(), ctx)?;
                    Ok(TypePattern::NegatedType { ty, span: span.clone(), })
                }
            },

            ast::TypeNamePattern::TypeName { name, binding, span, } => {
                let ty = Self::find_pattern_ty(name, pattern.span(), ctx)?;
                Ok(TypePattern::Type {
                    ty,
                    binding: binding.clone(),
                    span: span.clone(),
                })
            }

            ast::TypeNamePattern::NegatedTypeName { name, span } => {
                let ty = Self::find_pattern_ty(name, pattern.span(), ctx)?;
                Ok(TypePattern::NegatedType { ty, span: span.clone(), })
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

            TypePattern::NegatedType { ty ,.. } => write!(f, "not {}", ty),

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
