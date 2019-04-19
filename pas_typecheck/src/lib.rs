pub use self::{annotation::*, context::*, result::*, ty::*};

mod annotation;
mod context;
mod result;

pub mod ast {
    pub use self::{
        block::*, cond::*, ctor::*, expression::*, function::*, iter::*, op::*, statement::*,
        typedecl::*, unit::*,
    };

    mod block;
    mod cond;
    mod ctor;
    mod expression;
    mod function;
    mod iter;
    mod op;
    mod statement;
    mod typedecl;
    mod unit;

    mod prelude {
        pub use pas_common::span::*;
        pub use pas_syn::{
            ast::{self, FunctionParamMod},
            ident::*,
            parse::InvalidStatement,
        };

        pub use crate::{
            annotation::*, ast::*, context::*, result::*, FunctionParamSig, FunctionSig, Primitive,
            Type, TypePattern,
        };
    }
}

pub mod ty {
    use std::{fmt, rc::Rc};

    use pas_common::span::*;
    use pas_syn::{
        ast::{self, ClassKind, FunctionParamMod, Typed},
        ident::*,
        Operator,
    };

    use crate::{
        ast::{Class, FunctionDecl, Interface, Variant},
        context::ns,
        result::*,
        Context, Decl, NameError, TypeAnnotation,
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

    #[derive(Eq, PartialEq, Hash, Clone, Debug)]
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
                Type::Interface(iface) => Some(iface.ident.clone()),
                Type::Record(class) | Type::Class(class) => Some(class.ident.clone()),
                Type::Variant(variant) => Some(variant.ident.clone()),
                _ => None,
            }
        }

        pub fn full_name(&self) -> Option<String> {
            match self {
                Type::Nothing => Some("Nothing".to_string()),
                Type::Any => Some("Any".to_string()),
                Type::GenericSelf => Some("Self".to_string()),
                Type::Primitive(p) => Some(p.name().to_string()),
                Type::Interface(iface) => Some(iface.ident.to_string()),
                Type::Record(class) | Type::Class(class) => Some(class.ident.to_string()),
                Type::Variant(variant) => Some(variant.ident.to_string()),
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
                | Type::Function(..) => false,

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
                    Type::Class(..) => ctx.is_iface_impl(from, &iface.ident),
                    Type::Interface(from_iface) => iface.ident == from_iface.ident,
                    _ => false,
                },
                Type::Any => match from {
                    Type::Class(..) | Type::Interface(..) => true,
                    _ => false,
                }
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
                Type::Interface(iface) => iface.methods.iter().find(|m| m.ident == *method),
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
    }

    impl fmt::Display for Type {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            match self.full_name() {
                Some(name) => write!(f, "{}", name),
                None => match self {
                    Type::Nil => write!(f, "nil"),
                    Type::Class(class) => write!(f, "{}", class.ident),
                    Type::Record(class) => write!(f, "{}", class.ident),
                    Type::Interface(iface) => write!(f, "{}", iface.ident),
                    Type::Pointer(target_ty) => write!(f, "^{}", target_ty),
                    Type::Array { element, dim } => write!(f, "array[{}] of {}", dim, element),
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
            ty: &ast::TypeName,
            span: &Span,
            ctx: &Context,
        ) -> TypecheckResult<Type> {
            let ty = ctx.find_type(ty)?;

            if !ty.is_matchable() {
                Err(TypecheckError::NotMatchable {
                    ty: ty.clone(),
                    span: span.clone(),
                })
            } else {
                Ok(ty)
            }
        }

        pub fn find(pattern: &ast::TypeNamePattern, ctx: &Context) -> TypecheckResult<TypePattern> {
            match pattern {
                ast::TypeNamePattern::TypeName {
                    ty:
                        ast::TypeName::Ident {
                            indirection: 0,
                            ident,
                        },
                    binding,
                    span,
                } if ident.as_slice().len() > 1 => match Self::find_variant_case(ident, ctx)? {
                    Some((variant, case_index)) => Ok(TypePattern::VariantCase {
                        variant: variant.clone(),
                        case_index,
                        data_binding: binding.clone(),
                        span: span.clone(),
                    }),

                    None => {
                        let ty_name = ast::TypeName::Ident { ident: ident.clone(), indirection: 0 };
                        let ty = Self::find_pattern_ty(&ty_name, pattern.span(), ctx)?;
                        Ok(TypePattern::Type {
                            ty,
                            binding: binding.clone(),
                            span: span.clone(),
                        })
                    }
                },

                ast::TypeNamePattern::NegatedTypeName {
                    ty:
                        ast::TypeName::Ident {
                            indirection: 0,
                            ident,
                        },
                    span,
                } if ident.as_slice().len() > 1 => match Self::find_variant_case(ident, ctx)? {
                    Some((variant, case_index)) => Ok(TypePattern::NegatedVariantCase {
                        variant: variant.clone(),
                        case_index,
                        span: span.clone(),
                    }),

                    None => {
                        let ty_name = ast::TypeName::Ident { ident: ident.clone(), indirection: 0 };
                        let ty = Self::find_pattern_ty(&ty_name, pattern.span(), ctx)?;
                        Ok(TypePattern::NegatedType { ty, span: span.clone(), })
                    }
                },

                ast::TypeNamePattern::TypeName { ty, binding, span, } => {
                    let ty = Self::find_pattern_ty(ty, pattern.span(), ctx)?;
                    Ok(TypePattern::Type {
                        ty,
                        binding: binding.clone(),
                        span: span.clone(),
                    })
                }

                ast::TypeNamePattern::NegatedTypeName { ty, span } => {
                    let ty = Self::find_pattern_ty(ty, pattern.span(), ctx)?;
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
                    write!(f, "{}.{}", variant.ident, case_ident)?;
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
                    write!(f, "not {}.{}", variant.ident, case_ident)
                }
            }
        }
    }
}

#[cfg(test)]
mod test {
    use crate::FunctionParamSig;

    use super::*;

    const INT32: Type = Type::Primitive(Primitive::Int32);
    const BOOL: Type = Type::Primitive(Primitive::Boolean);

    #[test]
    fn sig_without_self_is_invalid_impl() {
        let iface_sig = FunctionSig {
            return_ty: BOOL,
            params: vec![],
        };

        let impl_sig = FunctionSig {
            return_ty: INT32,
            params: vec![],
        };

        assert_eq!(None, iface_sig.impl_ty(&impl_sig));
    }

    #[test]
    fn sig_with_self_return_is_valid_impl() {
        let iface_sig = FunctionSig {
            return_ty: Type::GenericSelf,
            params: vec![],
        };

        let impl_sig = FunctionSig {
            return_ty: INT32,
            params: vec![],
        };

        assert_eq!(Some(&INT32), iface_sig.impl_ty(&impl_sig));
    }

    #[test]
    fn sig_with_no_params_is_invalid_impl() {
        let iface_sig = FunctionSig {
            return_ty: Type::Nothing,
            params: vec![],
        };

        let impl_sig = FunctionSig {
            return_ty: Type::Nothing,
            params: vec![],
        };

        assert_eq!(None, iface_sig.impl_ty(&impl_sig));
    }

    #[test]
    fn sig_with_self_param_is_valid_impl() {
        let iface_sig = FunctionSig {
            return_ty: Type::Nothing,
            params: vec![FunctionParamSig::by_val(Type::GenericSelf)],
        };

        let impl_sig = FunctionSig {
            return_ty: Type::Nothing,
            params: vec![FunctionParamSig::by_val(INT32)],
        };

        assert_eq!(Some(&INT32), iface_sig.impl_ty(&impl_sig));
    }

    #[test]
    fn sig_with_self_param_and_return_is_valid_impl() {
        let iface_sig = FunctionSig {
            return_ty: Type::GenericSelf,
            params: vec![FunctionParamSig::by_val(Type::GenericSelf)],
        };

        let impl_sig = FunctionSig {
            return_ty: INT32,
            params: vec![FunctionParamSig::by_val(INT32)],
        };

        assert_eq!(Some(&INT32), iface_sig.impl_ty(&impl_sig));
    }

    #[test]
    fn sig_with_mismatched_self_param_and_return_is_invalid_impl() {
        let iface_sig = FunctionSig {
            return_ty: Type::GenericSelf,
            params: vec![FunctionParamSig::by_val(Type::GenericSelf)],
        };

        let impl_sig = FunctionSig {
            return_ty: INT32,
            params: vec![FunctionParamSig::by_val(BOOL)],
        };

        assert_eq!(None, iface_sig.impl_ty(&impl_sig));
    }
}
