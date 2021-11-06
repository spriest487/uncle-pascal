use std::{fmt, rc::Rc};

use pas_common::span::*;
use pas_syn::{
    ast::{self, ClassKind, FunctionParamMod, Typed},
    ident::*,
    Operator,
};

use crate::ast::{Member, FunctionParam};
use crate::{
    ast::{Class, FunctionDecl, Variant},
    context,
    result::*,
    Context, Decl, GenericError, GenericResult, GenericTarget, NameError, NamingResult,
    Symbol, TypeAnnotation,
};
use crate::TypeArgsResult::NotGeneric;

#[cfg(test)]
mod test;

pub type TypeParam = ast::TypeParam<Type>;

pub type TypeList = ast::TypeList<Type>;
pub type TypeParamList = ast::TypeList<TypeParam>;

#[derive(Hash, Eq, PartialEq, Copy, Clone, Debug)]
pub enum Conversion {
    Blittable,
    UnsafeBlittable,
    Illegal,
}

pub fn typecheck_type_params(
    type_params: &ast::TypeList<ast::TypeParam<ast::TypeName>>,
    ctx: &mut Context
) -> TypecheckResult<TypeParamList> {
    let mut items = Vec::new();

    for ty_param in &type_params.items {
        let constraint = match &ty_param.constraint {
            Some(constraint) => {
                let is_ty = typecheck_type(&constraint.is_ty, ctx)?;
                Some(ast::TypeConstraint {
                    param_ident: ty_param.ident.clone(),
                    span: constraint.span.clone(),
                    is_ty,
                })
            }
            None => None,
        };

        items.push(ast::TypeParam {
            ident: ty_param.ident.clone(),
            constraint
        });
    }

    Ok(TypeParamList::new(items, type_params.span().clone()))
}

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
pub struct FunctionSigTypeParam {
    pub is_ty: Type,
}

#[derive(Eq, PartialEq, Hash, Clone, Debug)]
pub struct FunctionSig {
    pub return_ty: Type,
    pub params: Vec<FunctionParamSig>,
    pub type_params: Option<ast::TypeList<FunctionSigTypeParam>>,
}

impl FunctionSig {
    pub fn new(
        return_ty: Type,
        params: Vec<FunctionParam>,
        type_params: Option<TypeParamList>,
    ) -> Self {
        let params = params.into_iter()
            .map(|p| FunctionParamSig {
                ty: p.ty.clone(),
                modifier: p.modifier.clone(),
            })
            .collect();

        let type_params = match type_params {
            Some(type_params) => {
                let items: Vec<_> = type_params.items.iter()
                    .map(|decl_param| {
                        let is_ty = decl_param.constraint.as_ref()
                            .map(|c| c.is_ty.clone())
                            .unwrap_or(Type::Any);

                        FunctionSigTypeParam { is_ty }
                    })
                    .collect();

                Some(ast::TypeList::new(items, type_params.span().clone()))
            }
            None => None,
        };

        Self {
            return_ty,
            params,
            type_params,
        }
    }

    pub fn of_decl(decl: &FunctionDecl) -> Self {
        let return_ty = decl.return_ty.clone().unwrap_or(Type::Nothing);
        Self::new(return_ty, decl.params.clone(), decl.type_params.clone())
    }

    /// test if a list of type args that could be used to specialize this function are applicable
    /// e.g. this sig has type params, the number of type params is the same as the number
    /// of args provided, and that each arg passes the constraints for its corresponding param
    pub fn validate_type_args(&self, type_args: &TypeList, span: &Span, ctx: &Context) -> GenericResult<()> {
        let expected_type_args_len = match self.type_params.as_ref() {
            Some(type_params) => type_params.len(),
            None => 0,
        };

        if type_args.len() != expected_type_args_len {
            return Err(GenericError::ArgsLenMismatch {
                expected: expected_type_args_len,
                actual: type_args.len(),
                target: GenericTarget::FunctionSig(self.clone()),
                span: span.clone(),
            });
        }

        let type_params = self.type_params.as_ref().expect("must have type params or previous check would fail");

        for arg_pos in 0..type_params.len() {
            match &type_params.items[arg_pos].is_ty {
                Type::Any => {
                    // nothing to validate
                },

                Type::Interface(is_iface_ident) => {
                    let actual_ty = &type_args.items[arg_pos];
                    if !ctx.is_iface_impl(actual_ty, is_iface_ident) {
                        return Err(GenericError::ArgConstraintNotSatisfied {
                            is_not_ty: Type::Interface(is_iface_ident.clone()),
                            arg_ty: actual_ty.clone(),
                            span: span.clone(),
                        });
                    }
                }

                bad => panic!("unsupported type in signature type param constraint: {}", bad),
            }
        }

        Ok(())
    }

    pub fn specialize_generic(&self, type_args: &TypeList, span: &Span, ctx: &Context) -> GenericResult<Self> {
        self.validate_type_args(type_args, span, ctx)?;

        let params = self
            .params
            .iter()
            .map(|sig_param| {
                let ty = sig_param.ty.clone().substitute_type_args(type_args);
                Ok(FunctionParamSig {
                    ty,
                    ..sig_param.clone()
                })
            })
            .collect::<GenericResult<_>>()?;

        let return_ty = self.return_ty.clone().substitute_type_args(type_args);

        let specialized_sig = FunctionSig {
            return_ty,
            params,
            type_params: self.type_params.clone(),
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
    pub fn self_ty_from_args<'arg>(&self, args: &'arg [Type]) -> Option<&'arg Type> {
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
        write!(f, "function (")?;

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
    Pointer,
}

impl Primitive {
    pub const ALL: [Primitive; 5] = [
        Primitive::Boolean,
        Primitive::Byte,
        Primitive::Int32,
        Primitive::Real32,
        Primitive::Pointer,
    ];

    pub fn name(&self) -> &str {
        match self {
            Primitive::Boolean => "Boolean",
            Primitive::Byte => "Byte",
            Primitive::Int32 => "Integer",
            Primitive::Real32 => "Real32",
            Primitive::Pointer => "Pointer",
        }
    }
}

#[derive(Eq, PartialEq, Hash, Clone, Debug)]
pub struct TypeParamType {
    pub name: Ident,
    pub pos: usize,
    pub is_iface: Option<Box<Type>>,
}

impl fmt::Display for TypeParamType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

pub enum TypeArgsResult<'a> {
    Specialized(&'a TypeList),
    Unspecialized(&'a ast::TypeList<Ident>),
    NotGeneric,
}

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
            Type::Class(_) => true,
            Type::Interface(_) => true,
            Type::Variant(_) => false,
            Type::Array { .. } => false,
            Type::DynArray { .. } => true,
            Type::MethodSelf => true,
            Type::GenericParam(_) => false,
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
            (Type::Array { dim, .. }, Type::Array { dim: other_dim, .. }) => {
                *dim == *other_dim
            }

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
            Type::DynArray { element }
            | Type::Array { element, .. } => Some(element),

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

    pub fn implicit_conversion_from(&self, from: &Self, ctx: &Context) -> Conversion {
        if *self == *from {
            return Conversion::Blittable;
        }

        match self {
            Type::Primitive(Primitive::Pointer) if *from == Type::Nil => Conversion::Blittable,

            Type::Primitive(Primitive::Pointer) if from.is_rc_reference() => Conversion::UnsafeBlittable,
            Type::Primitive(Primitive::Pointer) if from.is_pointer() => Conversion::UnsafeBlittable,

            Type::Pointer(..)
            | Type::Class(..)
            | Type::Interface(..)
            | Type::DynArray { .. } if *from == Type::Primitive(Primitive::Pointer) => Conversion::UnsafeBlittable,

            Type::Pointer(_) if *self == *from || *from == Type::Nil => Conversion::Blittable,

            Type::Interface(iface) => match from {
                Type::Class(..) if ctx.is_iface_impl(from, &iface) => Conversion::Blittable,
                Type::Interface(from_iface) if iface == from_iface => Conversion::Blittable,
                _ => Conversion::Illegal,
            },

            Type::Any => match from {
                Type::DynArray { .. } | Type::Class(..) | Type::Interface(..) => Conversion::Blittable,
                _ => Conversion::Illegal,
            },

            _ => Conversion::Illegal,
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
            | (Type::Primitive(a), Operator::IntegerDivide, Type::Primitive(b))
            | (Type::Primitive(a), Operator::Multiply, Type::Primitive(b)) => *a == *b,

            (_, Operator::Shl, _) | (_, Operator::Shr, _) if *self == *rhs => {
                match self {
                    Type::Primitive(Primitive::Byte) => true,
                    Type::Primitive(Primitive::Int32) => true,
                    _ => false,
                }
            }

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
            let items = name_type_args.items.iter()
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
            },

            Type::Record(name)  if name.decl_name.type_params.is_some() => {
                Type::Record(Box::new(Self::new_class_name(&name, args)))
            },

            Type::Variant(name)  if name.decl_name.type_params.is_some() => {
                Type::Variant(Box::new(Self::new_class_name(&name, args)))
            },

            Type::DynArray { element } => Type::DynArray {
                element: element.substitute_type_args(args).into(),
            },

            Type::Array { element, dim } => Type::Array {
                element: element.substitute_type_args(args).into(),
                dim,
            },

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

pub fn typecheck_type(ty: &ast::TypeName, ctx: &Context) -> TypecheckResult<Type> {
    match ty {
        ast::TypeName::Ident {
            ident,
            indirection,
            type_args,
            span,
        } => {
            let (_, raw_ty) = ctx.find_type(ident)?;
            let raw_ty = raw_ty.clone();

            let ty = match type_args {
                Some(type_args) => {
                    let mut checked_type_arg_items = Vec::new();
                    for arg in &type_args.items {
                        let arg_ty = typecheck_type(arg, ctx)?;
                        checked_type_arg_items.push(arg_ty);
                    }

                    let checked_type_args = TypeList::new(checked_type_arg_items, type_args.span().clone());

                    Type::specialize_generic(&raw_ty, &checked_type_args, span)?
                }

                None => raw_ty.clone(),
            };

            Ok(ty.indirect_by(*indirection))
        }

        ast::TypeName::Array { element, dim, .. } => {
            let element = typecheck_type(element.as_ref(), ctx)?;

            match dim {
                Some(dim) => Ok(Type::Array {
                    element: Box::new(element),
                    dim: *dim,
                }),

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

pub fn specialize_class_def(class: &Class, ty_args: &TypeList, span: &Span) -> GenericResult<Class> {
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
        generic.is_unspecialized_generic() && !self.is_unspecialized_generic() && self.name() == generic.name()
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

pub fn string_type(ctx: &Context) -> TypecheckResult<Type> {
    let span = context::builtin_span();
    let ns = IdentPath::from(Ident::new("System", span.clone()));
    let str_class_name = ast::TypeName::Ident {
        ident: ns.child(Ident::new("String", span.clone())),
        indirection: 0,
        type_args: None,
        span,
    };

    typecheck_type(&str_class_name, ctx)
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum TypePattern {
    VariantCase {
        variant: Symbol,
        case: Ident,
        data_binding: Option<Ident>,
        span: Span,
    },
    NegatedVariantCase {
        variant: Symbol,
        case: Ident,
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
pub struct PatternBinding {
    pub ident: Ident,
    pub ty: Type,
}

impl TypePattern {
    // for an identPath (eg System.Option.Some) check if the last part is a declared variant
    // case and return the separate variant ident (eg System.Option) and case ident (eg Some)
    fn find_variant_case(
        path: &IdentPath,
        ctx: &Context,
    ) -> TypecheckResult<Option<(IdentPath, Ident)>> {
        let variant_parts = path.iter().cloned().take(path.as_slice().len() - 1);
        let stem_name = IdentPath::from_parts(variant_parts);
        let case_ident = path.last();

        match ctx.resolve(&stem_name) {
            Some(context::MemberRef::Value {
                value: Decl::Type { ty, .. },
                ..
            }) if ty.as_variant().is_ok() => {
                let variant = ty.as_variant().unwrap();
                let variant_def = ctx.find_variant_def(&variant.qualified).unwrap();

                // check case with this ident exists
                if variant_def.case_position(case_ident).is_none() {
                    let err_span = path.first().span.to(&case_ident.span);

                    return Err(NameError::MemberNotFound {
                        base: Type::Variant(Box::new(variant_def.name.clone())),
                        member: case_ident.clone(),
                        span: err_span,
                    }
                    .into());
                }

                Ok(Some((
                    variant_def.name.qualified.clone(),
                    case_ident.clone(),
                )))
            }

            _ => Ok(None),
        }
    }

    // patterns referring to a typename can use a generic typename as the pattern, in which
    // case we infer the specialized type (or throw NotMatchable for generics we can't infer a
    // proper specialization for)
    fn find_pattern_ty(
        name: &ast::TypeName,
        span: &Span,
        expect_ty: &Type,
        ctx: &Context,
    ) -> TypecheckResult<Type> {
        let raw_ty = match name {
            ast::TypeName::Ident { ident, .. } => ctx.find_type(ident)?.1.clone(),
            _ => typecheck_type(name, ctx)?,
        };

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
        variant: &IdentPath,
        span: &Span,
        expect_ty: &Type,
        ctx: &mut Context,
    ) -> TypecheckResult<Symbol> {
        match expect_ty {
            expect_var @ Type::Variant(..) => {
                let variant_def = ctx.find_variant_def(variant)?;
                let var_ty = Type::Variant(Box::new(variant_def.name.clone()));

                var_ty.infer_specialized_from_hint(expect_var)
                    .map(|ty| match ty {
                        Type::Variant(v) => {
                            (**v).clone()
                        },
                        _ => unreachable!("should never infer a non-variant specialized type for a generic variant"),
                    })
                    .ok_or_else(|| TypecheckError::NotMatchable {
                        ty: var_ty.clone(),
                        span: span.clone(),
                    })
            }

            _ => {
                let variant_def = ctx.find_variant_def(variant)?;
                // expect_ty is probably Nothing and we have to assume the type we find from
                // just the typename is right (if not, we'll get a type mismatch later)
                // todo: make sure we get a type mismatch later
                Ok(variant_def.name.clone())
            }
        }
    }

    pub fn typecheck(
        pattern: &ast::TypeNamePattern,
        expect_ty: &Type,
        ctx: &mut Context,
    ) -> TypecheckResult<TypePattern> {
        match pattern {
            // this pattern typename will never contain generic args (we can't parse those here),
            // so either this is a non-generic type or we'll infer a specialization from the
            // expression's expected type
            ast::TypeNamePattern::TypeName {
                name,
                kind,
                span,
            } => {
                let span = span.clone();

                match Self::find_variant_case(name, ctx)? {
                    Some((variant, case)) => {
                        let variant = Self::find_pattern_variant(&variant, &span, expect_ty, ctx)?;

                        match kind {
                            ast::TypeNamePatternKind::Is | ast::TypeNamePatternKind::IsWithBinding(..) => {
                                let data_binding = kind.binding().cloned();
                                Ok(TypePattern::VariantCase { variant, case, data_binding, span, })
                            }
                            ast::TypeNamePatternKind::IsNot => {
                                Ok(TypePattern::NegatedVariantCase { variant, case, span, })
                            }
                        }
                    }

                    None => {
                        let ty_name = ast::TypeName::Ident {
                            span: name.span().clone(),
                            indirection: 0,
                            type_args: None,
                            ident: name.clone(),
                        };

                        let ty = Self::find_pattern_ty(&ty_name, pattern.span(), expect_ty, ctx)?;

                        match kind {
                            ast::TypeNamePatternKind::Is | ast::TypeNamePatternKind::IsWithBinding(..) => {
                                let binding = kind.binding().cloned();
                                Ok(TypePattern::Type { ty, binding, span, })
                            }
                            ast::TypeNamePatternKind::IsNot => {
                                Ok(TypePattern::NegatedType { ty, span, })
                            }
                        }
                    }
                }
            },

            ast::TypeNamePattern::ExactType { name, kind, span } => {
                let span = span.clone();

                let ty = Self::find_pattern_ty(name, &span, expect_ty, ctx)?;

                match kind {
                    ast::TypeNamePatternKind::Is | ast::TypeNamePatternKind::IsWithBinding(..) => {
                        let binding = kind.binding().cloned();
                        Ok(TypePattern::Type { ty, binding, span, })
                    }
                    ast::TypeNamePatternKind::IsNot => {
                        Ok(TypePattern::NegatedType { ty, span, })
                    }
                }
            }
        }
    }

    pub fn bindings(&self, ctx: &mut Context) -> TypecheckResult<Vec<PatternBinding>> {
        match self {
            TypePattern::Type {
                ty,
                binding: Some(ident),
                ..
            } => {
                let binding = PatternBinding {
                    ident: ident.clone(),
                    ty: ty.clone(),
                };
                Ok(vec![binding])
            }

            TypePattern::VariantCase {
                variant,
                case,
                data_binding: Some(ident),
                ..
            } => {
                let variant_def = ctx.instantiate_variant(variant)?;
                let case_ty = variant_def.cases.iter()
                    .find_map(|c| if c.ident == *case {
                        c.data_ty.clone()
                    } else {
                        None
                    })
                    .expect("variant case pattern with a binding must always reference a case which has a data member");

                let binding = PatternBinding {
                    ty: case_ty,
                    ident: ident.clone(),
                };
                Ok(vec![binding])
            }

            _ => Ok(Vec::new()),
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
                case,
                data_binding,
                ..
            } => {
                write!(f, "{}.{}", variant.qualified, case)?;
                if let Some(binding) = data_binding {
                    write!(f, " {}", binding)?;
                }
                Ok(())
            }

            TypePattern::NegatedVariantCase { variant, case, .. } => {
                write!(f, "not {}.{}", variant.qualified, case)
            }
        }
    }
}
