mod specialize;
mod array;

#[cfg(test)]
mod test;

pub mod layout;
pub mod pattern;
pub mod primitive;
pub mod sig;
pub mod ty_param;

pub use self::array::*;
pub use self::pattern::*;
pub use self::primitive::*;
pub use self::sig::*;
pub use self::specialize::*;
pub use self::ty_param::*;
use crate::ast;
use crate::ast::Access;
use crate::ast::ArrayTypeName;
use crate::ast::Ident;
use crate::ast::IdentPath;
use crate::ast::IdentTypeName;
use crate::ast::StructKind;
use crate::ast::TypeAnnotation;
use crate::ast::INTERFACE_METHOD_ACCESS;
use crate::typ::ast::{FieldDecl, MethodDecl};
use crate::typ::builtin_span;
use crate::typ::builtin_unit_path;
use crate::typ::context;
use crate::typ::result::*;
use crate::typ::Context;
use crate::typ::GenericError;
use crate::typ::GenericResult;
use crate::typ::GenericTarget;
use crate::typ::NameResult;
use crate::typ::Symbol;
use crate::typ::Typed;
use crate::typ::SYSTEM_UNIT_NAME;
use crate::Operator;
use common::span::*;
use std::borrow::Cow;
use std::fmt;
use std::rc::Rc;

#[derive(Eq, PartialEq, Hash, Clone, Debug)]
pub enum Type {
    Nothing,
    Nil,
    Primitive(Primitive),
    Pointer(Rc<Type>),
    Function(Rc<FunctionSig>),
    Record(Rc<Symbol>),
    Class(Rc<Symbol>),
    Interface(Rc<IdentPath>),
    Variant(Rc<Symbol>),
    Array(Rc<ArrayType>),
    DynArray { element: Rc<Type> },
    MethodSelf,
    GenericParam(Rc<TypeParamType>),
    Any,
    Enum(Rc<Symbol>),
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
    pub fn record(sym: impl Into<Symbol>) -> Self {
        Type::Record(Rc::new(sym.into()))
    }

    pub fn class(sym: impl Into<Symbol>) -> Self {
        Type::Class(Rc::new(sym.into()))
    }

    pub fn variant(sym: impl Into<Rc<Symbol>>) -> Self {
        Type::Variant(sym.into())
    }

    pub fn enumeration(sym: impl Into<Symbol>) -> Self {
        Type::Enum(Rc::new(sym.into()))
    }
    
    pub fn interface(name: impl Into<IdentPath>) -> Self {
        Type::Interface(Rc::new(name.into()))
    }
    
    pub fn generic_param(name: Ident, pos: usize) -> Type {
        let ty_param_ty = TypeParamType {
            name,
            pos,
            is_iface: None,
        };
        
        Type::GenericParam(Rc::new(ty_param_ty))
    }

    pub fn array(self, dim: usize) -> Self {
        Type::Array(Rc::new(ArrayType {
            element_ty: self,
            dim,
        }))
    }

    pub fn dyn_array(self) -> Self {
        Type::DynArray { element: Rc::new(self) }
    }

    pub fn generic_constrained_param(name: Ident, pos: usize, is_iface: impl Into<Rc<Type>>) -> Type {
        let ty_param_ty = TypeParamType {
            name,
            pos,
            is_iface: Some(is_iface.into()),
        };

        Type::GenericParam(Rc::new(ty_param_ty))
    }
    
    pub fn from_struct_type(sym: impl Into<Symbol>, kind: StructKind) -> Self {
        let sym = Rc::new(sym.into());
        
        match kind {
            StructKind::Class => Type::Class(sym),
            StructKind::Record | StructKind::PackedRecord => Type::Record(sym),
        }
    }
    
    pub fn full_path(&self) -> Option<Cow<IdentPath>> {
        match self {
            Type::Nothing => Some(Cow::Owned(builtin_unit_path("Nothing"))),
            Type::Any => Some(Cow::Owned(builtin_unit_path("Any"))),
            Type::MethodSelf => Some(Cow::Owned(IdentPath::from(Ident::new("Self", builtin_span())))),
            Type::Primitive(p) => Some(Cow::Owned(builtin_unit_path(p.name()))),
            Type::Interface(iface) => Some(Cow::Borrowed(iface.as_ref())),
            
            Type::Record(decl_name) 
            | Type::Class(decl_name) 
            | Type::Variant(decl_name) => Some(Cow::Borrowed(&decl_name.full_path)),
            
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

    pub fn has_default(&self, ctx: &Context) -> NameResult<bool> {
        // in an unsafe context, we can assign a default value to any variable
        if ctx.allow_unsafe() {
            return Ok(true);
        }

        let result = match self {
            Type::Any
            | Type::Class(_)
            | Type::Interface(_)
            | Type::DynArray { .. }
            | Type::Function(_)
            | Type::Nothing
            | Type::GenericParam(_)
            | Type::Enum(_) => false,

            | Type::Variant(variant_name) => {
                let def = ctx.instantiate_variant_def(&variant_name)?;
                let mut result = true;
                for case in &def.cases {
                    if let Some(data_ty) = &case.data_ty {
                        if !data_ty.has_default(ctx)? {
                            result = false;
                            break;
                        }
                    }
                }

                result
            }

            | Type::Record(..) => {
                let fields = self.fields(ctx)?;
                let mut result = true;
                for field in &fields {
                    if !field.ty.has_default(ctx)? {
                        result = false;
                        break;
                    }
                }
                
                result
            }

            Type::Array(array_ty) => array_ty.element_ty.has_default(ctx)?,

            Type::Nil => true,
            Type::Primitive(..) => true,
            Type::Pointer(..) => true,
            
            Type::MethodSelf => {
                let current_self_ty = ctx
                    .current_function_body_env()
                    .and_then(|env| env.self_ty.as_ref());
                
                if let Some(self_ty) = current_self_ty {
                    self_ty.has_default(ctx)?
                } else {
                    false
                }
            }
        };
        
        Ok(result)
    }

    pub fn of_decl(type_decl: &ast::TypeDeclItem<Typed>) -> Self {
        match type_decl {
            ast::TypeDeclItem::Struct(class) if class.kind == StructKind::Record => {
                Type::Record(Rc::new(class.name.clone()))
            },

            ast::TypeDeclItem::Struct(class) => Type::Class(Rc::new(class.name.clone())),

            ast::TypeDeclItem::Variant(variant) => Type::Variant(variant.name.clone()),

            ast::TypeDeclItem::Interface(iface) => {
                Type::Interface(Rc::new(iface.name.full_path.clone()))
            },

            ast::TypeDeclItem::Enum(enum_decl) => Type::Enum(Rc::new(enum_decl.name.clone())),

            ast::TypeDeclItem::Alias(alias) => (*alias.ty).clone(),
        }
    }

    pub fn find_data_member(&self, member: &Ident, ctx: &Context) -> NameResult<Option<FieldDecl>> {
        match self {
            Type::Class(class_name) | Type::Record(class_name) => {
                let def = ctx.instantiate_struct_def(class_name)?;

                Ok(def.find_field(member).cloned())
            },

            _ => Ok(None),
        }
    }

    pub fn get_field(&self, index: usize, ctx: &Context) -> NameResult<Option<FieldDecl>> {
        match self {
            Type::Record(class) | Type::Class(class) => {
                let class = ctx.instantiate_struct_def(class)?;
                let field = class.fields().nth(index).cloned();
                
                Ok(field)
            },

            _ => Ok(None),
        }
    }

    pub fn field_count(&self, ctx: &Context) -> NameResult<usize> {
        match self {
            Type::Record(class) | Type::Class(class) => {
                let class = ctx.find_struct_def(&class.full_path)?;
                Ok(class.fields().count())
            },

            _ => Ok(0),
        }
    }

    pub fn fields(&self, ctx: &Context) -> NameResult<Vec<FieldDecl>> {
        let mut members = Vec::new();
        for i in 0..self.field_count(ctx)? {
            let member = self.get_field(i, ctx)?.unwrap();

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
    pub fn contains_unresolved_params(&self, ctx: &Context) -> bool {
        if let Type::GenericParam(ty_param_ty) = self {
            let mut func_ty_params = ctx
                .current_function_body_env()
                .into_iter()
                .flat_map(|env| {
                    let func_ty_params = env.ty_params
                        .iter()
                        .flat_map(|params| params.iter());
                    
                    let enclosing_ty_params = env.self_ty
                        .iter()
                        .flat_map(|self_ty| self_ty
                            .type_params()
                            .into_iter()
                            .flat_map(|params| params.iter()));
                    
                    func_ty_params.chain(enclosing_ty_params)
                });

            return !func_ty_params.any(|param| param.name == ty_param_ty.name);
        }

        if let TypeArgsResult::Specialized(_, type_args) = &self.type_args() {
            return type_args.items.iter().any(|a| a.contains_unresolved_params(ctx));
        }

        match self {
            Type::Array(array_ty) => array_ty.element_ty.contains_unresolved_params(ctx),
            
            Type::DynArray { element, .. } => element.contains_unresolved_params(ctx),
            
            Type::Function(sig) => {
                sig.contains_generic_params(ctx)
            }

            _ => false,
        }
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
                match (&name.type_params, &name.type_args) {
                    (Some(type_params), None) => {
                        TypeArgsResult::Unspecialized(type_params)
                    }

                    (Some(type_params), Some(type_args)) => {
                        TypeArgsResult::Specialized(type_params, type_args)
                    }
                    
                    (None, None) => {
                        TypeArgsResult::NotGeneric
                    }
                    
                    (None, Some(..)) => unreachable!(),
                }
            },

            _ => TypeArgsResult::NotGeneric,
        }
    }

    pub fn type_params(&self) -> Option<&TypeParamList> {
        match self {
            Type::Variant(name) | Type::Class(name) | Type::Record(name) => {
                name.type_params.as_ref()
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
    
    pub fn is_sized(&self, ctx: &Context) -> NameResult<bool> {
        let is_unsized = ctx.is_unsized_ty(self)?;
        Ok(!is_unsized)
    }
    
    pub fn expect_sized(&self, ctx: &Context, at: &Span) -> TypeResult<()> {
        let sized = self
            .is_sized(ctx)
            .map_err(|e| TypeError::from_name_err(e, at.clone()))?;

        if !sized {
            return Err(TypeError::InvalidUnsizedType {
                ty: self.clone(),
                at: at.clone(),
            });
        }
        
        Ok(())
    }

    pub fn deref_ty(&self) -> Option<&Type> {
        match self {
            Type::Pointer(ty) => Some(ty.as_ref()),
            _ => None,
        }
    }

    pub fn ptr(self) -> Self {
        Type::Pointer(Rc::new(self))
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
                // can't offset untyped pointers
                if (op == Operator::Add || op == Operator::Sub) && self.deref_ty().is_none() {
                    return false;
                }
                
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
            ) => {
                // can't offset untyped pointers
                if (op == Operator::Add || op == Operator::Sub) && self.deref_ty().is_none() {
                    return false;
                }

                *self == *rhs
            },

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
    
    pub fn methods(&self, ctx: &Context) -> NameResult<Vec<MethodDecl>> {
        match self {
            Type::Interface(iface) => {
                let iface_def = ctx.find_iface_def(iface)?;
                let methods = iface_def
                    .methods
                    .iter()
                    .map(|m| MethodDecl { 
                        func_decl: m.decl.clone(),
                        access: Access::Published,
                    })
                    .collect();
                
                Ok(methods)
            },

            Type::Record(name) | Type::Class(name) => {
                let struct_def = if name.is_unspecialized_generic() {
                    ctx.find_struct_def(&name.full_path)?.clone()
                } else {
                    ctx.instantiate_struct_def(&name)?
                };

                let methods = struct_def 
                    .methods()
                    .map(|method| method.clone())
                    .collect();

                Ok(methods)
            }

            Type::Variant(name) => {
                let variant_def = if name.is_unspecialized_generic() {
                    ctx.find_variant_def(&name.full_path)?.clone()
                } else {
                    ctx.instantiate_variant_def(&name)?
                };
                
                Ok(variant_def.methods.clone())
            }

            Type::Primitive(primitive) => {
                let methods = ctx
                    .get_primitive_methods(*primitive)
                    .values()
                    .cloned()
                    .collect();

                Ok(methods)
            }

            // Type::GenericParam(param) => match &param.is_iface {
            //     Some(is_iface) => is_iface.methods(ctx),
            //     None => Ok(Vec::new()),
            // }

            _ => Ok(Vec::new()),
        }
    }
    
    pub fn methods_at(&self,
        ctx: &Context,
        at: &Span
    ) -> TypeResult<Vec<MethodDecl>> {
        self
            .methods(ctx)
            .map_err(|err| {
                TypeError::from_name_err(err, at.clone())
            })
    }

    pub fn find_method_index(
        &self,
        name: &Ident,
        sig: &FunctionSig,
        ctx: &Context
    ) -> NameResult<Option<usize>> {
        match self {
            Type::Interface(iface_name) => {
                let iface_def = ctx.find_iface_def(iface_name)?;

                let index = iface_def
                    .methods
                    .iter()
                    .enumerate()
                    .find_map(|(index, m)| {
                        if *m.ident() == *name && m.decl.sig().eq_as_impl(sig) {
                            Some(index)
                        } else {
                            None
                        }
                    });

                Ok(index)
            }

            Type::Record(type_name) | Type::Class(type_name) => {
                let struct_def = if type_name.is_unspecialized_generic() {
                    ctx.find_struct_def(&type_name.full_path)?.clone()
                } else {
                    ctx.instantiate_struct_def(&type_name)?
                };

                Ok(find_in_method_decls(name, sig, struct_def.methods.iter()))
            }

            Type::Variant(type_name) => {
                let variant_def = if type_name.is_unspecialized_generic() {
                    ctx.find_variant_def(&type_name.full_path)?.clone()
                } else {
                    ctx.instantiate_variant_def(&type_name)?
                };

                Ok(find_in_method_decls(name, sig, variant_def.methods.iter()))
            }

            Type::Primitive(primitive) => {
                let methods = ctx.get_primitive_methods(*primitive);
                
                Ok(find_in_method_decls(name, sig, methods.values()))
            }

            // Type::GenericParam(param) => match &param.is_iface {
            //     Some(is_iface) => is_iface.find_method_index(name, sig, ctx),
            //     None => Ok(None),
            // }

            _ => Ok(None),
        }
    }

    pub fn get_method(&self, method_index: usize, ctx: &Context) -> NameResult<MethodDecl> {
        match self {
            Type::Interface(iface) => {
                let iface_def = ctx.find_iface_def(iface)?;

                let iface_method = iface_def.methods
                    .get(method_index)
                    .unwrap_or_else(|| panic!("invalid method index: {} in {}", method_index, iface));
                
                Ok(MethodDecl {
                    func_decl: iface_method.decl.clone(),
                    access: INTERFACE_METHOD_ACCESS,
                })
            },

            Type::Record(name) | Type::Class(name) => {
                let struct_def = if name.is_unspecialized_generic() {
                    ctx.find_struct_def(&name.full_path)?.clone()
                } else {
                    ctx.instantiate_struct_def(&name)?
                };

                let method = struct_def.methods.get(method_index);
                Ok(method.cloned().expect("invalid method index"))
            }

            Type::Variant(name) => {
                let variant_def = if name.is_unspecialized_generic() {
                    ctx.find_variant_def(&name.full_path)?.clone()
                } else {
                    ctx.instantiate_variant_def(&name)?
                };

                let method = variant_def.methods.get(method_index);
                Ok(method.cloned().expect("invalid method index"))
            }

            Type::Primitive(primitive) => {
                let methods = ctx.get_primitive_methods(*primitive);
                let method = methods.values().nth(method_index);

                Ok(method.cloned().expect("invalid method index"))
            }

            // Type::GenericParam(param) => match &param.is_iface {
            //     Some(is_iface) => is_iface.get_method(method_index, ctx),
            //     None => panic!("invalid type for method: {self}"),
            // }

            _ => panic!("invalid type for method: {self}"),
        }
    }
    
    pub fn find_method(&self,
        method_ident: &Ident,
        sig: &FunctionSig,
        ctx: &Context
    ) -> NameResult<Option<(usize, MethodDecl)>> {
        match self.find_method_index(method_ident, sig, ctx)? {
            Some(index) => {
                let method = self.get_method(index, ctx)?;
                Ok(Some((index, method)))
            }

            None => {
                Ok(None)
            }
        }
    }
    
    pub fn get_current_access(&self, ctx: &Context) -> Access {
        match self.full_path() {
            Some(name) => ctx.get_access(name.as_ref()),
            
            None => Access::Public,
        }
    }

    pub fn implemented_ifaces(&self, ctx: &Context) -> NameResult<Vec<Type>> {
        match self {
            Type::GenericParam(param_ty) => match &param_ty.is_iface {
                Some(as_iface) => Ok(vec![(**as_iface).clone()]),
                None => Ok(Vec::new()),
            },

            Type::Primitive(primitive) => {
                Ok(ctx.get_primitive_impls(*primitive).to_vec())
            }

            Type::Record(name) | Type::Class(name) => {
                let def = ctx.instantiate_struct_def(name)?;
                Ok(def.implements.clone())
            }
            
            Type::Variant(name) => {
                let def = ctx.instantiate_variant_def(name)?;
                Ok(def.implements.clone())
            }

            _ => {
                Ok(Vec::new())
            },
        }
    }
    
    pub fn implemented_ifaces_at(&self, ctx: &Context, at: &Span) -> TypeResult<Vec<Type>> {
        self.implemented_ifaces(ctx)
            .map_err(|err| TypeError::from_name_err(err, at.clone()))
    }
    
    pub fn into_something(self) -> Option<Self> {
        match self {
            Type::Nothing => None,
            something => Some(something),
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

    pub fn as_iface(&self) -> Option<&IdentPath> {
        match self {
            Type::Interface(iface) => Some(iface),
            _ => None,
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

    pub fn match_constraint(&self, constraint_ty: &Type, ctx: &Context) -> bool {
        match constraint_ty {
            Type::Interface(..) => {
                // todo should we try to return an error here?
                ctx.is_implementation(self, constraint_ty).ok().unwrap_or(false)
            }

            // "Any" used as a constraint means all types, nothing to validate
            Type::Any => true,

            // nothing else is a valid constraint
            _ => false,
        }
    }

    // todo: error handling
    pub fn substitute_type_args(self, args: &impl TypeArgResolver) -> Self {
        if args.len() == 0 {
            return self;
        }

        match self {
            Type::GenericParam(param) => args.resolve(&param).into_owned(),

            Type::Class(name) if name.type_params.is_some() => {
                Type::Class(Rc::new((*name).clone().substitute_ty_args(args)))
            },

            Type::Record(name) if name.type_params.is_some() => {
                Type::Record(Rc::new((*name).clone().substitute_ty_args(args)))
            },

            Type::Variant(name) if name.type_params.is_some() => {
                Type::Variant(Rc::new((*name).clone().substitute_ty_args(args)))
            },

            Type::DynArray { element } => Type::DynArray {
                element: (*element).clone().substitute_type_args(args).into(),
            },

            Type::Array(array_ty) => {
                let array_ty = (*array_ty).clone();
                ArrayType {
                    element_ty: array_ty.element_ty.substitute_type_args(args).into(),
                    dim: array_ty.dim,
                }
                .into()
            },

            Type::Pointer(base_ty) => (*base_ty).clone().substitute_type_args(args).ptr(),

            Type::Function(sig) => {
                let sig = sig.substitute_type_args(args);
                Type::Function(Rc::new(sig))
            },

            other => other,
        }
    }

    pub fn specialize_generic<'a, 'res>(
        &'a self,
        args: &'a TypeArgList,
        ctx: &Context,
    ) -> GenericResult<Cow<'res, Self>>
    where
        'a: 'res,
    {
        let specialized = match self {
            Type::GenericParam(type_param) => args.resolve(type_param),

            Type::Record(sym) => {
                let sym = specialize_generic_name(&sym, args, ctx)?;
                let record_ty = Type::Record(Rc::new(sym.into_owned()));

                Cow::Owned(record_ty)
            },

            Type::Class(sym) => {
                let sym = specialize_generic_name(&sym, args, ctx)?;
                let class_ty = Type::Class(Rc::new(sym.into_owned()));

                Cow::Owned(class_ty)
            },

            Type::Variant(variant) => {
                let sym = specialize_generic_name(&variant, args, ctx)?;
                let variant_ty = Type::Variant(Rc::new(sym.into_owned()));

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
                    element: Rc::new(element_ty.into_owned()),
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

    pub fn visit_generics<Visitor>(self, visitor: &Visitor) -> Self
    where
        Visitor: Fn(TypeParamType) -> TypeParamType
    {
        match self {
            Type::GenericParam(type_param) => {
                Type::GenericParam(Rc::new(visitor((*type_param).clone())))
            }

            Type::Record(sym) => {
                Type::Record(Rc::new((*sym).clone().visit_generics(visitor)))
            },

            Type::Class(sym) => {
                Type::Class(Rc::new((*sym).clone().visit_generics(visitor)))
            },

            Type::Variant(sym) => {
                Type::Variant(Rc::new((*sym).clone().visit_generics(visitor)))
            },

            Type::Enum(sym) => {
                Type::Enum(Rc::new((*sym).clone().visit_generics(visitor)))
            }

            Type::Array(array_ty) => {
                let element_ty = array_ty.element_ty.clone().visit_generics(visitor);
                let arr_ty = ArrayType {
                    element_ty,
                    dim: array_ty.dim,
                };
                Type::from(arr_ty)
            },

            Type::DynArray { element } => {
                let element_ty = (*element).clone().visit_generics(visitor);
                Type::dyn_array(element_ty)
            },

            Type::Function(sig) => {
                let sig = (*sig).clone().visit_generics(visitor);
                Type::Function(Rc::new(sig))
            },
            
            Type::Pointer(deref) => {
                Type::Pointer(Rc::new((*deref).clone().visit_generics(visitor)))
            }

            Type::Nothing
            | Type::Nil
            | Type::Primitive(..)
            | Type::Interface(..)
            | Type::MethodSelf
            | Type::Any => self,
        }
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
            Type::Primitive(p) => write!(f, "{}.{}", SYSTEM_UNIT_NAME, p.name()),
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

#[derive(Debug, Copy, Clone)]
pub struct TypeMemberRef<'ty> {
    pub ident: &'ty Ident,
    pub ty: &'ty Type,
}

pub fn typecheck_type_path(path: &ast::TypePath, ctx: &mut Context) -> TypeResult<Type> {
    let (_, ty) = ctx
        .find_type(&path.name)
        .map_err(|err| TypeError::from_name_err(err, path.span.clone()))?;

    let ty = ty.clone();

    // validate type params, it's an error to write a path with mismatched type params
    let expect_params = ty.type_params().cloned();

    let params_match = match (&expect_params, &path.type_params) {
        (Some(expect), Some(actual)) => {
            if expect.len() != actual.len() {
                false
            } else {
                expect
                    .iter()
                    .zip(actual.iter())
                    .all(|(param, actual_name)| param.name == *actual_name)
            }
        }
        (None, None) => true,
        _ => false,
    };

    if !params_match {
        let err = GenericError::ParamMismatch {
            target: GenericTarget::Name(path.name.clone()),
            expected: expect_params,
            actual: path.type_params.clone(),
        };

        return Err(TypeError::from_generic_err(err, path.span.clone()));
    }
    
    Ok(ty)
}


pub fn typecheck_type(ty: &ast::TypeName, ctx: &mut Context) -> TypeResult<Type> {
    match ty {
        ast::TypeName::Ident(IdentTypeName {
            ident,
            indirection,
            type_args,
            span,
        }) => {
            let (_, raw_ty) = ctx
                .find_type(ident)
                .map_err(|err| TypeError::NameError {
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
                    let checked_type_args = TypeArgList::new(checked_type_arg_items, type_args_span);

                    Type::specialize_generic(&raw_ty, &checked_type_args, ctx)
                        .map_err(|err| TypeError::from_generic_err(err, span.clone()))?
                        .into_owned()
                },

                None => raw_ty.clone(),
            };

            Ok(ty.indirect_by(*indirection))
        },

        ast::TypeName::Array(ArrayTypeName { element, dim, .. }) => {
            typecheck_array_type(element, dim, ctx)
        },

        ast::TypeName::Function(func_ty_name) => {
            let return_ty = match &func_ty_name.return_ty {
                Some(return_ty) => typecheck_type(return_ty, ctx)?,
                None => Type::Nothing,
            };

            let mut params = Vec::new();
            for param in &func_ty_name.params {
                let param_ty = typecheck_type(&param.ty, ctx)?;

                params.push(FunctionSigParam {
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

        ast::TypeName::Unspecified(_) => unreachable!("trying to resolve unknown type"),
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

    fn name(&self) -> Cow<IdentPath> {
        self.full_path()
            .expect("only types with full paths can be specialized")
    }

    /// Apply type a given set of type arguments to this type, with a parameter list indicating
    /// which parameters we're providing matching arguments for.
    /// e.g. in the method `Class[A].Method[B](a: A, b: B)`, there are two separate parameter
    /// lists which a provided arg list of length 1 could satisfy
    fn apply_type_args_by_name(self, params: &impl TypeParamContainer, args: &impl TypeArgResolver) -> Self {
        // callers should already have checked this
        assert_eq!(params.len(), args.len(), "apply_type_args: params and args counts did not match");

        let result = match self.clone() {
            Type::GenericParam(param) => {                
                // search by name is OK because in any scope where there's multiple levels of 
                // generic params, the names of params must be unique between all param lists
                params
                    .find_position(param.name.name.as_str())
                    .and_then(|pos| args.find_by_pos(pos))
                    .cloned()
                    .unwrap_or_else(|| Type::GenericParam(param.clone()))
            }

            Type::Record(name) => {
                Type::record(name.as_ref().clone().apply_type_args_by_name(params, args))
            }

            Type::Class(name) => {
                Type::class(name.as_ref().clone().apply_type_args_by_name(params, args))
            }

            Type::Variant(name) => {
                Type::variant(name.as_ref().clone().apply_type_args_by_name(params, args))
            }

            Type::Function(sig) => {
                let sig= sig.apply_ty_args(params, args);
                Type::Function(Rc::new(sig))
            }

            Type::Pointer(deref_ty) => {
                deref_ty
                    .as_ref()
                    .clone()
                    .apply_type_args_by_name(params, args)
                    .ptr()
            }
            
            Type::Array(array_ty) => {
                array_ty
                    .element_ty
                    .clone()
                    .apply_type_args_by_name(params, args)
                    .array(array_ty.dim)
            }
            
            Type::DynArray { element, .. } => {
                element
                    .as_ref()
                    .clone()
                    .apply_type_args_by_name(params, args)
                    .dyn_array()
            }
            
            // non-generic types
            other => other,
        };

        result
    }
}

fn find_in_method_decls<'it, MethodsIter>(
    name: &Ident,
    sig: &FunctionSig,
    methods: MethodsIter
) -> Option<usize>
where
    MethodsIter: Iterator<Item=&'it MethodDecl>
{
    methods
        .enumerate()
        .find_map(|(index, m)| {
            if *m.func_decl.ident() == *name && m.func_decl.sig().eq_as_impl(sig) {
                Some(index)
            } else {
                None
            }
        })
}

pub fn string_type(ctx: &mut Context) -> TypeResult<Type> {
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
