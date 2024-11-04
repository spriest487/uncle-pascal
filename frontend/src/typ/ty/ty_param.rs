use crate::ast;
use crate::ast::{Ident, TypeConstraint};
use crate::typ::{Context, GenericError, GenericResult, Specializable};
use crate::typ::Type;
use crate::typ::TypeResult;
use crate::typ::Typed;
use crate::typ::typecheck_type;
use common::span::Spanned;
use std::borrow::Cow;
use std::fmt;
use std::rc::Rc;

pub type TypeParam = ast::TypeParam<Type>;

impl TypeParam {
    pub fn into_generic_param_ty(self, pos: usize) -> Type {
        match self.constraint {
            Some(constraint) => {
                Type::generic_constrained_param(self.name, pos, constraint.is_ty)
            },
            None => {
                Type::generic_param(self.name, pos)
            },
        }
    }
}

pub type TypeArgList = ast::TypeArgList<Typed>;

impl TypeArgList {
    pub fn apply_type_args_by_name(self, params: &impl TypeParamContainer, args: &impl TypeArgResolver) -> Self {
        self.map(|arg, _pos | arg.apply_type_args_by_name(params, args))
    }
}

pub type TypeParamList = ast::TypeList<TypeParam>;

impl TypeParamList {
    pub fn apply_type_args_by_name(self, params: &impl TypeParamContainer, args: &impl TypeArgResolver) -> Self {
        self
            .map(|ty_param, _pos| {
                let constraint = ty_param.constraint
                    .map(|constraint| TypeConstraint {
                        is_ty: constraint.is_ty.apply_type_args_by_name(params, args),
                        ..constraint
                    });

                TypeParam {
                    name: ty_param.name,
                    constraint,
                }
            })
    }
    
    pub fn to_type_args(self) -> TypeArgList {
        self.map(|item, pos| Type::GenericParam(Rc::new(TypeParamType {
            name: item.name,
            pos,
            is_ty: item
                .constraint
                .map(|constraint| constraint.is_ty)
                .unwrap_or(Type::Nothing)
        })))
    }
}

pub fn typecheck_type_params(
    type_params: &ast::TypeList<ast::TypeParam<ast::TypeName>>,
    ctx: &mut Context,
) -> TypeResult<TypeParamList> {
    let mut items = Vec::new();

    for ty_param in &type_params.items {
        let constraint = match &ty_param.constraint {
            Some(constraint) => {
                let is_ty = typecheck_type(&constraint.is_ty, ctx)?;
                Some(ast::TypeConstraint {
                    name: ty_param.name.clone(),
                    span: constraint.span.clone(),
                    is_ty,
                })
            }
            None => None,
        };

        items.push(ast::TypeParam {
            name: ty_param.name.clone(),
            constraint,
        });
    }

    Ok(TypeParamList::new(items, type_params.span().clone()))
}

pub fn validate_generic_constraints(args: &TypeArgList, params: &TypeParamList, ctx: &Context) -> GenericResult<()> {
    for pos in 0..params.len() {
        if let Some(constraint_ty) = &params[pos].constraint {
            let ty_arg = &args.items[pos];
            if !ty_arg.match_constraint(&constraint_ty.is_ty, ctx) {
                return Err(GenericError::ConstraintNotSatisfied {
                    is_not_ty: constraint_ty.is_ty.clone(),
                    actual_ty: Some(ty_arg.clone()),
                });
            }
        }
    }
    
    Ok(())
}

#[derive(Eq, PartialEq, Hash, Clone, Debug)]
pub struct TypeParamType {
    pub name: Ident,
    pub pos: usize,
    pub is_ty: Type,
}

impl fmt::Display for TypeParamType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

#[derive(Debug, PartialEq)]
pub enum TypeArgsResult<'a> {
    Specialized(&'a TypeParamList, &'a TypeArgList),
    Unspecialized(&'a TypeParamList),
    NotGeneric,
}

pub trait TypeArgResolver {
    fn resolve(&self, param: &TypeParamType) -> Cow<Type>;

    fn find_by_pos(&self, pos: usize) -> Option<&Type>;

    fn len(&self) -> usize;
}

impl TypeArgResolver for TypeArgList {
    fn resolve(&self, param: &TypeParamType) -> Cow<Type> {
        let arg = self
            .find_by_pos(param.pos)
            .expect("resolving type param out of range");
        Cow::Borrowed(arg)
    }

    fn find_by_pos(&self, pos: usize) -> Option<&Type> {
        self.items.get(pos)
    }

    fn len(&self) -> usize {
        self.items.len()
    }
}

pub trait TypeParamContainer {
    fn find_position(&self, name: &str) -> Option<usize>;
    fn len(&self) -> usize;
}

impl TypeParamContainer for TypeParamList {
    fn find_position(&self, name: &str) -> Option<usize> {
        self.items
            .iter()
            .position(|param| param.name.name.as_str() == name)
    }
    
    fn len(&self) -> usize {
        self.items.len()
    }
}
