use crate::ast;
use crate::ast::Ident;
use crate::typ::{Context, GenericError, GenericResult};
use crate::typ::Type;
use crate::typ::TypeResult;
use crate::typ::Typed;
use crate::typ::typecheck_type;
use common::span::Spanned;
use std::borrow::Cow;
use std::fmt;

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
pub type TypeParamList = ast::TypeList<TypeParam>;

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
                    param_ident: ty_param.name.clone(),
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

pub fn validate_ty_args(args: &TypeArgList, params: &TypeParamList, ctx: &Context) -> GenericResult<()> {
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
    pub is_iface: Option<Box<Type>>,
}

impl fmt::Display for TypeParamType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

#[derive(Debug)]
pub enum TypeArgsResult<'a> {
    Specialized(&'a TypeArgList),
    Unspecialized(&'a ast::TypeList<TypeParam>),
    NotGeneric,
}

pub trait TypeArgsResolver {
    fn resolve(&self, param: &TypeParamType) -> Cow<Type>;

    fn find_by_pos(&self, pos: usize) -> Option<&Type>;

    fn len(&self) -> usize;
}

impl TypeArgsResolver for TypeArgList {
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
