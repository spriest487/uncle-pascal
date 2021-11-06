use pas_common::span::Spanned;
use std::fmt;

use crate::{typecheck_type, Context, Type, TypecheckResult};
use pas_syn::{ast, Ident};

pub type TypeParam = ast::TypeParam<Type>;

pub type TypeList = ast::TypeList<Type>;
pub type TypeParamList = ast::TypeList<TypeParam>;

pub fn typecheck_type_params(
    type_params: &ast::TypeList<ast::TypeParam<ast::TypeName>>,
    ctx: &mut Context,
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
            constraint,
        });
    }

    Ok(TypeParamList::new(items, type_params.span().clone()))
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
