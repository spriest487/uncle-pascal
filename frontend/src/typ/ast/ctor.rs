#[cfg(test)]
mod test;

use crate::ast;
use crate::typ::ast::cast::implicit_conversion;
use crate::typ::ast::create_default_literal;
use crate::typ::ast::typecheck_expr;
use crate::typ::ast::typecheck_type_args;
use crate::typ::ArrayType;
use crate::typ::Context;
use crate::typ::GenericError;
use crate::typ::GenericTarget;
use crate::typ::GenericTypeHint;
use crate::typ::NameContainer;
use crate::typ::NameError;
use crate::typ::Specializable;
use crate::typ::Type;
use crate::typ::TypeArgList;
use crate::typ::TypeError;
use crate::typ::TypeResult;
use crate::typ::Typed;
use crate::typ::TypedValue;
use crate::typ::ValueKind;
use common::span::Span;
use common::span::Spanned;
use linked_hash_map::LinkedHashMap;
use std::iter;
use std::rc::Rc;

pub type ObjectCtor = ast::ObjectCtor<Typed>;
pub type ObjectCtorMember = ast::ObjectCtorMember<Typed>;
pub type ObjectCtorArgs = ast::ObjectCtorArgs<Typed>;
pub type CollectionCtor = ast::CollectionCtor<Typed>;
pub type CollectionCtorElement = ast::CollectionCtorElement<Typed>;

pub fn typecheck_object_ctor(
    ctor: &ast::ObjectCtor<Span>,
    span: Span,
    expect_ty: &Type,
    ctx: &mut Context,
) -> TypeResult<ObjectCtor> {
    let ty_args = match &ctor.ty_args {
        Some(list) => Some(typecheck_type_args(list, ctx)?),
        None => None,
    };
    
    let ctor_ty = find_ctor_ty(ctor, expect_ty, ty_args.as_ref(), &span, ctx)?;

    let ty_name = ctor_ty
        .full_path()
        .ok_or_else(|| TypeError::InvalidCtorType {
            ty: ctor_ty.clone(),
            span: span.clone(),
        })?
        .into_owned();

    if ctor_ty.is_unspecialized_generic() {
        let err = GenericError::CannotInferArgs {
            target: GenericTarget::Name(ty_name.clone()),
            hint: GenericTypeHint::ExpectedValueType(expect_ty.clone()),
        };

        return Err(TypeError::NameError {
            span: span.clone(),
            err: NameError::GenericError(err),
        });
    }

    if !ctx.is_visible(&ty_name) {
        return Err(TypeError::NameNotVisible {
            name: ty_name,
            span,
        });
    }

    let mut expect_fields: LinkedHashMap<_, _> = ctor_ty
        .fields(ctx)
        .map_err(|err| TypeError::NameError {
            err,
            span: span.clone(),
        })?
        .into_iter()
        .map(|member| (member.ident, (member.ty, member.span, member.access)))
        .collect();

    let mut fields: Vec<ObjectCtorMember> = Vec::new();

    for arg in &ctor.args.members {
        // check for duplicate items for the same field
        let find_prev = fields
            .iter()
            .find(|a| a.ident == arg.ident);

        if let Some(prev) = find_prev {
            return Err(TypeError::DuplicateNamedArg {
                name: arg.ident.clone(),
                span: arg.span().clone(),
                previous: prev.span().clone(),
            });
        }

        let (member_ty, _member_span, member_access) = match expect_fields.remove(&arg.ident) {
            Some(member) => member,
            None => {
                // ctor has a named argument which doesn't exist in the type
                let err = NameError::MemberNotFound {
                    base: NameContainer::Type(ctor_ty),
                    member: arg.ident.clone(),
                };
                return Err(TypeError::NameError {
                    span: arg.span().clone(),
                    err,
                });
            },
        };
        
        if ctor_ty.get_current_access(ctx) < member_access {
            return Err(TypeError::TypeMemberInaccessible {
                span: arg.span.clone(),
                access: member_access,
                ty: ctor_ty,
                member: arg.ident.clone(),
            });
        }

        let value = implicit_conversion(
            typecheck_expr(&arg.value, &member_ty, ctx)?,
            &member_ty,
            ctx,
        )?;

        fields.push(ObjectCtorMember {
            ident: arg.ident.clone(),
            value,
            span: arg.span.clone(),
        });
    }

    // any remaining members must have valid default values
    let mut missing_members = Vec::new();
    for (member_ident, (member_ty, member_span, _)) in expect_fields {
        let has_default = member_ty
            .has_default(ctx)
            .map_err(|e| TypeError::from_name_err(e, member_span.clone()))?;

        if has_default {
            fields.push(ObjectCtorMember {
                ident: member_ident,
                span: member_span.clone(),
                value: create_default_literal(member_ty, ctor.annotation.clone()),
            });
        } else {
            missing_members.push(member_ident);
        }
    }

    if !missing_members.is_empty() {
        return Err(TypeError::CtorMissingMembers {
            ctor_ty,
            span: ctor.annotation.clone(),
            members: missing_members,
        });
    }

    let args = ObjectCtorArgs {
        span: ctor.args.span.clone(),
        members: fields,
    };

    let annotation = TypedValue {
        ty: ctor_ty,
        value_kind: ValueKind::Temporary,
        span,
        decl: None,
    }
    .into();

    Ok(ObjectCtor {
        ident: Some(ty_name),
        args,
        ty_args,
        annotation,
    })
}

fn find_ctor_ty(
    ctor: &ast::ObjectCtor,
    expect_ty: &Type,
    ty_args: Option<&TypeArgList>,
    span: &Span,
    ctx: &mut Context
) -> TypeResult<Type>  {
    let ctor_ty = match &ctor.ident {
        Some(ctor_ident) => {
            let (_, generic_ty) = ctx
                .find_type(&ctor_ident)
                .map_err(|err| {
                    TypeError::from_name_err(err, ctor_ident.span().clone())
                })?;

            let raw_ty_name = generic_ty
                .full_path()
                .ok_or_else(|| TypeError::InvalidCtorType {
                    ty: generic_ty.clone(),
                    span: span.clone(),
                })?;
            
            match &ty_args {
                Some(ty_arg_list) => {
                    generic_ty
                        .specialize(*ty_arg_list, ctx)
                        .map_err(|err| {
                            TypeError::from_generic_err(err, span.clone())
                        })?
                        .into_owned()
                }

                None => {
                    // infer the type args from the expected type of the expression
                    generic_ty
                        .infer_specialized_from_hint(expect_ty)
                        .ok_or_else(|| {
                            // eprintln!("specialization failed:\n{:#?}\ninto:\n{:#?}", generic_ty, expect_ty);
                            
                            TypeError::from_generic_err(GenericError::CannotInferArgs {
                                target: GenericTarget::Name(raw_ty_name.into_owned()),
                                hint: GenericTypeHint::ExpectedValueType(expect_ty.clone()),
                            }, span.clone())
                        })?
                        .clone()
                }
            }
        }

        None => {
            expect_ty.clone()
        },
    };

    Ok(ctor_ty)
}

pub fn typecheck_collection_ctor(
    ctor: &ast::CollectionCtor<Span>,
    expect_ty: &Type,
    ctx: &mut Context,
) -> TypeResult<CollectionCtor> {
    let (mut elements, element_ty) = match expect_ty.element_ty() {
        Some(elem_ty) if !elem_ty.contains_unresolved_params(ctx) => {
            let elements = elements_for_expected_ty(ctor, elem_ty, ctx)?;
            (elements, elem_ty.clone())
        },

        _ => {
            let elements = elements_for_inferred_ty(ctor, ctx)?;
            let elem_ty = elements[0].value.annotation().ty().into_owned();
            (elements, elem_ty)
        },
    };

    let collection_ty = match expect_ty {
        // known dyn array ty_def
        Type::DynArray { .. } => Type::DynArray {
            element: Rc::new(element_ty),
        },

        // known static array ty_def
        Type::Array(array_ty) => {            
            default_fill_elements(
                array_ty.dim,
                &element_ty,
                &mut elements,
                ctx,
                ctor.annotation.span()
            )?;

            ArrayType::new(element_ty, elements.len()).into()
        }

        // unknown ty_def - construct a static array of these elements
        _ => ArrayType::new(element_ty, elements.len()).into(),
    };

    let annotation = TypedValue {
        ty: collection_ty,
        span: ctor.annotation.clone(),
        value_kind: ValueKind::Temporary,
        decl: None,
    }
    .into();

    Ok(CollectionCtor {
        elements,
        annotation,
    })
}

fn elements_for_inferred_ty(
    ctor: &ast::CollectionCtor<Span>,
    ctx: &mut Context,
) -> TypeResult<Vec<CollectionCtorElement>> {
    // must have at least one element to infer types
    if ctor.elements.is_empty() {
        return Err(TypeError::UnableToInferType {
            expr: Box::new(ast::Expr::from(ctor.clone())),
        });
    }

    let mut elements = Vec::new();
    let first_element_val = typecheck_expr(&ctor.elements[0].value, &Type::Nothing, ctx)?;
    let expected_ty = first_element_val.annotation().ty().into_owned();

    elements.push(CollectionCtorElement {
        value: first_element_val,
    });

    for e in ctor.elements.iter().skip(1) {
        let element = typecheck_expr(&e.value, &expected_ty, ctx)?;
        element.annotation().expect_value(&expected_ty)?;

        elements.push(CollectionCtorElement { value: element });
    }

    Ok(elements)
}

fn elements_for_expected_ty(
    ctor: &ast::CollectionCtor<Span>,
    expected_ty: &Type,
    ctx: &mut Context,
) -> TypeResult<Vec<CollectionCtorElement>> {
    let mut elements = Vec::new();
    for e in &ctor.elements {
        let value = typecheck_expr(&e.value, expected_ty, ctx)?;
        value.annotation().expect_value(&expected_ty)?;

        elements.push(CollectionCtorElement { value });
    }

    Ok(elements)
}

fn default_fill_elements(
    expect_dim: usize,
    element_ty: &Type,
    elements: &mut Vec<CollectionCtorElement>,
    ctx: &Context,
    span: &Span
) -> TypeResult<()> {
    if expect_dim <= elements.len() {
        return Ok(());
    }

    let has_default = element_ty
        .has_default(ctx)
        .map_err(|e| TypeError::from_name_err(e, span.clone()))?;

    if has_default {
        let default_count = expect_dim - elements.len();
        let default_val = create_default_literal(element_ty.clone(), span.clone());
        
        let default_element = CollectionCtorElement { value: default_val.clone() };

        let default_elements = iter::repeat(default_element)
            .take(default_count);

        elements.extend(default_elements);
    }
    
    Ok(())
}
