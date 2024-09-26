use crate::ast;
use crate::ast::Expr;
use crate::ast::IdentPath;
use crate::typ::ast::cast::implicit_conversion;
use crate::typ::ast::typecheck_expr;
use crate::typ::ArrayType;
use crate::typ::Context;
use crate::typ::GenericError;
use crate::typ::GenericTarget;
use crate::typ::GenericTypeHint;
use crate::typ::NameContainer;
use crate::typ::NameError;
use crate::typ::Specializable;
use crate::typ::Type;
use crate::typ::TypeError;
use crate::typ::TypeResult;
use crate::typ::Typed;
use crate::typ::TypedValue;
use crate::typ::ValueKind;
use linked_hash_map::LinkedHashMap;
use common::span::{Span, Spanned};
use std::iter;

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
    let ctor_ty = find_ctor_ty(ctor.ident.as_ref(), expect_ty, &span, ctx)?;

    let ty_name = ctor_ty.full_path()
        .ok_or_else(|| TypeError::InvalidCtorType {
            ty: ctor_ty.clone(),
            span: span.clone(),
        })?;

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

    if !ctx.is_accessible(&ty_name) {
        return Err(TypeError::Private {
            name: ty_name,
            span,
        });
    }

    if !ctx.is_constructor_accessible(&ctor_ty) {
        return Err(TypeError::PrivateConstructor { ty: ctor_ty, span });
    }

    let mut expect_members: LinkedHashMap<_, _> = ctor_ty
        .fields(ctx)
        .map_err(|err| TypeError::NameError {
            err,
            span: span.clone(),
        })?
        .into_iter()
        .map(|member| (member.ident, member.ty))
        .collect();

    let mut members: Vec<ObjectCtorMember> = Vec::new();

    for arg in &ctor.args.members {
        if let Some(prev) = members.iter().find(|a| a.ident == arg.ident) {
            // ctor has duplicate named arguments
            return Err(TypeError::DuplicateNamedArg {
                name: arg.ident.clone(),
                span: arg.span().clone(),
                previous: prev.span().clone(),
            });
        }

        let member_ty = match expect_members.remove(&arg.ident) {
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

        let value = implicit_conversion(
            typecheck_expr(&arg.value, &member_ty, ctx)?,
            &member_ty,
            ctx,
        )?;

        members.push(ObjectCtorMember {
            ident: arg.ident.clone(),
            value,
            span: arg.span.clone(),
        });
    }

    // any remaining members must have valid default values
    let mut missing_members = Vec::new();
    for (member_ident, member_ty) in expect_members {
        match member_ty.default_val() {
            Some(default_lit) => {
                let value = Expr::Literal(default_lit, TypedValue {
                    decl: None,
                    span: ctor.annotation.clone(),
                    ty: member_ty,
                    value_kind: ValueKind::Temporary,
                }.into());

                members.push(ObjectCtorMember {
                    ident: member_ident,
                    span: ctor.annotation.clone(),
                    value,
                });
            }
            None => missing_members.push(member_ident),
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
        members,
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
        annotation,
    })
}

fn find_ctor_ty(explicit_ty_name: Option<&IdentPath>, expect_ty: &Type, span: &Span, ctx: &Context) -> TypeResult<Type>  {
    let ctor_ty = match explicit_ty_name {
        Some(ctor_ident) => {
            let (_, raw_ty) = ctx
                .find_type(&ctor_ident)
                .map_err(|err| TypeError::NameError {
                    err,
                    span: ctor_ident.span().clone(),
                })?;

            let raw_ty_name = raw_ty
                .full_path()
                .ok_or_else(|| TypeError::InvalidCtorType {
                    ty: raw_ty.clone(),
                    span: span.clone(),
                })?;

            // generic types can't be constructed, but if the type hint is a parameterized instance of
            // the generic type the constructor expr refers to, use that instead
            raw_ty
                .infer_specialized_from_hint(expect_ty)
                .ok_or_else(|| {
                    let err = GenericError::CannotInferArgs {
                        target: GenericTarget::Name(raw_ty_name.clone()),
                        hint: GenericTypeHint::ExpectedValueType(expect_ty.clone()),
                    };

                    TypeError::NameError {
                        span: span.clone(),
                        err: NameError::GenericError(err),
                    }
                })?
                .clone()
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
        Some(elem_ty) if !elem_ty.contains_generic_params(ctx) => {
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
            element: Box::new(element_ty),
        },

        // known static array ty_def
        Type::Array(array_ty) => {
            default_fill_elements(array_ty.dim, &element_ty, &mut elements, ctor.annotation.span());
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
    // must have at at least one element to infer types
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

fn default_fill_elements(expect_dim: usize, element_ty: &Type, elements: &mut Vec<CollectionCtorElement>, span: &Span) {
    if expect_dim <= elements.len() {
        return;
    }

    if let Some(default_lit) = element_ty.default_val() {
        let default_count = expect_dim - elements.len();
        let default_val = Expr::Literal(default_lit, TypedValue {
            decl: None,
            span: span.clone(),
            ty: element_ty.clone(),
            value_kind: ValueKind::Temporary,
        }.into());

        let default_elements = iter::repeat(CollectionCtorElement { value: default_val.clone() })
            .take(default_count);

        elements.extend(default_elements);
    }
}
