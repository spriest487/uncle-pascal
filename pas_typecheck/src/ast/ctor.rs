use crate::ast::cast::check_implicit_conversion;
use crate::ast::prelude::*;

pub type ObjectCtor = ast::ObjectCtor<TypeAnnotation>;
pub type ObjectCtorMember = ast::ObjectCtorMember<TypeAnnotation>;
pub type ObjectCtorArgs = ast::ObjectCtorArgs<TypeAnnotation>;

pub fn typecheck_object_ctor(
    ctor: &ast::ObjectCtor<Span>,
    span: Span,
    expect_ty: &Type,
    ctx: &mut Context,
) -> TypecheckResult<ObjectCtor> {
    let (_, raw_ty) = ctx.find_type(&ctor.ident)?;

    let ty_name = raw_ty
        .full_path()
        .ok_or_else(|| TypecheckError::InvalidCtorType {
            ty: raw_ty.clone(),
            span: span.clone(),
        })?;

    // generic types can't be constructed, but if the type hint is a parameterized instance of
    // the generic type the constructor expression refers to, use that instead
    let ty = raw_ty
        .infer_specialized_from_hint(expect_ty)
        .ok_or_else(|| GenericError::CannotInferArgs {
            target: GenericTarget::Name(ty_name.clone()),
            span: span.clone(),
            hint: GenericTypeHint::ExpectedValueType(expect_ty.clone()),
        })?
        .clone();

    if ty.is_unspecialized_generic() {
        return Err(GenericError::CannotInferArgs {
            target: GenericTarget::Name(ctor.ident.clone()),
            hint: GenericTypeHint::ExpectedValueType(expect_ty.clone()),
            span,
        }
        .into());
    }

    if !ctx.is_accessible(&ty_name) {
        return Err(TypecheckError::Private {
            name: ty_name,
            span,
        });
    }

    if !ctx.is_constructor_accessible(&ty) {
        return Err(TypecheckError::PrivateConstructor { ty, span });
    }

    let ty_members: Vec<_> = ty.members(ctx)?;
    let mut members: Vec<ObjectCtorMember> = Vec::new();

    for arg in &ctor.args.members {
        if let Some(prev) = members.iter().find(|a| a.ident == arg.ident) {
            return Err(TypecheckError::DuplicateNamedArg {
                name: arg.ident.clone(),
                span: arg.span().clone(),
                previous: prev.span().clone(),
            });
        }

        let member = match ty_members.iter().find(|m| m.ident == arg.ident) {
            Some(member) => member,
            None => {
                return Err(NameError::MemberNotFound {
                    base: NameContainer::Type(ty),
                    member: arg.ident.clone(),
                    span: arg.span.clone(),
                }
                .into())
            }
        };

        let value = typecheck_expr(&arg.value, &member.ty, ctx)?;
        let value_ty = value.annotation().ty();

        check_implicit_conversion(&member.ty, &value_ty, value.span(), ctx)?;

        members.push(ObjectCtorMember {
            ident: arg.ident.clone(),
            value,
            span: arg.span.clone(),
        });
    }

    if members.len() != ty.members_len(ctx)? {
        let actual = members
            .into_iter()
            .map(|m| m.value.annotation().ty().into_owned())
            .collect();
        return Err(TypecheckError::InvalidArgs {
            span: ctor.annotation.clone(),
            expected: ty_members.into_iter().map(|m| m.ty).collect(),
            actual,
        });
    }

    let args = ObjectCtorArgs {
        open: ctor.args.open.clone(),
        close: ctor.args.close.clone(),
        members,
    };

    let annotation = TypedValueAnnotation {
        ty,
        value_kind: ValueKind::Temporary,
        span,
        decl: None,
    }.into();

    Ok(ObjectCtor {
        ident: ty_name,
        args,
        annotation,
    })
}

pub type CollectionCtor = ast::CollectionCtor<TypeAnnotation>;

pub fn typecheck_collection_ctor(
    ctor: &ast::CollectionCtor<Span>,
    expect_ty: &Type,
    ctx: &mut Context,
) -> TypecheckResult<CollectionCtor> {
    let (elements, elem_ty) = match expect_ty.index_element_ty() {
        None => {
            let elements = elements_for_inferred_ty(ctor, ctx)?;
            let elem_ty = elements[0].annotation().ty().into_owned();
            (elements, elem_ty)
        }

        Some(elem_ty) => {
            if false && elem_ty.contains_generic_params() {
                let elements = elements_for_inferred_ty(ctor, ctx)?;
                let elem_ty = elements[0].annotation().ty().into_owned();
                (elements, elem_ty)
            } else {
                let elements = elements_for_expected_ty(ctor, elem_ty, ctx)?;
                (elements, elem_ty.clone())
            }
        }
    };

    let collection_ty = match expect_ty {
        Type::DynArray { .. } => Type::DynArray {
            element: Box::new(elem_ty),
        },

        _ => Type::Array(Rc::new(ArrayType {
            element_ty: elem_ty,
            dim: elements.len(),
        })),
    };

    let annotation = TypedValueAnnotation {
        ty: collection_ty,
        span: ctor.annotation.clone(),
        value_kind: ValueKind::Temporary,
        decl: None,
    }.into();

    Ok(CollectionCtor {
        elements,
        annotation,
    })
}

fn elements_for_inferred_ty(
    ctor: &ast::CollectionCtor<Span>,
    ctx: &mut Context,
) -> TypecheckResult<Vec<Expression>> {
    // must have at at least one element to infer types
    if ctor.elements.is_empty() {
        return Err(TypecheckError::UnableToInferType {
            expr: Box::new(ast::Expression::from(ctor.clone())),
        });
    }

    let mut elements = Vec::new();
    let first_element = typecheck_expr(&ctor.elements[0], &Type::Nothing, ctx)?;
    let expected_ty = first_element.annotation().ty().into_owned();

    elements.push(first_element);

    for e in ctor.elements.iter().skip(1) {
        let element = typecheck_expr(e, &expected_ty, ctx)?;
        element.annotation().expect_value(&expected_ty)?;

        elements.push(element);
    }

    Ok(elements)
}

fn elements_for_expected_ty(
    ctor: &ast::CollectionCtor<Span>,
    expected_ty: &Type,
    ctx: &mut Context,
) -> TypecheckResult<Vec<Expression>> {
    let mut elements = Vec::new();
    for e in &ctor.elements {
        let element = typecheck_expr(e, expected_ty, ctx)?;
        element.annotation().expect_value(&expected_ty)?;

        elements.push(element);
    }

    Ok(elements)
}
