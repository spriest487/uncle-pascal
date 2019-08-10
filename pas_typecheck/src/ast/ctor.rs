use crate::ast::prelude::*;
use pas_syn::Operator;

pub type ObjectCtor = ast::ObjectCtor<TypeAnnotation>;
pub type ObjectCtorMember = ast::ObjectCtorMember<TypeAnnotation>;
pub type ObjectCtorArgs = ast::ObjectCtorArgs<TypeAnnotation>;

pub fn typecheck_object_ctor(
    ctor: &ast::ObjectCtor<Span>,
    span: Span,
    expect_ty: &Type,
    ctx: &mut Context,
) -> TypecheckResult<ObjectCtor> {
    let raw_ty = find_type(&ctor.ident, ctx)?;

    // generic types can't be constructed, but if the type hint is a parameterized instance of
    // the generic type the constructor expression refers to, use that instead
    let ty = raw_ty
        .infer_specialized_from_hint(expect_ty)
        .ok_or_else(|| TypecheckError::InvalidCtorType {
            ty: raw_ty.clone(),
            span: span.clone(),
        })?
        .clone();

    let ty_name = ty
        .full_path()
        .ok_or_else(|| TypecheckError::InvalidCtorType {
            ty: ty.clone(),
            span: span.clone(),
        })?;

    if !ctx.is_accessible(&ty_name) {
        return Err(TypecheckError::Private {
            name: ty_name,
            span,
        });
    }

    if !ctx.is_constructor_accessible(&ty) {
        return Err(TypecheckError::PrivateConstructor { ty, span });
    }

    let ty_members: Vec<_> = ty.members().map(|m| m.ty).cloned().collect();
    let mut members = Vec::new();

    for (member_ty, ctor_member) in ty_members.iter().zip(ctor.args.members.iter()) {
        let value = typecheck_expr(&ctor_member.value, member_ty, ctx)?;

        match ty.find_member(&ctor_member.ident) {
            None => {
                return Err(NameError::MemberNotFound {
                    base: ty,
                    member: ctor_member.ident.clone(),
                    span: ctor_member.ident.span.clone(),
                }
                .into());
            }

            Some(member_ref) if !member_ref.ty.assignable_from(value.annotation().ty(), ctx) => {
                return Err(TypecheckError::InvalidBinOp {
                    rhs: value.annotation().ty().clone(),
                    lhs: member_ref.ty.clone(),
                    op: Operator::Assignment,
                    span: ctor_member.value.annotation().span().clone(),
                });
            }

            Some(_) => members.push(ObjectCtorMember {
                ident: ctor_member.ident.clone(),
                value,
            }),
        }
    }

    if members.len() != ty.members_len() {
        let actual = members
            .into_iter()
            .map(|m| m.value.annotation().ty().clone())
            .collect();
        return Err(TypecheckError::InvalidArgs {
            span: ctor.annotation.clone(),
            expected: ty_members,
            actual,
        });
    }

    let args = ObjectCtorArgs {
        open: ctor.args.open.clone(),
        close: ctor.args.close.clone(),
        members,
    };

    let annotation = TypeAnnotation::TypedValue {
        ty,
        value_kind: ValueKind::Temporary,
        span,
        decl: None,
    };

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
    let mut elements = Vec::new();

    let elem_ty = match expect_ty.collection_element_ty() {
        None => {
            // must have at at least one element to infer types
            if ctor.elements.is_empty() {
                return Err(TypecheckError::UnableToInferType {
                    expr: Box::new(ast::Expression::from(ctor.clone())),
                });
            }

            let first_element = typecheck_expr(&ctor.elements[0], &Type::Nothing, ctx)?;

            let elem_ty = first_element.annotation().ty().clone();
            elements.push(first_element);

            for e in ctor.elements.iter().skip(1) {
                let element = typecheck_expr(e, &elem_ty, ctx)?;
                element.annotation().expect_value(&elem_ty)?;

                elements.push(element);
            }

            elem_ty
        }

        Some(elem_ty) => {
            for e in &ctor.elements {
                let element = typecheck_expr(e, elem_ty, ctx)?;
                element.annotation().expect_value(&elem_ty)?;

                elements.push(element);
            }

            elem_ty.clone()
        }
    };

    let collection_ty = match expect_ty {
        Type::DynArray { .. } => {
            Type::DynArray { element: Box::new(elem_ty) }
        }

        _ => Type::Array {
            element: Box::new(elem_ty),
            dim: elements.len(),
        },
    };

    let annotation = TypeAnnotation::TypedValue {
        ty: collection_ty,
        span: ctor.annotation.clone(),
        value_kind: ValueKind::Temporary,
        decl: None,
    };

    Ok(CollectionCtor {
        elements,
        annotation,
    })
}
