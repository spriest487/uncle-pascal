use crate::ast::prelude::*;
use pas_syn::Operator;

pub type ObjectCtor = ast::ObjectCtor<TypeAnnotation>;
pub type ObjectCtorMember = ast::ObjectCtorMember<TypeAnnotation>;
pub type ObjectCtorArgs = ast::ObjectCtorArgs<TypeAnnotation>;

pub fn typecheck_object_ctor(
    ctor: &ast::ObjectCtor<Span>,
    ctx: &mut Context,
) -> TypecheckResult<ObjectCtor> {
    let ty_ident = ast::TypeName::Ident {
        ident: ctor.ident.clone(),
        indirection: 0,
    };

    let ty = ctx.find_type(&ty_ident)?.clone();
    let ty_name = ty
        .full_path()
        .ok_or_else(|| TypecheckError::InvalidCtorType {
            ty: ty.clone(),
            span: ctor.annotation.span().clone(),
        })?;

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

            Some(member_ref) if !member_ref.ty.assignable_from(value.annotation().ty()) => {
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

    let span = ctor.annotation.clone();
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
                    expr: ast::Expression::from(ctor.clone()),
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
        Type::Nothing => Type::Array {
            element: Box::new(elem_ty.clone()),
            dim: elements.len(),
        },
        _ => expect_ty.clone(),
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
