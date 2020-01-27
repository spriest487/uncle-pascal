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

    if ty.is_generic() {
        return Err(GenericError::CannotInferArgs {
            target: GenericTarget::Name(ctor.ident.clone()),
            hint: GenericTypeHint::ExpectedValueType(expect_ty.clone()),
            span,
        }.into());
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
            None => return Err(NameError::MemberNotFound {
                base: ty,
                member: arg.ident.clone(),
                span: arg.span.clone(),
            }.into()),
        };

        let value = typecheck_expr(&arg.value, &member.ty, ctx)?;

        if !member.ty.blittable_from(value.annotation().ty(), ctx) {
            return Err(TypecheckError::InvalidBinOp {
                lhs: member.ty.clone(),
                rhs: value.annotation().ty().clone(),
                op: Operator::Assignment,
                span: arg.span().clone(),
            });
        }

        members.push(ObjectCtorMember {
            ident: arg.ident.clone(),
            value,
            span: arg.span.clone(),
        });
    }

    if members.len() != ty.members_len(ctx)? {
        let actual = members
            .into_iter()
            .map(|m| m.value.annotation().ty().clone())
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
        Type::DynArray { .. } => Type::DynArray {
            element: Box::new(elem_ty),
        },

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
