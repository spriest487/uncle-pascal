use {
    crate::ast::prelude::*,
};

pub type ObjectCtor = ast::ObjectCtor<TypeAnnotation>;
pub type ObjectCtorMember = ast::ObjectCtorMember<TypeAnnotation>;
pub type ObjecCtorArgs = ast::ObjectCtorArgs<TypeAnnotation>;

pub fn typecheck_object_ctor(ctor: &ast::ObjectCtor<Span>, ctx: &mut Context) -> TypecheckResult<ObjectCtor> {
    let ty_ident = ast::TypeName::Ident {
        ident: ctor.ident.clone(),
        indirection: 0
    };

    let ty = ctx.find_type(&ty_ident)?.clone();

    let ty_members: Vec<_> = ty.members().cloned().collect();
    let mut members = Vec::new();

    for (member_ty, ctor_member) in ty_members.iter().zip(ctor.args.members.iter()) {
        let value = typecheck_expr(&ctor_member.value, member_ty, ctx)?;

        match ty.find_member(&ctor_member.ident) {
            None => {
                return Err(TypecheckError::MemberNotFound {
                    base: ty,
                    member: ctor_member.ident.clone(),
                    span: ctor_member.ident.span.clone(),
                });
            },

            Some(wrong_ty) if *wrong_ty != value.annotation.ty => {
                return Err(TypecheckError::TypeMismatch {
                    actual: value.annotation.ty,
                    expected: wrong_ty.clone(),
                    span: ctor_member.value.annotation.span().clone(),
                });
            }

            Some(_) => {
                members.push(ObjectCtorMember {
                    ident: ctor_member.ident.clone(),
                    value,
                })
            }
        }
    }

    if members.len() != ty.members_len() {
        let actual = members.into_iter()
            .map(|m| m.value.annotation.ty)
            .collect();
        return Err(TypecheckError::InvalidArgs {
            span: ctor.annotation.clone(),
            expected: ty_members,
            actual,
        });
    }

    let args = ObjecCtorArgs {
        open: ctor.args.open.clone(),
        close: ctor.args.close.clone(),
        members,
    };

    let span = ctor.annotation.clone();
    let annotation = TypeAnnotation::typed_value(ty, ValueKind::Temporary, span);

    Ok(ObjectCtor {
        ident: ctor.ident.clone(),
        args,
        annotation,
    })
}