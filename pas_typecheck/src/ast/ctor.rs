use {
    crate::ast::prelude::*,
};

pub type ObjectCtor = ast::ObjectCtor<TypeAnnotation>;
pub type ObjectCtorMember = ast::ObjectCtorMember<TypeAnnotation>;
pub type ObjecCtorArgs = ast::ObjectCtorArgs<TypeAnnotation>;

pub fn typecheck_object_ctor(ctor: &ast::ObjectCtor<Span>, ctx: &mut Context) -> TypecheckResult<ObjectCtor> {
    let ty_ident = ast::TypeName::Ident(ctor.ident.clone());
    let ty = ctx.find_type(&ty_ident)?.clone();

    let mut members = Vec::new();
    for member in &ctor.args.members {
        let value = typecheck_expr(&member.value, ctx)?;

        match ty.find_member(&member.ident) {
            None => {
                return Err(TypecheckError::MemberNotFound {
                    base: ty,
                    member: member.ident.clone(),
                    span: member.ident.span.clone(),
                });
            },

            Some(wrong_ty) if *wrong_ty != value.annotation.ty => {
                return Err(TypecheckError::TypeMismatch {
                    actual: value.annotation.ty,
                    expected: wrong_ty.clone(),
                    span: member.value.annotation.span().clone(),
                });
            }

            Some(_) => {
                members.push(ObjectCtorMember {
                    ident: member.ident.clone(),
                    value,
                })
            }
        }
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