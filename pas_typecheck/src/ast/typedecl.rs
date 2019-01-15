use {
    crate::ast::prelude::*,
};

pub type TypeDecl = ast::TypeDecl<TypeAnnotation>;
pub type Class = ast::Class<TypeAnnotation>;
pub type Member = ast::Member<TypeAnnotation>;

pub fn typecheck_type_decl(
    type_decl: &ast::TypeDecl<Span>,
    ctx: &mut Context)
    -> TypecheckResult<TypeDecl>
{
    match type_decl {
        ast::TypeDecl::Class(class) => {
            let class = typecheck_class(class, ctx)?;
            Ok(ast::TypeDecl::Class(class))
        },
    }
}

pub fn typecheck_class(
    class: &ast::Class<Span>,
    ctx: &mut Context)
    -> TypecheckResult<Class>
{
    let mut members = Vec::new();
    for member in &class.members {
        let ty = ctx.find_type(&member.ty)?.clone();
        members.push(Member {
            ty,
            span: member.span.clone(),
            ident: member.ident.clone(),
        });
    }

    Ok(Class {
        ident: class.ident.clone(),
        span: class.span.clone(),
        members,
    })
}
