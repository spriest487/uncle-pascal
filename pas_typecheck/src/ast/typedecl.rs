use {
    crate::ast::prelude::*,
};

pub type TypeDecl = ast::TypeDecl<TypeAnnotation>;
pub type Class = ast::Class<TypeAnnotation>;
pub type Member = ast::Member<TypeAnnotation>;
pub type Interface = ast::Interface<TypeAnnotation>;

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
        ast::TypeDecl::Interface(iface) => {
            let iface = typecheck_iface(iface, ctx)?;
            Ok(ast::TypeDecl::Interface(iface))
        }
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
        kind: class.kind,
        ident: class.ident.clone(),
        span: class.span.clone(),
        members,
    })
}

pub fn typecheck_iface(
    iface: &ast::Interface<Span>,
    ctx: &mut Context)
    -> TypecheckResult<Interface>
{
    let mut methods: Vec<FunctionDecl> = Vec::new();
    for method in &iface.methods {
        if let Some(existing) = methods.iter().find(|other| other.ident == method.ident) {
            return Err(TypecheckError::ScopeError(NameError::AlreadyDefined {
                ident: method.ident.clone(),
                existing: existing.span().clone(),
            }));
        }

        let method = typecheck_func_decl(method, ctx)?;
        methods.push(method);
    }

    Ok(Interface {
        ident: iface.ident.clone(),
        span: iface.span.clone(),
        methods,
    })
}