use crate::ast::prelude::*;
use pas_syn::Ident;

pub type TypeDecl = ast::TypeDecl<TypeAnnotation>;
pub type Class = ast::Class<TypeAnnotation>;
pub type Member = ast::Member<TypeAnnotation>;
pub type Interface = ast::Interface<TypeAnnotation>;
pub type Variant = ast::Variant<TypeAnnotation>;

pub fn typecheck_type_decl(
    type_decl: &ast::TypeDecl<Span>,
    ctx: &mut Context,
) -> TypecheckResult<TypeDecl> {
    match type_decl {
        ast::TypeDecl::Class(class) => {
            let class = typecheck_class(class, ctx)?;
            Ok(ast::TypeDecl::Class(class))
        }
        ast::TypeDecl::Interface(iface) => {
            let iface = typecheck_iface(iface, ctx)?;
            Ok(ast::TypeDecl::Interface(iface))
        }

        ast::TypeDecl::Variant(variant) => {
            let variant = typecheck_variant(variant, ctx)?;
            Ok(ast::TypeDecl::Variant(variant))
        }
    }
}

pub fn typecheck_class(class: &ast::Class<Span>, ctx: &mut Context) -> TypecheckResult<Class> {
    let mut members = Vec::new();
    for member in &class.members {
        let ty = ctx.find_type(&member.ty)?.clone();
        members.push(Member {
            ty,
            span: member.span.clone(),
            ident: member.ident.clone(),
        });
    }

    assert_eq!(
        1,
        class.ident.as_slice().len(),
        "parsed class def name must be unqualified"
    );
    let ident = ctx.qualify_name(class.ident.last().clone());

    Ok(Class {
        kind: class.kind,
        ident,
        span: class.span.clone(),
        members,
    })
}

pub fn typecheck_iface(
    iface: &ast::Interface<Span>,
    ctx: &mut Context,
) -> TypecheckResult<Interface> {
    let iface_scope = ctx.push_scope(None);

    let self_ident = Ident::new("Self", iface.span().clone());
    ctx.declare_type(self_ident, Type::GenericSelf)?;

    let mut methods: Vec<FunctionDecl> = Vec::new();
    for method in &iface.methods {
        if let Some(existing) = methods.iter().find(|other| other.ident == method.ident) {
            let method_path = iface.ident.clone().child(method.ident.clone());

            return Err(TypecheckError::ScopeError(NameError::AlreadyDefined {
                ident: method_path,
                existing: existing.span().clone(),
            }));
        }

        let method = typecheck_func_decl(method, ctx)?;
        methods.push(method);
    }

    ctx.pop_scope(iface_scope);

    assert_eq!(
        1,
        iface.ident.as_slice().len(),
        "parsed class def name must be unqualified"
    );
    let ident = ctx.qualify_name(iface.ident.last().clone());

    Ok(Interface {
        ident,
        span: iface.span.clone(),
        methods,
    })
}

pub fn typecheck_variant(
    variant: &ast::Variant<Span>,
    ctx: &mut Context,
) -> TypecheckResult<Variant> {
    if variant.cases.len() == 0 {
        return Err(TypecheckError::EmptyVariant(Box::new(variant.clone())));
    }

    let mut cases = Vec::with_capacity(variant.cases.len());
    for case in &variant.cases {
        let data_ty = match &case.data_ty {
            Some(data_ty) => Some(ctx.find_type(data_ty)?),
            None => None,
        };

        cases.push(ast::VariantCase {
            ident: case.ident.clone(),
            span: case.span.clone(),
            data_ty,
        });
    }

    Ok(Variant {
        ident: ctx.qualify_name(variant.ident.last().clone()),
        cases,
        span: variant.span().clone(),
    })
}
