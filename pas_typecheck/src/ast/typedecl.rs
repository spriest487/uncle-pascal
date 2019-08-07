use crate::ast::prelude::*;
use pas_syn::Ident;

pub type TypeDecl = ast::TypeDecl<TypeAnnotation>;
pub type Class = ast::Class<TypeAnnotation>;
pub type Member = ast::Member<TypeAnnotation>;
pub type Interface = ast::Interface<TypeAnnotation>;
pub type Variant = ast::Variant<TypeAnnotation>;

pub fn typecheck_type_decl(
    name: QualifiedDeclName,
    type_decl: &ast::TypeDecl<Span>,
    ctx: &mut Context,
) -> TypecheckResult<TypeDecl> {
    match type_decl {
        ast::TypeDecl::Class(class) => {
            let class = typecheck_class(name, class, ctx)?;
            Ok(ast::TypeDecl::Class(class))
        },
        ast::TypeDecl::Interface(iface) => {
            let iface = typecheck_iface(name, iface, ctx)?;
            Ok(ast::TypeDecl::Interface(iface))
        },

        ast::TypeDecl::Variant(variant) => {
            let variant = typecheck_variant(name, variant, ctx)?;
            Ok(ast::TypeDecl::Variant(variant))
        },
    }
}

pub fn typecheck_class(
    name: QualifiedDeclName,
    class: &ast::Class<Span>,
    ctx: &mut Context,
) -> TypecheckResult<Class> {
    let mut members = Vec::new();
    for member in &class.members {
        let ty = typecheck_type(&member.ty, ctx)?.clone();
        members.push(Member {
            ty,
            span: member.span.clone(),
            ident: member.ident.clone(),
        });
    }

    Ok(Class {
        kind: class.kind,
        name,
        span: class.span.clone(),
        members,
    })
}

pub fn typecheck_iface(
    name: QualifiedDeclName,
    iface: &ast::Interface<Span>,
    ctx: &mut Context,
) -> TypecheckResult<Interface> {
    // declare Self type - type decls are always in their own scope so we don't need to push
    // another one
    let self_ident = Ident::new("Self", iface.span().clone());
    ctx.declare_type(self_ident, Type::MethodSelf, Visibility::Private)?;

    let mut methods: Vec<FunctionDecl> = Vec::new();
    for method in &iface.methods {
        if let Some(existing) = methods.iter().find(|other| other.ident == method.ident) {
            let method_path = name.qualified.clone().child(method.ident.single().clone());

            return Err(TypecheckError::ScopeError(NameError::AlreadyDefined {
                ident: method_path,
                existing: existing.span().clone(),
            }));
        }

        let mut method_decl = typecheck_func_decl(method, ctx)?;
        // todo: better type so we don't have to do this
        // methods don't have qualified names
        method_decl.ident = method.ident.clone();

        methods.push(method_decl);
    }

    Ok(Interface {
        name,
        span: iface.span.clone(),
        methods,
    })
}

pub fn typecheck_variant(
    name: QualifiedDeclName,
    variant: &ast::Variant<Span>,
    ctx: &mut Context,
) -> TypecheckResult<Variant> {
    if variant.cases.is_empty() {
        return Err(TypecheckError::EmptyVariant(Box::new(variant.clone())));
    }

    let mut cases = Vec::with_capacity(variant.cases.len());
    for case in &variant.cases {
        let data_ty = match &case.data_ty {
            Some(data_ty) => Some(typecheck_type(data_ty, ctx)?),
            None => None,
        };

        cases.push(ast::VariantCase {
            ident: case.ident.clone(),
            span: case.span.clone(),
            data_ty,
        });
    }

    Ok(Variant {
        name,
        cases,
        span: variant.span().clone(),
    })
}
