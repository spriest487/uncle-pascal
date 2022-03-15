use crate::ast::prelude::*;

pub type TypeDecl = ast::TypeDecl<TypeAnnotation>;
pub type Composite = ast::Composite<TypeAnnotation>;
pub type Member = ast::CompositeMember<TypeAnnotation>;
pub type Interface = ast::Interface<TypeAnnotation>;
pub type Variant = ast::Variant<TypeAnnotation>;

pub fn typecheck_type_decl(
    name: Symbol,
    type_decl: &ast::TypeDecl<Span>,
    ctx: &mut Context,
) -> TypecheckResult<TypeDecl> {
    match type_decl {
        ast::TypeDecl::Class(class) => {
            let class = typecheck_composite(name, class, ctx)?;
            Ok(ast::TypeDecl::Class(Rc::new(class)))
        }
        ast::TypeDecl::Interface(iface) => {
            let iface = typecheck_iface(name, iface, ctx)?;
            Ok(ast::TypeDecl::Interface(Rc::new(iface)))
        }

        ast::TypeDecl::Variant(variant) => {
            let variant = typecheck_variant(name, variant, ctx)?;
            Ok(ast::TypeDecl::Variant(Rc::new(variant)))
        }
    }
}

pub fn typecheck_composite(
    name: Symbol,
    class: &ast::Composite<Span>,
    ctx: &mut Context,
) -> TypecheckResult<Composite> {
    let self_ty = match class.kind {
        ast::CompositeKind::Record => Type::Record(Box::new(name.clone())),
        ast::CompositeKind::Class => Type::Class(Box::new(name.clone())),
    };
    ctx.declare_self_ty(self_ty.clone(), name.span().clone())?;
    ctx.declare_type(class.name.ident.clone(), self_ty, Visibility::Implementation)?;

    let mut members = Vec::new();
    for member in &class.members {
        let ty = typecheck_type(&member.ty, ctx)?.clone();

        let is_unsized = ctx.is_unsized_ty(&ty)
            .map_err(|err| TypecheckError::from_name_err(err, class.span().clone()))?;

        if is_unsized {
            return Err(TypecheckError::UnsizedMember {
                decl: name.qualified,
                member: member.ident.clone(),
                member_ty: ty,
            })
        }

        members.push(Member {
            ty,
            span: member.span.clone(),
            ident: member.ident.clone(),
        });
    }

    Ok(Composite {
        kind: class.kind,
        name,
        span: class.span.clone(),
        members,
    })
}

pub fn typecheck_iface(
    name: Symbol,
    iface: &ast::Interface<Span>,
    ctx: &mut Context,
) -> TypecheckResult<Interface> {
    // declare Self type - type decls are always in their own scope so we don't need to push
    // another one
    ctx.declare_self_ty(Type::MethodSelf, iface.name.span().clone())?;

    let mut methods: Vec<FunctionDecl> = Vec::new();
    for method in &iface.methods {
        if let Some(existing) = methods.iter().find(|other| other.ident == method.ident) {
            let method_path = name.qualified.clone().child(method.ident.single().clone());

            return Err(TypecheckError::NameError {
                err: NameError::AlreadyDefined {
                    ident: method_path,
                    existing: existing.span().clone(),
                },
                span: method.ident.span().clone(),
            });
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
    name: Symbol,
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
