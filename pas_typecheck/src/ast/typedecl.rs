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
        }
        ast::TypeDecl::Interface(iface) => {
            let iface = typecheck_iface(name, iface, ctx)?;
            Ok(ast::TypeDecl::Interface(iface))
        }

        ast::TypeDecl::Variant(variant) => {
            let variant = typecheck_variant(name, variant, ctx)?;
            Ok(ast::TypeDecl::Variant(variant))
        }
    }
}

pub fn typecheck_class(
    name: QualifiedDeclName,
    class: &ast::Class<Span>,
    ctx: &mut Context
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
        name: name,
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
    ctx.declare_type(self_ident, Type::GenericSelf)?;

    let mut methods: Vec<FunctionDecl> = Vec::new();
    for method in &iface.methods {
        if let Some(existing) = methods.iter().find(|other| other.ident == method.ident) {
            let method_path = name.qualified.clone().child(method.ident.clone());

            return Err(TypecheckError::ScopeError(NameError::AlreadyDefined {
                ident: method_path,
                existing: existing.span().clone(),
            }));
        }

        let method = typecheck_func_decl(method, ctx)?;
        methods.push(method);
    }

    Ok(Interface {
        name: name,
        span: iface.span.clone(),
        methods,
    })
}

pub fn typecheck_variant(
    name: QualifiedDeclName,
    variant: &ast::Variant<Span>,
    ctx: &mut Context,
) -> TypecheckResult<Variant> {
    if variant.cases.len() == 0 {
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
        name: name,
        cases,
        span: variant.span().clone(),
    })
}

pub fn parameterize_class(class: &Class, args: Vec<Type>, span: &Span) -> TypecheckResult<Class> {
    let expected_args: &[Ident] = &class.name.decl_name.type_params.as_slice();
    if args.len() != expected_args.len() {
        return Err(TypecheckError::WrongTypeArgs {
            ty: Type::Class(class.clone().into()),
            expected: expected_args.len(),
            actual: args.len(),
            span: span.clone(),
        })
    }

    let mut members = Vec::new();
    for member in &class.members {
        match &member.ty {
            Type::GenericParam(ident) => {
                let index = expected_args.iter().position(|arg| *arg == *ident)
                    .expect("member with generic param type must match one of the class's generic params");

                members.push(Member {
                    ident: member.ident.clone(),
                    span: member.span.clone(),
                    ty: args[index].clone(),
                })
            }

            _ => members.push(member.clone()),
        }
    }

    let parameterized_name = QualifiedDeclName {
        type_args: args,
        ..class.name.clone()
    };

    Ok(Class {
        name: parameterized_name,
        members,
        span: class.span.clone(),
        kind: class.kind.clone(),
    })
}