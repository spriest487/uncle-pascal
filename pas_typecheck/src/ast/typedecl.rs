use crate::{
    ast::{typecheck_func_decl, InterfaceMethodDecl},
    typecheck_type, Context, NameError, Primitive, Symbol, Type, TypeAnnotation, TypecheckError,
    TypecheckResult,
};
use pas_common::span::{Span, Spanned};
use pas_syn::{
    ast,
    ast::{StructKind, Visibility},
};

pub type StructDef = ast::StructDef<TypeAnnotation>;
pub type Member = ast::StructMember<TypeAnnotation>;
pub type InterfaceDecl = ast::InterfaceDecl<TypeAnnotation>;
pub type VariantDef = ast::VariantDef<TypeAnnotation>;
pub type AliasDecl = ast::AliasDecl<TypeAnnotation>;

pub const VARIANT_TAG_TYPE: Type = Type::Primitive(Primitive::Int32);

pub fn typecheck_struct_decl(
    name: Symbol,
    class: &ast::StructDef<Span>,
    ctx: &mut Context,
) -> TypecheckResult<StructDef> {
    let self_ty = match class.kind {
        StructKind::Record | StructKind::PackedRecord => Type::Record(Box::new(name.clone())),
        StructKind::Class => Type::Class(Box::new(name.clone())),
    };
    ctx.declare_self_ty(self_ty.clone(), name.span().clone())?;
    ctx.declare_type(
        class.name.ident.clone(),
        self_ty,
        Visibility::Implementation,
    )?;

    let mut members = Vec::new();
    for member in &class.members {
        let ty = typecheck_type(&member.ty, ctx)?.clone();

        let is_unsized = ctx
            .is_unsized_ty(&ty)
            .map_err(|err| TypecheckError::from_name_err(err, class.span().clone()))?;

        if is_unsized {
            return Err(TypecheckError::UnsizedMember {
                decl: name.qualified,
                member: member.ident.clone(),
                member_ty: ty,
            });
        }

        members.push(Member {
            ty,
            span: member.span.clone(),
            ident: member.ident.clone(),
        });
    }

    Ok(StructDef {
        kind: class.kind,
        name,
        span: class.span.clone(),
        members,
    })
}

pub fn typecheck_iface(
    name: Symbol,
    iface: &ast::InterfaceDecl<Span>,
    ctx: &mut Context,
) -> TypecheckResult<InterfaceDecl> {
    // declare Self type - type decls are always in their own scope so we don't need to push
    // another one
    ctx.declare_self_ty(Type::MethodSelf, iface.name.span().clone())?;

    let mut methods: Vec<InterfaceMethodDecl> = Vec::new();
    for method in &iface.methods {
        if let Some(existing) = methods
            .iter()
            .find(|other| other.decl.ident == method.decl.ident)
        {
            let method_path = name
                .qualified
                .clone()
                .child(method.decl.ident.single().clone());

            return Err(TypecheckError::NameError {
                err: NameError::AlreadyDefined {
                    ident: method_path,
                    existing: existing.span().clone(),
                },
                span: method.decl.ident.span().clone(),
            });
        }

        let mut method_decl = typecheck_func_decl(&method.decl, ctx)?;
        // todo: better type so we don't have to do this
        // methods don't have qualified names
        method_decl.ident = method.decl.ident.clone();

        methods.push(ast::InterfaceMethodDecl { decl: method_decl });
    }

    Ok(InterfaceDecl {
        name,
        span: iface.span.clone(),
        methods,
    })
}

pub fn typecheck_variant(
    name: Symbol,
    variant: &ast::VariantDef<Span>,
    ctx: &mut Context,
) -> TypecheckResult<VariantDef> {
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

    Ok(VariantDef {
        name,
        cases,
        span: variant.span().clone(),
    })
}

pub fn typecheck_alias(
    name: Symbol,
    alias: &ast::AliasDecl<Span>,
    ctx: &mut Context,
) -> TypecheckResult<AliasDecl> {
    let ty = typecheck_type(&alias.ty, ctx)?;

    Ok(AliasDecl {
        name,
        ty: Box::new(ty),
        span: alias.span.clone(),
    })
}
